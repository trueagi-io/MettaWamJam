/*  mwj.pl - HTTP Metta Server
%
%   @author Mike Archbold
%   @title MettaWamJam Server  (Warren Abstract Machine for MeTTa)
%
%   This program implements an HTTP server in SWI-Prolog that provides
%   endpoints for evaluating MeTTa language expressions over HTTP. The
%   server exposes a `/metta` endpoint for evaluating MeTTa strings and
%   a `/stop` endpoint to gracefully shut down the server.
%
%   The Metta server loads a MeTTa transpiler, PeTTa from `src/metta.pl`, handles
%   optional MeTTa file input from the command-line arguments, and initializes
%   the HTTP server on a configurable port (default 5000).
%
%   This server is intended to be called locally only. Do not expose this
%   server to the public internet until proper input sanitization, authentication,
%   and security hardening have been implemented on a front end.
%
%   @usage
%     % Start the server with optional initial metta code / atomspace:
%     $ swipl mwj.pl <metta_file>
%
%     % Evaluate MeTTa expressions via POST example:
%     $ curl -X POST http://localhost:5000/metta -H 'Content-Type: text/plain' --data '!(+ 1 1)'
%
%     % Shut down the server:
%     $ curl -v http://localhost:5000/stop
%     Alternatively, the docker stop command will work.
%
%     If your bash shell seems to hang after exit, just enter 'reset', you might not 
%     see your typing 'reset'.
*/

% Import necessary modules for running an HTTP server, dispatching requests,
% and making HTTP client calls.
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).  
:- use_module(library(http/http_header)).  


% If the user submits a malformed query, this code will prevent a halt of the swipl server
% invoked by PeTTa.
:- redefine_system_predicate(halt/1).
halt(Status) :- format("Blocked halt(~w).~n", [Status]), !.

% Register HTTP handlers for the /metta and /stop routes.
% These handlers will be invoked when HTTP requests are made to the respective paths.
:- http_handler(root(metta), metta, []).		
:- http_handler(root(stop), stop, []).  
:- http_handler(root(reset), reset, []).  

%!  server(+Port) is det.
%
%   Starts the HTTP server on the specified Port.
%   The server will use the dispatch table defined by http_dispatch/1.
%
%   @arg Port The port number the HTTP server should listen on.
%
%   @example
%     ?- server(5000).
%     % Starts the server listening on port 5000.
server(Port) :-						
        http_server(http_dispatch, [port(Port),workers(1)]).  % workers = 1 prevents multi-threading


% --- Load PeTTa Prolog code to handle MeTTa calls ------------- %
:- ensure_loaded('./src/metta.pl').         % <-- LOADS PETTA HERE
% -------------------------------------------------------------- %


%! --- Initialize and check if atomspace/metta is passed for loading, if so, load ---
%
%   Loads a MeTTa file if one is provided as a command-line argument
%   when the Prolog system is started. If no arguments are passed, or
%   if the special atom `mork` is passed, no file will be loaded and
%   execution continues without attempting to load a MeTTa script.
%
%   If a file path is provided as the first argument and the file exists,
%   it attempts to load it via `load_metta_file/2`. The results of the
%   loading are printed to the terminal. If the file does not exist,
%   a message is printed and execution continues normally.
%
%   This is primarily useful for automatically preloading a MeTTa
%   script or atomspace during startup, such as from the command line.
%
%   @example
%     % Start Prolog and load a MeTTa script:
%     $ swipl mwj.pl MyAtomSpace.metta
%
:- 
   % Set working directory
   working_directory(Dir, Dir),               % Variable appears twice but just returns working dir.
   assertz(working_dir(Dir)),
   % assertz(working_dir('/PeTTa/faiss_ffi')),  % Hack for docker to see example faiss
   working_dir(Dir),
   format("~nmwj working directory > ~w~n~n", Dir),!,

   % Check arguments passed...
   current_prolog_flag(argv, Args),
   ( Args = [] ->
        true                                  % No arguments provided → do nothing
   ; Args = [mork] ->
        true                                  % Special no-op argument → do nothing
   ; Args = [File|_] ->                       % First argument is a file path
        (   exists_file(File)
        ->  file_directory_name(File, _),     % Extracts atomspace directory (presently nulled out)
            load_metta_file(File, Results),   % Load MeTTa file / atomspace
            format("~nmwj successfully loaded user atomspace... ~n~n"),
            maplist(swrite, Results, ResultsR),   % Convert results to string representations
            maplist(format("~w~n"), ResultsR)     % Print results
        ;   format("~nmwj no initial atomspace found... ~n~n")   % File not found → skip loading
        )
   ).

%!  metta(+Request) is det.
%
%   HTTP handler for the `/metta` endpoint. Accepts an HTTP POST request
%   containing a MeTTa expression in the body, evaluates it, and returns the result.
%
%   The request body is read as a string and passed to `process_metta_string/2`,
%   which performs the actual MeTTa evaluation.
%
%   @arg Request The HTTP request, automatically supplied by the server.
%
%   @example
%     % POST request:
%     $ curl -X POST http://localhost:5000/metta -H 'Content-Type: text/plain' --data '!(+ 1 1)'
%
metta(Request) :-
        http_read_data(Request, Body, [to(string)]),
        format('Content-type: application/json~n~n'),
        % suppress output other than the result
        with_output_to(string(_),
                process_metta_string(Body, Result)
            ),
        maplist(swrite,Result,ResultsR),   
        write(ResultsR).

%!  stop(+Request) is det.
%
%   HTTP handler for the `/stop` endpoint. Gracefully shuts down the server.
%   Responds with a confirmation message, then spawns a detached thread that
%   stops the HTTP server and halts the Prolog process.
%
%   This design avoids blocking the current HTTP thread while the server
%   is shutting down.
stop(_Request) :-
    format('Content-type: text/plain~n~n'),
    format('Stopping server...~n'),
    flush_output,
    %  Stop server + exit from a *separate* thread
    thread_create(
        ( sleep(0.1),                        
          http_current_server(http_dispatch, Port),
          http_stop_server(Port, []),
          halt                               
        ),
        _,
        [detached(true)]
    ).

% Need means of prolog level reset...
reset(_Request) :-  
    %forall(
    %    ( current_predicate(Name/Arity),
    %      functor(Head, Name, Arity),
    %      predicate_property(Head, dynamic)
    %    ),
    %    retractall(Head)
    %),
    %make,
    format("Content-type: text/plain~n~n"),
    format("Reset complete.~n").

% Start the Prolog server
:- server(5000),
    thread_get_message(_).
