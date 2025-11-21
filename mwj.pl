/*  mwj.pl - HTTP Metta Server
%
%   @author Mike Archbold
%   @version 0.1
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
%   This server is intended to be run locally only. Do not expose this
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
:- use_module(library(json)).  


% If the user submits a malformed query, this code will prevent a halt of the swipl server
% invoked by PeTTa.
:- redefine_system_predicate(halt/1).
halt(Status) :- format("Blocked halt(~w).~n", [Status]), !.

% Register HTTP handlers for the /metta and /stop routes.
% These handlers will be invoked when HTTP requests are made to the respective paths.
:- http_handler(root(metta), metta, []).		
:- http_handler(root(stop), stop, []).  

%!  server(+Port) is det.
%
%   Starts the HTTP server on the specified Port.
%   The server will use the dispatch table defined by http_dispatch/1.
%
%   @arg Port The port number the HTTP server should listen on.
%
%   @example
%     ?- server(5000).
%     % Starts the server on localhost:5000.
server(Port) :-						
        http_server(http_dispatch, [port(Port)]).


% --- Load PeTTa Prolog code to handle MeTTa calls ------------- %
:- ensure_loaded('./src/metta.pl').     % <-- LOADS PETTA HERE
% -------------------------------------------------------------- %


% --- Check if atomspace/metta is passed for loading, if so, load

%!  Command-line MeTTa file loader.
%
%   Loads a MeTTa file if one is provided as a command-line argument.
%   If no arguments are passed or the special atom `mork` is used,
%   this step is skipped. Also will continue if it can't find the 
%   input MeTTa code / atomspace.
%
%   Useful for preloading a MeTTa script or knowledge base on startup.
%
%   @example
%     $ swipl mwj.pl  MyAtomSpace.metta
%
%   @note The use of `mork` as a no-op flag is for compatibility in case used
%
:- current_prolog_flag(argv, Args),
   ( Args = [] ->
        true                                  % do nothing
   ; Args = [mork] ->
        true                                  % do nothing
   ; Args = [File|_] ->                       % if atomspace passed
        (   exists_file(File)
        ->  file_directory_name(File, Dir),
            format("~nmwj successfully loaded user atomspace... ~n~n"),
            assertz(working_dir(Dir)),
            load_metta_file(File, Results),
            maplist(swrite, Results, ResultsR),
            maplist(format("~w~n"), ResultsR)
        ;   format("~nmwj no initial atomspace found... ~n~n")   % file not found → silently skip
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
    % 2. Stop server + exit from a *separate* thread
    thread_create(
        ( sleep(0.1),                        
          http_current_server(http_dispatch, Port),
          http_stop_server(Port, []),
          halt                               
        ),
        _,
        [detached(true)]
    ).

% Start the Prolog server
:- server(5000).
