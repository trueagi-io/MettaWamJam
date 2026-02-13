/*  mwj.pl - HTTP Metta Server
%
%   @author Mike Archbold
%   @title MettaWamJam Server  (Warren Abstract Machine for MeTTa)
%
%   This program implements an HTTP server in SWI-Prolog that provides
%   endpoints for evaluating MeTTa language expressions over HTTP. The
%   server exposes a `/metta` endpoint for evaluating MeTTa strings and
%   a `/stop` endpoint to gracefully shut down the server. There is also
%   a `/metta_stateless` endpoint for use without saving state on the server.
%
%   The Metta server loads a MeTTa transpiler, PeTTa from `src/metta.pl`, handles
%   optional MeTTa file input from the command-line arguments, and initializes
%   the HTTP server on a configurable port (default 5001).
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
%     $ curl -X POST http://localhost:5001/metta -H 'Content-Type: text/plain' --data '!(+ 1 1)'
%
%     % Shut down the server:
%     $ curl -v http://localhost:5001/stop
%     Alternatively, the docker stop command will work.
%
%     If your bash shell seems to hang after exit, just enter 'reset', you might not 
%     see your typing 'reset'.
*/

%***************************** CAUTION ***************************************
%   In general, avoid modifying predicates used by the /PeTTa repo. If necessary,
%   check with the PeTTa or associated teams. The only variable to be set in this
%   program is:
%                       working_dir(Value), the working directory
%*****************************************************************************

% ----------------- MWJ configuration -----------------
% Max seconds a spawned child process is allowed to run before SIGKILL (-9)
mwj_child_time_limit(30.0).
% -----------------------------------------------------


% Import necessary modules for running an HTTP server, dispatching requests,
% and making HTTP client calls.
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).  
:- use_module(library(http/http_header)).
:- use_module(library(process)).


% If the user submits a malformed query, this code will prevent a halt of the swipl server
% invoked by PeTTa.
:- redefine_system_predicate(halt/1).
halt(Status) :- format("Blocked halt(~w).~n", [Status]), !.

% Register HTTP handlers for the /metta and /stop routes.
% These handlers will be invoked when HTTP requests are made to the respective paths.
:- http_handler(root(metta), metta, []).		
:- http_handler(root(metta_stateless), metta_stateless, []).  
:- http_handler(root(stop), stop, []).  

%!  server(+Port) is det.
%
%   Starts the HTTP server on the specified Port.
%   The server will use the dispatch table defined by http_dispatch/1.
%
%   @arg Port The port number the HTTP server should listen on.
%
%   @example
%     ?- server(5001).
%     % Starts the server listening on port 5001.
server(Port) :-						
        http_server(http_dispatch, [port(Port),workers(5)]).  % workers = 1 prevents multi-threading


% --- Load PeTTa Prolog code to handle MeTTa calls ------------- %
:- ensure_loaded('metta.pl').         % <-- LOADS PETTA HERE
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
   % Set relative working directory, default to .
   assertz(working_dir(".")),
    
   % Check arguments passed...
   current_prolog_flag(argv, Args),
   ( Args = [] ->
        true                                  % No arguments provided → do nothing
   ; Args = [mork] ->
        true                                  % Special no-op argument → do nothing
   ; Args = [File|_] ->                       % First argument is a file path
        (   exists_file(File)
        ->  file_directory_name(File, Dir),   % Extracts atomspace directory (relative)
            retractall(working_dir(_)),       % Retract default working directory 
            assertz(working_dir(Dir)),        % Reset relative working directory to input file directory
            load_metta_file(File, Results),   % Load MeTTa file / atomspace
            format("~nmwj successfully loaded user atomspace... ~n~n"),
            maplist(swrite, Results, ResultsR),   % Convert results to string representations
            maplist(format("~w~n"), ResultsR)     % Print results
        ;   format("~nmwj no initial atomspace found... ~n~n")   % File not found → skip loading
        )
   ),
   working_dir(WorkingDir_Final),
   format("~nmwj working directory > ~w~n~n", WorkingDir_Final).

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
%     $ curl -X POST http://localhost:5001/metta -H 'Content-Type: text/plain' --data '!(+ 1 1)'
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

%!  metta_stateless(+Request) is det.
%
%   Spawn a new process to handle the request.
%   Child provides the response, then exits.
%
metta_stateless(Request) :-
    http_read_data(Request, Body, [to(string)]),
    
    % Use temp file for large input instead of command line
    tmp_file(input, InputFile),
    tmp_file(output, OutputFile),
    
    % Write input to file
    open(InputFile, write, InStream),
    write(InStream, Body),
    close(InStream),
    
    % Build the goal string
    format(atom(Goal), 'run_stateless_child_from_file("~w", "~w")', [InputFile, OutputFile]),
    
    % Spawn the process with increased stack
    process_create(
        path(swipl),
        ['--stack-limit=2g', '-g', Goal, '-t', 'halt', 'mwj.pl', '--', 'child'],
        [process(PID)]
    ),
    
    mwj_child_time_limit(LimitSeconds),
    catch(
        call_with_time_limit(LimitSeconds, process_wait(PID, _)),
        time_limit_exceeded,
        (
            process_kill(PID, kill),   % SIGKILL (-9)
            process_wait(PID, _)       % reap zombie
        )
        ),

    % Wait for child process to complete
    %process_wait(PID, _Status),
    
    % Read output from file
    (exists_file(OutputFile) ->
        read_file_to_string(OutputFile, Output, []),
        delete_file(OutputFile),
        delete_file(InputFile)
    ;
        Output = '{"error": "MeTTa query failed"}'
    ),
    
    % Return child's response
    format('Content-type: application/json~n~n'),
    write(Output).

%!  run_stateless_child_from_file(+InputFile, +OutputFile) is det.
%
%   Child process reads input from file, writes output to file.
%   Better for large inputs.
%
run_stateless_child_from_file(InputFile, OutputFile) :-
    % Read input
    read_file_to_string(InputFile, Body, []),
    
     % Temporary priming query for debug
    with_output_to(string(_), ( process_metta_string('!(+ 0 0)', _) ) ),

    % Process the MeTTa query
    with_output_to(string(_),
        (   process_metta_string(Body, Result),
            process_metta_string('!(match &self $x $x)', StateDump)
        )
    ),
    
    % Convert results to strings
    maplist(swrite, Result, ResultR),
    maplist(swrite, StateDump, ResultR2),
    
    % Write to output file
    open(OutputFile, write, OutStream),
    format(OutStream, '~w~n~w~n', [ResultR, ResultR2]),
    close(OutStream),
    
    % Child exits
    halt.


%!  stop(+Request) is det.
%
%   The /stop endpoint will clear atomspace and halt this program. 
%   Responds with a confirmation message, then spawns a detached thread that
%   stops the HTTP server and halts the Prolog process.
%
%   This design avoids blocking the current HTTP thread while the server
%   is shutting down.
%
%   ** IF YOU WANT TO USE /stop TO CLEAR AND RESTART ATOMSPACE WITH DOCKER: **
%
%   If running in Docker with `--restart=always` with "docker run...", the container will
%   immediately restart after this call. This is useful if you want to clear
%   atomspace, easily restarting with a refreshed environment. If you use --restart=always
%   for this purpose, follow these steps:
%
%       1) Use "docker stop <container>" instead of /stop to stop the container (no restart)
%       2) Omit -rm from the docker run command line
%       3) Use "docker restart <container>" not "docker run" since omitting -rm will 
%          retain the container.
%
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

% Start the Prolog server only if not running as a child process
:- (current_prolog_flag(argv, Args), memberchk('child', Args) -> 
        true  % Child mode - don't start new server
    ;   
        server(5001), thread_get_message(_)  % Parent mode - start server
   ).
