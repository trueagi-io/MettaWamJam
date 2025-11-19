# MeTTaWamJam

MWJ is lightweight, blazing fast SWI-Prolog HTTP server for MeTTa. It provides a /metta endpoint for MeTTa execution and /stop for controlled shutdown. MWJ loads the PeTTa transpiler and your optional input MeTTa file (atomspace / code), running on localhost using a configurable port (default 5000). MWJ is not intended for public deployment without additional security hardening, so you should craft your front end if needed. You could, for example, create an Apache front end facing the public internet with security authentications, input sanitizing, etc, that calls the MWJ server. There is no security specifically built in to the MWJ server.

If you just want to use the handy default docker image, you need not clone this repo but simply follow these easy command steps to get started:

1) docker pull jazzbox35/mwj

2) docker run --rm -it -p 5000:5000 jazzbox35/mwj:latest

Note: the order of ports is host:container; so if you want your machine to call docker using port 80 you would use 80:5000. Once the server starts in a docker container, it will display the ?- Prolog prompt. Just ignore this window until you issue the stop command (below).

3) *(Open a new terminal command line) 
curl -X POST http://localhost:5000/metta -H "Content-Type: text/plain" --data '!(+ 1 2)'
 
The above command will return a result of [3] if the installation worked.

4) curl -X POST http://localhost:5000/stop 

The above will shut down the server and docker container gracefully when done (again run from your machine outside docker).


If you wish to modify the server, then clone the repo, modify as desired for your installation, and rebuild the Dockerfile. 