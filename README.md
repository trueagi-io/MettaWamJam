# MettaWamJam

MWJ is lightweight, blazing fast SWI-Prolog HTTP server for MeTTa. It provides a /metta endpoint for MeTTa execution and /stop for controlled shutdown. MWJ runs the PeTTa transpiler for MeTTa and loads your optional input MeTTa file (atomspace / code). The server listens on a configurable port (default localhost:5000 in commands below). 
<P><P>
MWJ is not intended for public deployment without additional security hardening, so you should craft your front end if needed. You could, for example, create an Apache front end facing the public internet with security authentications, input sanitizing, etc, that calls the MWJ server. There is no security specifically built in to the MWJ server.


<B>If you just want to use the handy default docker image make sure you have docker installed on your machine and then just run these commands (no need to clone repo):</B>

<hr>
<B>1. docker pull jazzbox35/mwj</B>
<hr>
<B>2. docker run --rm -d --name mwj -p 127.0.0.1:5000:5000 --tty jazzbox35/mwj:latest</B>
<br>
OR WITH YOUR ATOMSPACE
<br>
<B>docker run --rm -d --name mwj -p 127.0.0.1:5000:5000 --tty -v ATOMSPACE:/PeTTa/atomspace.metta jazzbox35/mwj:latest</B>
<P><P>
To pass an input atomspace (any '.metta' file) replace the all-caps ATOMSPACE above with the full path to your .metta file. Example:  /home/user/my_file.metta:/PeTTa/atomspace.metta  (no quotes)

Note: the order of ports is host:container; so if you want your machine to invoke the server (ie., call docker) using your machine's host port 80 you would use 127.0.0.1:80:5000. The localhost port 80 is redirected to docker's port 5000 in this scenario.

<hr>
3. 
<B>curl -X POST http&#58;//localhost:5000/metta -H "Content-Type: text/plain" --data '!(+ 1 2)'</B>
<P><P>
The above command will return a result of [3] if the installation worked.
For curl commands, always put single quotes (') around your metta query.

<hr>
4. <B>curl -X POST http&#58;//localhost:5000/stop</B>
<P><P>
The above will shut down the server and docker container gracefully when done. Alternatively, you can issue this command to shut down the server: <B>docker stop mwj</B>

<P><P>

If you wish to modify the server, then clone the repo, modify as desired for your installation, and rebuild the docker image using Dockerfile. 

"WAM" is an acronym for Warren Abstract Machine.


