# MettaWamJam

MWJ is a lightweight, blazing-fast SWI-Prolog HTTP server for MeTTa. It provides a `/metta` endpoint for MeTTa execution and `/stop` for controlled shutdown. MWJ runs the PeTTa transpiler for MeTTa and loads an atomspace (if provided). You can also run MM2 programs on top of MORK using this server. Please see the example Python programs in this repo for running your MeTTa and MM2 programs.

MWJ is not intended for public deployment without additional security hardening, so you should craft your front end, firewall, etc as needed. You could, for example, create an Apache front end facing the public internet and restrict access to the MWJ server.

<B>If you just want to use the handy default Docker image make sure you have Docker installed on your machine and then run the commands below. There is no need to clone the repo. The default settings will limit access to the MWJ server to your machine (ie., localhost on 127.0.0.1:5000).</B>

<B>1. Pull, run the container, start your own server  (default)</B>

amd64:

```bash
docker run --pull always --rm -d --name mwj -p 127.0.0.1:5000:5000 jazzbox35/mwj:latest
```

OR with your atomspace (mount a .metta file into the container)

amd64:

```bash
docker run --pull always --rm -d --name mwj -p 127.0.0.1:5000:5000  -v /full/path/to/atomspace.metta:/PeTTa/mount/atomspace.metta jazzbox35/mwj:latest
```
To pass an input atomspace, replace `/full/path/to/atomspace.metta` above with the absolute path to your `.metta` file. For example: `/home/user/my_file.metta:/PeTTa/mount/atomspace.metta`. Don't change `:/PeTTa/mount/atomspace.metta` just change `/full/path/to/atomspace.metta` to `/home/user/my_file.metta`.

Note: the order of ports is `host:container`, so if you want your machine to invoke the server using your machine's host port 80 you would use `127.0.0.1:80:5000`.

<B>2. Send a MeTTa query (example)</B>
```bash
curl -X POST http://localhost:5000/metta \
  -H "Content-Type: text/plain" \
  --data '!(+ 1 2)'
```
The above command should return a result of `[3]` if the installation worked. For curl commands, always put single quotes (') around your MeTTa query to avoid shell interpolation.
To experiment with running MeTTa code, clone the RunMeTTaCode.py program in the repo. 


<B>3. Stop the server (graceful)</B>
```bash
curl -X POST http://localhost:5000/stop
```
Alternatively, stop the Docker container directly:
```bash
docker stop mwj
```
IF YOU WANT DOCKER TO CLEAR AND RESTART YOUR ATOMSPACE AUTOMATICALLY USING /stop:

   Use of the `--restart=always` parameter in your "docker run" command will force Docker to
   stop and immediately restart the server from scratch. This is useful if you want to clear
   atomspace, easily restarting with a refreshed environment. If you use `--restart=always`
   for this purpose (in your docker run command), follow these steps:

       1) Add `--restart=always` and omit `--rm` from the docker run command line.
       2) When you desire to refresh your atomspace from scratch issue /stop for this purpose.
       3) Use `docker stop <container>` instead of /stop to definitely stop the container.
       4) To restart, use `docker restart <container>` not `docker run...` since omitting --rm will 
          retain the container. The --rm parameter will remove the container every run. 
          Omitting --rm will retain your container and you will have to restart it.

If you wish to modify the server, clone the repo, change the code as desired for your installation, and rebuild the Docker image using the included `Dockerfile`.

"WAM" is an acronym for Warren Abstract Machine.
"JAM" is you get fast MeTTa, MORK in an AI toolbox!
