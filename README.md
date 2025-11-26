# MettaWamJam

MWJ is a lightweight, blazing-fast SWI-Prolog HTTP server for MeTTa. It provides a `/metta` endpoint for MeTTa execution and `/stop` for controlled shutdown. MWJ runs the PeTTa transpiler for MeTTa and loads the atomspace (if provided).

MWJ is not intended for public deployment without additional security hardening, so you should craft your front end if needed. You could, for example, create an Apache front end facing the public internet and restrict access.

If you just want to use the handy default Docker image make sure you have Docker installed on your machine and then run the commands below (no need to clone the repo):

1. Pull the image
```bash
docker pull jazzbox35/mwj
```

2. Run the container (default)
```bash
docker run --rm -d --name mwj -p 127.0.0.1:5000:5000 --tty jazzbox35/mwj:latest
```

OR with your atomspace (mount a .metta file into the container)
```bash
docker run --rm -d --name mwj -p 127.0.0.1:5000:5000 --tty -v /full/path/to/atomspace.metta:/PeTTa/atomspace.metta jazzbox35/mwj:latest
```
To pass an input atomspace, replace `/full/path/to/atomspace.metta` above with the absolute path to your `.metta` file (for example: `/home/user/my_file.metta:/PeTTa/atomspace.metta`).

Note: the order of ports is `host:container`, so if you want your machine to invoke the server using your machine's host port 80 you would use `127.0.0.1:80:5000`.

3. Send a MeTTa query (example)
```bash
curl -X POST http://localhost:5000/metta \
  -H "Content-Type: text/plain" \
  --data '!(+ 1 2)'
```
The above command should return a result of `[3]` if the installation worked. For curl commands, always put single quotes (') around your MeTTa query to avoid shell interpolation.

4. Stop the server (graceful)
```bash
curl -X POST http://localhost:5000/stop
```
Alternatively, stop the Docker container directly:
```bash
docker stop mwj
```

If you wish to modify the server, clone the repo, change the code as desired for your installation, and rebuild the Docker image using the included `Dockerfile`.

"WAM" is an acronym for Warren Abstract Machine.