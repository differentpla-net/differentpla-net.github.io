---
title: "k3s on Raspberry Pi: node.js Server"
short_title: "node.js Server"
date: 2020-02-06T19:03:00
layout: series
series: k3s
tags: k3s raspberry-pi docker
---

Now that I've successfully run `nginx` on my cluster, I'm going to do the same with a simple `node.js` server.

## Create the node.js app

```
mkdir node-server
cd node-server
vi server.js    # see below
vi Dockerfile   # see below
```

## `server.js`

```
var http = require('http');
var server = http.createServer(function(req, res) {
        console.log('Received request for ' + req.url);
        res.writeHead(200);
        res.end('Hello World!');
});
server.listen(8111);
console.log("Server listening on port 8111");
```

## `Dockerfile`

```
FROM node:12.14.1
EXPOSE 8111
COPY server.js .
CMD ["node", "server.js"]
```

## Create the docker image

```
docker build -t node-server .
```

## Push it to the private docker repository

```
docker tag node-server rpi201:5000/node-server
docker push rpi201:5000/node-server
```

## Create a deployment

```
sudo kubectl create deployment node-server --image=rpi201:5000/node-server
```

## Expose the deployment

```
sudo kubectl expose deployment node-server --port 8111
```

## Check it's working

```
sudo kubectl get endpoints node-server
curl 10.42.1.16:8111
```

## Optional: Clean Up

```
sudo kubectl delete service node-server
sudo kubectl delete deployment node-server
```

## Links

- <https://stackoverflow.com/questions/28349392/how-to-push-a-docker-image-to-a-private-repository>
