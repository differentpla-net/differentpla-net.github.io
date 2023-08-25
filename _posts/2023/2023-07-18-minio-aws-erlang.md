---
title: "Using minio with aws_erlang"
date: 2023-07-17T09:17:20Z
tags: erlang minio
---

You've got some Erlang code that uses S3, and you want to test locally. Here's how to use `aws_erlang` and `minio`.

First, let's get minio running:

Based on https://min.io/download#/docker

```sh
docker run --detach \
    --name minio \
    -p 9000:9000 \
    -p 9001:9001 \
    minio/minio \
    server /data --console-address ":9001"
```

Browse to http://127.0.0.1:9001/ the default credentials are minioadmin/minioadmin.

Create an access key. http://127.0.0.1:9001/access-keys

jbeaSgzqDDM3qGIaLcMa
f8pLFpa1o40Xw7NgidjwmwrS3kkDrf5qwXq5GoDQ

Create a bucket. http://127.0.0.1:9001/buckets/add-bucket

Can we talk to it with aws-cli?

https://min.io/docs/minio/linux/integrations/aws-cli-with-minio.html

```sh
export AWS_ACCESS_KEY_ID=jbeaSgzqDDM3qGIaLcMa
export AWS_SECRET_ACCESS_KEY=f8pLFpa1o40Xw7NgidjwmwrS3kkDrf5qwXq5GoDQ
export AWS_REGION=localhost
aws --endpoint-url http://localhost:9000 s3 ls
aws --endpoint-url http://localhost:9000 s3 ls my-first-bucket
aws --endpoint-url http://localhost:9000 s3 cp README.md s3://my-first-bucket
```

That all seems to work. Can we get aws_erlang to talk to it?
