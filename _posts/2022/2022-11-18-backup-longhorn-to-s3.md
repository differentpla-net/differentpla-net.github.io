---
title: "Backing up Longhorn to S3"
date: 2022-11-19T17:49:00Z
tags: longhorn
layout: series
series: k3s
---

Create an S3 bucket:

1. Create bucket.
2. Bucket name: differentpla-net-longhorn-backups / eu-west-2; remember that bucket names must be unique for the entire world, so stick your domain / company name on the front. Maybe put a randomly-generated suffix on it as well.
3. ACLs disabled
4. Block all public access
5. Disable versioning
6. No tags
7. Enable encryption / S3-managed keys

Create a policy:

```
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "s3:PutObject",
                "s3:GetObject",
                "s3:ListBucket",
                "s3:DeleteObject"
            ],
            "Resource": [
              "arn:aws:s3:::differentpla-net-longhorn-backups"
              "arn:aws:s3:::differentpla-net-longhorn-backups/*"
            ]
        }
    ]
}
```

Call it Longhorn-Backup.

Create a user in AWS IAM:

1. Add user. Call it 'longhorn'. Select 'Access key - Programmatic access'
2. Attach the policy we just created.
3. No tags.

Make a note of the access key ID / secret.
