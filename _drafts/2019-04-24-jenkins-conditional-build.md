---
title: "Jenkins: Conditional Builds"
---

Sometimes you've got a Jenkins job that you want to run only when _part_ of your repository has changed.

For example:

- We've got an `agent-runtime` repository, containing the workings of Electric Imp's ... agent runtime. Inside that repository is a `docker/` folder, which contains the `Dockerfile` stuff required to build various docker images.
- We want a Jenkins job that builds the docker images, so it polls the `agent-runtime` repository.
- But we only want the job to run if the `docker/` directory is changed.

One of my colleagues came up with the following use of "Conditional step" in Jenkins.

1. Copy a "tag file" from the last-successful previous build of this job.
2. Recalculate the "tag".
3. If it hasn't changed, we're done; skip the build.
4. If it has changed, run the build and publish the new tag file for later.

For example, you might figure out the last-modified time of the relevant source files and save that to the tag file.

In our `agent-runtime-docker` job, we calculate the SHA of the contents of the `docker/` directory and use that as the tag.
