# Importing a new libcurl

We're currently using libcurl 7.51.0, which is 2 years old; we should upgrade
to something newer.

## References

- http://pdh11.blogspot.com/2013/08/bow-shaped-branches-using-vendor.html
- https://imp.slack.com/archives/C6B140TSP/p1522253781000852

## Make sure master builds

If `master` doesn't build _anyway_, you're gonna have a bad time later.

    cd ~/Source/imp/ei
    git checkout master
    git pull
    scons

## Find the previous vendor merge

    git log thirdparty/curl

... find commit 3a9890f701 by David Dunn: "Import curl 7.51.0"; use that.

## Create a branch

    git checkout 3a9890f701
    git checkout -b curl-vendor

That leaves a lot of crap lying around, so clean it up:

    git clean -df

## Delete the old curl

    rm -rf thirdparty/curl

## Download the updated curl

I think I'll do this in stages. It'll take longer, but I prefer the smaller steps.

    curl -O https://curl.haxx.se/download/curl-7.52.0.tar.bz2
    tar xf curl-7.52.0.tar.bz2

## Import it

    mv curl-7.52.0 thirdparty/curl
    git add -A thirdparty/curl

    git commit -m "Import curl 7.52.0"

## Create a merge branch

    git checkout master
    git checkout -b rl-curl-7.52.0

## Merge the vendor branch

    git merge curl-vendor

... there will be conflicts; to get the list of conflicted files:

    git diff --name-only --diff-filter=U

(or VS Code will display them as 'MERGE CHANGES' in the source control sidebar)


