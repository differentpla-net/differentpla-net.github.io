#!/bin/bash

mkdir -p _tags
touch _tags/.gitkeep

# Remove the existing tag indexes
rm -f _tags/*.md

# Get a list of uniq tags, and generate _tags/foo.md for each one, using scripts/_tag.md
# scripts/_tag.md uses layout: tag, and _layouts/tag.html uses Liquid to iterate over
# pages with that tag, generating the index.
grep -h '^tags:' -R _posts | \
    cut -d: -f2- | \
    tr ' ' '\n' | \
    grep -v '^$' | \
    sort | \
    uniq | \
    xargs -r -I{} \
        sh -c "sed 's/TAG/{}/g' < scripts/_tag.md > _tags/{}.md"
