#!/bin/bash

# Remove the existing series indexes
rm -f series/*.md

# Get a list of uniq series, and generate series/foo.md for each one, using scripts/_series.md
# scripts/_series.md uses layout: series, and _layouts/series.html uses Liquid to iterate over
# pages with that series, generating the index.
grep -h '^series:' -R _posts | \
    cut -d: -f2- | \
    tr ' ' '\n' | \
    grep -v '^$' | \
    sort | \
    uniq | \
    xargs -r -I{} \
        sh -c "sed 's/SERIES/{}/g' < scripts/_series.md > series/{}.md"
