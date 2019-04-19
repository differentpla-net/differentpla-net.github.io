#!/bin/bash

rm tag/*.md
grep -h '^tags:' _posts/* | \
    cut -d: -f2- | \
    tr ' ' '\n' | \
    grep -v '^$' | \
    sort | \
    uniq | \
    xargs -n1 -r -I{} \
        sh -c "sed 's/TAG/{}/g' < scripts/_tag.md > tag/{}.md"
