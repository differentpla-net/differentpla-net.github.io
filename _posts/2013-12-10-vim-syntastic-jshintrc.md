---
layout: post
title: "vim, syntastic, .jshintrc"
date: 2013-12-10T14:21:17.928Z
tags: vim jshint
alias: /post/UqckrDVlYJsDAAAB/vim-syntastic-jshintrc
---

I've been using [Syntastic](https://github.com/scrooloose/syntastic) for a while now.
It's awesome.

One thing it's missing, however, is the ability to automatically find an appropriate
`.jshintrc` file for the file you're editing.

Fixed. Put this lot in your `.vimrc` file:

    function s:find_jshintrc(dir)
        let l:found = globpath(a:dir, '.jshintrc')
        if filereadable(l:found)
            return l:found
        endif
    
        let l:parent = fnamemodify(a:dir, ':h')
        if l:parent != a:dir
            return s:find_jshintrc(l:parent)
        endif
    
        return "~/.jshintrc"
    endfunction
    
    function UpdateJsHintConf()
        let l:dir = expand('%:p:h')
        let l:jshintrc = s:find_jshintrc(l:dir)
        let g:syntastic_javascript_jshint_conf = l:jshintrc
    endfunction
    
    au BufEnter * call UpdateJsHintConf()
    