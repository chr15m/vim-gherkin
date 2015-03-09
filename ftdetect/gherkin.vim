au BufRead,BufNewFile  *.gk set filetype=gherkin

fun! s:DetectGherkinShebang()
    if getline(1) =~# '^#!.*/bin/env\s\+gherkin\>'
        set ft=gherkin
    endif
endfun

autocmd BufNewFile,BufRead * call s:DetectGherkinShebang()
