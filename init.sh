#!/bin/sh

go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest
go install github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest
go install github.com/rogpeppe/godef@latest
go install github.com/jstemmer/gotags@latest
go install golang.org/x/tools/cmd/goimports@latest
go install github.com/x-motemen/gore/cmd/gore@latest

mkdir -p ~/.emacs.d/git && \
    cd ~/.emacs.d/git && \
    rm -fr swiper && \
    git clone https://github.com/abo-abo/swiper && \
    cd swiper && make deps && make compile
