#!/bin/sh

go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest
go install github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest

mkdir ~/.emacs.d/git && cd ~/.emacs.d/git && git clone https://github.com/abo-abo/swiper && cd swiper && make deps && make compile
