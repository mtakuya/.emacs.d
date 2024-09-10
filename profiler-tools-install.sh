#!/usr/bin/bash

wget https://github.com/tkuchiki/alp/releases/download/v1.0.21/alp_linux_amd64.zip
unzip alp_linux_amd64.zip
install ./alp /usr/local/bin
rm alp_linux_amd64.zip
rm alp

wget https://github.com/tkuchiki/slp/releases/download/v0.2.1/slp_linux_amd64.zip
unzip slp_linux_amd64.zip -x README.md
install ./slp /usr/local/bin
rm slp_linux_amd64.zip
rm slp
