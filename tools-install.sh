#!/usr/bin/bash
apt-get update
apt-get install -y git make wget emacs dstat git-lfs graphviz gv sysstat jq mysqltuner unzip

wget https://github.com/tkuchiki/alp/releases/download/v1.0.21/alp_linux_amd64.zip
unzip alp_linux_amd64.zip
install ./alp /usr/local/bin
rm alp_linux_amd64.zip
