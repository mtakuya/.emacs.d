#!/usr/bin/bash

touch ~/.profile
source ~/.profile

sudo apt-get update
sudo apt-get install -y git make wget emacs dstat git-lfs graphviz gv sysstat jq mysqltuner percona-toolkit unzip htop tcpdump net-tools strace curl nasm ripgrep
