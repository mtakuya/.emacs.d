#!/usr/bin/bash

touch ~/.profile
source ~/.profile

apt-get update
apt-get install -y git make wget emacs dstat git-lfs graphviz gv sysstat jq mysqltuner percona-toolkit unzip htop tcpdump net-tools strace curl nasm ripgrep

wget https://go.dev/dl/go1.23.0.linux-amd64.tar.gz && rm -rf /usr/local/go && tar -C /usr/local -xzf go1.23.0.linux-amd64.tar.gz && rm go1.23.0.linux-amd64.tar.gz
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.profile

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
