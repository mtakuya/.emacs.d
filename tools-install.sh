#!/usr/bin/bash
apt-get update
apt-get install -y git make wget emacs dstat git-lfs graphviz gv sysstat jq mysqltuner percona-toolkit unzip htop tcpdump net-tools strace

wget https://go.dev/dl/go1.23.0.linux-amd64.tar.gz && rm -rf /usr/local/go && tar -C /usr/local -xzf go1.23.0.linux-amd64.tar.gz && rm go1.23.0.linux-amd64.tar.gz
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.profile

wget https://github.com/tkuchiki/alp/releases/download/v1.0.21/alp_linux_amd64.zip
unzip alp_linux_amd64.zip
install ./alp /usr/local/bin
rm alp_linux_amd64.zip
rm alp

wget https://github.com/tkuchiki/slp/releases/download/v0.2.1/slp_linux_amd64.zip
unzip slp_linux_amd64.zip
install ./slp /usr/local/bin
rm slp_linux_amd64.zip
rm slp
