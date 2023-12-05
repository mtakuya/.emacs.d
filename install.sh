#!/bin/sh
apt-get update
apt-get install -y git make wget emacs
wget https://go.dev/dl/go1.21.4.linux-amd64.tar.gz && rm -rf /usr/local/go && tar -C /usr/local -xzf go1.21.4.linux-amd64.tar.gz
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.profile
source ~/.profile
cd .emacs.d/
./init.sh
