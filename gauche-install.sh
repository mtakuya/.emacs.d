#!/usr/bin/bash

sudo apt-get update
sudo apt-get install -y build-essential libgdbm-dev zlib1g-dev texinfo

curl https://raw.githubusercontent.com/practical-scheme/get-gauche/master/get-gauche.sh -o get-gauche.sh
bash ./get-gauche.sh
