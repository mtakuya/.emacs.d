#!/usr/bin/bash

sudo apt-get update
sudo apt-get install -y git curl libssl-dev zlib1g-dev autoconf bison build-essential libyaml-dev libreadline-dev libncurses5-dev libffi-dev libgdbm-dev

git clone https://github.com/rbenv/rbenv.git ~/.rbenv

echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(rbenv init -)"' >> ~/.bashrc

source ~/.bashrc

git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build

git -C ~/.rbenv/plugins/ruby-build pull

rbenv install 3.3.5
rbenv global 3.3.5

ruby -v

gem update
gem install ruby-lsp solargraph
