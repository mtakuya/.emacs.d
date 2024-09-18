#!/usr/bin/bash

source ~/.profile

wget https://go.dev/dl/go1.23.0.linux-amd64.tar.gz && rm -rf /usr/local/go && tar -C /usr/local -xzf go1.23.0.linux-amd64.tar.gz && rm go1.23.0.linux-amd64.tar.gz
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.profile

go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/deadcode@latest
go install github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest
go install github.com/rogpeppe/godef@latest
go install github.com/jstemmer/gotags@latest
go install github.com/go-delve/delve/cmd/dlv@latest
go install github.com/matsuu/kataribe@latest
go install github.com/matsuu/go-mysql-query-digest@latest
go install github.com/google/pprof@latest
go install github.com/pressly/goose/v3/cmd/goose@latest
