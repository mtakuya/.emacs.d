#!/usr/bin/bash

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

rustup component add clippy rust-analysis rust-src rust-docs rustfmt rust-analyzer
