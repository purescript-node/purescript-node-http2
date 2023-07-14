#!/usr/bin/env sh

openssl req -x509 -newkey rsa:2048 -nodes -sha256 -subj '/CN=localhost'   -keyout test/localhost-privkey.pem -out test/localhost-cert.pem
