#!/usr/bin/python3

import socket
from os import getuid
from sys import stdin
from time import sleep

address = "/tmp/oxonbot" + str(getuid()) + "/socket"
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
# s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

s.connect(address)

# print(s.recv(1024).decode())
# s.send("foo".encode())
exit = False
while not exit:
    line = stdin.readline()
    if line.rstrip() == "exit":
        exit = True
    s.send(line.encode())

s.close()
