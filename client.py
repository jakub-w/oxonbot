#!/usr/bin/python3

import socket
from os import getuid
from sys import stdin
from time import sleep
import re


def ob_id_make_response(id: str):
    if not isinstance(id, str):
        raise TypeError
    if re.match('^\\w+$', id):
        result = 'ID' + chr(29) + 'RES' + chr(29) + id + '\n'
        # return 'ID\x1dRES\x1d{}\n'.format(id).encode('utf-8')
        return result.encode('utf-8')
    return None


def ob_create_request(command: str):
    return "Q" + chr(29) + "ASK" + chr(29) + "path" + chr(31) + "caller"\
        + chr(31) + command + "\n"

def read_line_print(sock: socket):
    buf = bytearray(1024)
    while True:
        numbytes = sock.recv_into(buf, 1024)
        buf_str = buf[:numbytes].decode()
        print(buf_str, end='')
        if buf_str[-1] == '\n':
            break


OB_ID_REQ = "ID\x1dREQ\n"

address = "/tmp/oxonbot" + str(getuid()) + "/socket"
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.connect(address)


if s.recv(1024).decode() == OB_ID_REQ:
    s.send(ob_id_make_response('python'))
else:
    print('No id req?')

read_line_print(s)
s.send(ob_create_request("roll 100 1000").encode("utf-8"))
read_line_print(s)

s.close()
