#!/usr/bin/python3

import socket
from os import getuid
# from sys import stdin
# from time import sleep
import re


class Message:
    msg_type = None
    msg_subtype = None
    msg_content = None

    def __init__(self, text: str):
        matches = re.match('^([A-Z]+)\x1d([A-Z]+)\x1d?([^\x03]+)?\x03$', text)
        if (matches is not None) and (len(matches.groups()) > 1):
            self.msg_type = matches.group(1)
            self.msg_subtype = matches.group(2)
            if len(matches.groups()) == 3:
                self.msg_content = matches.group(3)

    def __str__(self):
        if self.msg_content is not None:
            return '{}:{}:{}'.format(self.msg_type,
                                     self.msg_subtype,
                                     self.msg_content)
        else:
            return '{}:{}'.format(self.msg_type,
                                  self.msg_subtype)


class QueryResponse(Message):
    path = None
    caller = None
    text = None

    def __init__(self, msg: Message):
        self.msg_type = msg.msg_type
        self.msg_subtype = msg.msg_subtype
        self.msg_content = msg.msg_content

        if self.msg_content is None:
            return

        matches = re.match('^([^\x1f]+)\x1f([^\x1f]+)\x1f([^\x1f]+)$',
                           self.msg_content)
        if matches is not None:
            self.path = matches.group(1)
            self.caller = matches.group(2)
            self.text = matches.group(3)


def ob_id_make_response(id: str):
    if not isinstance(id, str):
        raise TypeError
    if re.match('^\\w+$', id):
        result = 'ID' + chr(29) + 'RES' + chr(29) + id + chr(3)
        # return 'ID\x1dRES\x1d{}\n'.format(id).encode('utf-8')
        return result.encode('utf-8')
    return None


def ob_create_request(command: str):
    return "Q" + chr(29) + "ASK" + chr(29) + "cmd" + chr(31) + "caller"\
        + chr(31) + command + chr(3)


def read_line_print(sock: socket):
    buf = bytearray(1024)
    while True:
        numbytes = sock.recv_into(buf, 1024)
        buf_str = buf[:numbytes].decode()
        print(buf_str, end='')
        if buf_str[-1] == chr(3):
            print()
            break
        elif buf_str[-1] == '\n':
            break


def read_line_to_str(sock: socket):
    buf = bytearray(1024)
    line = ""
    while True:
        numbytes = sock.recv_into(buf, 1024)
        buf_str = buf[:numbytes].decode()
        line = line + buf_str
        if buf_str[-1] == chr(3):
            break
        elif buf_str[-1] == '\n':
            break
    return line


address = "/tmp/oxonbot" + str(getuid()) + "/socket"
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.connect(address)

msg = Message(read_line_to_str(s))
print(msg)
if msg.msg_type == 'ID' and msg.msg_subtype == 'REQ':
    s.send(ob_id_make_response('python'))
    print('Id sent', end='\n\n')
else:
    print('No id req?', end='\n\n')

msg = Message(read_line_to_str(s))
print(msg)

s.send(ob_create_request("roll 100 10").encode("utf-8"))
print('Request sent', end='\n\n')
msg = Message(read_line_to_str(s))
if msg.msg_subtype == 'RES':
    msg = QueryResponse(msg)
    print(msg.text)

s.send(ob_create_request("quote random").encode("utf-8"))
print('Request sent', end='\n\n')
msg = Message(read_line_to_str(s))
if msg.msg_subtype == 'RES':
    msg = QueryResponse(msg)
    print(msg.text)

s.close()
