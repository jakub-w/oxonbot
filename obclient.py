# Copyright (C) 2019 Jakub Wojciech <jakub-w@riseup.net>

# This file is part of Oxonbot.

#     Oxonbot is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.

#     Oxonbot is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.

#     You should have received a copy of the GNU General Public License
#     along with Oxonbot.  If not, see <https://www.gnu.org/licenses/>.


import socket
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


class OxonbotClient:
    sock = None
    connected = None

    def __init__(self, id: str, ip: str, port: int):
        self.id = id
        self.ip = ip
        self.port = port

    def __del__(self):
        if self.sock:
            self.sock.close()

    def Connect(self):
        if self.connected:
            return

        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        # self.addr = socket.getaddrinfo(self.ip, self.port, socket.AF_INET)
        self.sock.connect((self.ip, self.port))

        # this should be an ID request
        msg = Message(read_line_to_str(self.sock))
        if msg.msg_type == 'ID' and msg.msg_subtype == 'REQ':
            if msg.msg_type == 'ID' and msg.msg_subtype == 'REQ':
                self.sock.send(ob_id_make_response(self.id))
            else:
                return None

        # this should be ID:ACK
        msg = Message(read_line_to_str(self.sock))

        self.connected = True
        return True

    # TODO: take caller as an argument (also modify ob_create_request)
    def SendQuery(self, path: str, caller: str, query: str):
        if not self.connected:
            return None
        self.sock.send(
            ob_create_request(path, caller, query).encode('utf-8'))
        msg = Message(read_line_to_str(self.sock))
        if msg.msg_type == 'Q' and msg.msg_subtype == 'RES':
            msg = QueryResponse(msg)
            return msg
        return None


def ob_id_make_response(id: str):
    if not isinstance(id, str):
        raise TypeError
    if re.match('^\\w+$', id):
        result = 'ID' + chr(29) + 'RES' + chr(29) + id + chr(3)
        # return 'ID\x1dRES\x1d{}\n'.format(id).encode('utf-8')
        return result.encode('utf-8')
    return None


def ob_create_request(path: str, caller: str, command: str):
    return "Q" + chr(29) + "ASK" + chr(29) + path + chr(31) + caller\
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
