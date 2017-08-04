import socket
import json
from time import sleep

name = "frosch03"

x = '18:{"you":"frosch03"}560:{"punter":1,"punters":2,"map":{"sites":[{"id":4,"x":2.0,"y":-2.0},{"id":1,"x":1.0,"y":0.0},{"id":3,"x":2.0,"y":-1.0},{"id":6,"x":0.0,"y":-2.0},{"id":5,"x":1.0,"y":-2.0},{"id":0,"x":0.0,"y":0.0},{"id":7,"x":0.0,"y":-1.0},{"id":2,"x":2.0,"y":0.0}],"rivers":[{"source":3,"target":4},{"source":0,"target":1},{"source":2,"target":3},{"source":1,"target":3},{"source":5,"target":6},{"source":4,"target":5},{"source":3,"target":5},{"source":6,"target":7},{"source":5,"target":7},{"source":1,"target":7},{"source":0,"target":7},{"source":1,"target":2}],"mines":[1,5]}}'

def pickle(s):
    return str(len(s)) + ':' + s


def unpickle(_s):
    s = _s
    data = []

    while len(s) > 3:
        lendlen = len(s.split(':')[0])
        dlen    = int(s.split(':')[0])
        d = s[(lendlen + 1):(lendlen + 1 + dlen)]
        data.append(json.loads(d))
        s = s[(lendlen + 1 + dlen):]

    return data


class OnlineProtocol(object):
    """This abstracts the protocol of the icfp2017 task

    """
    def __init__(self, _name, _server):
        self.sock = socket.socket()
        self.port = 9010
        self.chunk = 1024
        self.name   = _name
        self.server = _server
        self.startupConnection()

    def startupConnection(self):
        self.sock.connect((self.server, self.port))
        sendDict = {'me': self.name}
        self.sock.send(pickle(json.dumps(sendDict)))
        self.res = unpickle((self.sock).recv(1024))


s = socket.socket()
s.connect(("punter.inf.ed.ac.uk", 9007))
s.send(pickle("{\"me\": \"frosch03\"}"))
