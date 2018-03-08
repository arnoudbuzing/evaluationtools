SocketClient[{host_,port_}] := SocketConnect[{host,port},"ZMQ"]

Attributes[SocketRequest] = {HoldAllComplete};
SocketRequest[client_, expr_] := Module[{},
  BinaryWrite[client, BinarySerialize[ HoldComplete[ expr ] ] ];
  BinaryDeserialize[ SocketReadMessage[client] ]
]
