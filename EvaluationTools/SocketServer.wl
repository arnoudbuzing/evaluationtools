SocketServer[{host_,port_}] := Module[ {socket},
  socket = SocketOpen[{host,port},"ZMQ"];
  SocketListen[socket,
    Function[{assoc},
      BinaryWrite[assoc["SourceSocket"], BinarySerialize[ReleaseHold@BinaryDeserialize[assoc["DataByteArray"]]]]
    ],
    HandlerFunctionsKeys -> {"Socket", "SourceSocket", "DataByteArray"}
  ];
]
