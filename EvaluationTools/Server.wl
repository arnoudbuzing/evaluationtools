
(* simplified syntax, using localhost and port 'e' ~ 2.7182 *)
EvaluationServer[] := EvaluationServer["127.0.0.1","27182"]

(* syntax for IPAddress wrapper *)
EvaluationServer[host_IPAddress,port_String] := EvaluationServer[First[host],port];

(* canonical syntax, using host/port *)
EvaluationServer[host_String, port_String] := Module[{socket,listener},
  socket = SocketOpen[{host,port}];
  listener = socketListener[socket];
  EvaluationServerObject[host,port,socket,listener]
]

socketListener[socket_SocketObject] := SocketListen[ socket,
  Function[{assoc},
    Module[{source,data,request,response,line,input,expr,result,type},
      {source,data}=Lookup[assoc,{"SourceSocket","Data"}];
      request=ImportString[data,"HTTPRequest"];
      input = Switch[
        request["Method"],
        "GET",handleGet[request],
        "POST",handlePost[request]
      ]; (* input will be a held expression *)
      expr=ReleaseHold[input]; (* actual evaluation takes place here *)
      type = "type" /. request["Headers"];
      response = Switch[ type,
        "Base64",
          result=ExportString[FullForm[expr],"Base64"];
          ExportString[HTTPResponse[result,<|"StatusCode"->200,"ContentType"->"text/plain","Headers"->{}|>],"HTTPResponse"],
        "ByteArray",
          result=BinarySerialize[expr];
          ExportString[HTTPResponse[result,<|"StatusCode"->200,"ContentType"->"binary/octetstream","Headers"->{}|>],"HTTPResponse"]
      ];
      WriteString[source,response];
      Close[source]
    ]
  ]
];

handleGet[request_HTTPRequest] := Module[{},
  ToExpression[ "q" /. request["Query"] ]
]

handlePost[request_HTTPRequest] := Module[{type,length},
  Print[request];
  type = "type" /. request["Headers"];
  length = ToExpression[ "content-length" /. request["Headers"] ];
  Print["type: ",type];
  Switch[ type,
    "Base64",
      Print["Base64"];
      ImportString[request["Body"],"Base64"],
    "ByteArray",
      Print["ByteArray"];
      $Failed
  ]
]




(*** typesetting ***)

EvaluationServerObject /:  MakeBoxes[  object:EvaluationServerObject[host_, port_,socket_,listener_], form:(StandardForm |TraditionalForm)] := BoxForm`ArrangeSummaryBox[
  EvaluationServerObject,
  object,
  None,
  { {BoxForm`SummaryItem[{"Host: ",host}]}, {BoxForm`SummaryItem[{"Port: ",port}]} },
  { {BoxForm`SummaryItem[{"Socket: ",socket}]}, {BoxForm`SummaryItem[{"Listener: ",listener}]} },
  form,
  "Interpretable"->True
]

EvaluationServerObject[host_,port_,socket_,listener_][key_] := <|"Host"->host,"Port"->port,"SocketObject"->socket,"SocketListener"->listener|>[key]
