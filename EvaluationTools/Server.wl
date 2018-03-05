EvaluationServer[] := EvaluationServer["127.0.0.1","27182"]

EvaluationServer[host_IPAddress,port_String] := EvaluationServer[First[host],port];

EvaluationServer[host_String, port_String] := Module[{socket,listener},
  socket = SocketOpen[{host,port}];
  listener = socketListener[socket];
  EvaluationServerObject[host,port,socket,listener]
]

socketListener[socket_SocketObject] := SocketListen[ socket,
  Function[{assoc},
    Module[{source,data,request,response,line,input,expr,result,len},
      {source,data}=Lookup[assoc,{"SourceSocket","Data"}];
      request=ImportString[data,"HTTPRequest"];
      input = Switch[
        request["Method"],
        "GET",handleGet[request],
        "POST",handlePost[request]
      ]; (* input will be a held expression *)
      expr=ReleaseHold[input]; (* actual evaluation takes place here *)
      Print[expr];
      result=ExportString[expr,"Base64"];
      len=StringLength[result];
      response=ExportString[HTTPResponse[result,<|"StatusCode"->200,"ContentType"->"text/plain","Headers"->{"Content-Length"->len}|>],"HTTPResponse"];
      Print[response];
      WriteString[source,response];
      Close[source]
    ]
  ]
];

handleGet[request_HTTPRequest] := Module[{result},
  request["Query"]
]

handlePost[request_HTTPRequest] := Module[{result},
  ImportString[request["Body"],"Base64"]
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
