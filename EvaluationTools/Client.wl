Attributes[EvaluationRequest]={HoldAllComplete};

EvaluationRequest[ expr_ ] := EvaluationRequest[ <| "Host" -> "127.0.0.1", "Port" -> "27182", "Method" -> "POST" |>, expr ];

EvaluationRequest[ object_EvaluationServerObject, expr_ ] := EvaluationRequest[ <| "Host" -> object["Host"], "Port" -> object["Port"], "Method" -> "POST" |>, expr ]

EvaluationRequest[host_,port_,expr_] := EvaluationRequest[ <| "Host" -> host, "Port" -> port, "Method" -> "POST" |>, expr ];

EvaluationRequest[ assoc_Association, expr_ ] := Module[{query, url, request,response,result},
  host = assoc["Host"];
  port = assoc["Port"];
  method = assoc["Method"];
  query = ExportString[HoldComplete[expr],"Base64"];
  url = URLBuild[<|"Scheme" -> "http", "Domain" -> host, "Port" -> port |>];
  request = HTTPRequest[ url, <| Method -> method, "Body" -> query |> ];
  response = URLRead[request];
  result = ImportString[ response["Body"], "Base64"];
  result
]
