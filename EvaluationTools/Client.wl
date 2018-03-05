Attributes[EvaluationRequest]={HoldAllComplete};

EvaluationRequest[ expr_ ] := EvaluationRequest[ <| "Host" -> "127.0.0.1", "Port" -> "27182", "Method" -> "POST" |>, expr ];

EvaluationRequest[ object_EvaluationServerObject, expr_ ] := EvaluationRequest[ <| "Host" -> object["Host"], "Port" -> object["Port"], "Method" -> "POST" |>, expr ]

EvaluationRequest[host_,port_,expr_] := EvaluationRequest[ <| "Host" -> host, "Port" -> port, "Method" -> "POST" |>, expr ];

EvaluationRequest[ assoc_Association, expr_ ] := Module[{result},
  result = Switch[ assoc["Method"],
    "GET", requestGet[ assoc, expr],
    "POST", requestPost[ assoc, expr]
  ];
  result
]

Attributes[requestGet] = {HoldAllComplete};
requestGet[ assoc_Association, expr_ ] := Module[{},Null];

Attributes[requestPost] = {HoldAllComplete};
requestPost[ assoc_Association, expr_ ] := Module[{query,url,request,response,result},
  query = ExportString[HoldComplete[expr],"Base64"];
  url = URLBuild[<|"Scheme" -> "http", "Domain" -> assoc["Host"], "Port" -> assoc["Port"] |>];
  request = HTTPRequest[ url, <| Method -> assoc["Method"], "Body" -> query |> ];
  response = URLRead[request];
  result = ImportString[ response["Body"], "Base64"];
  result
]
