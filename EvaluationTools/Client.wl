Attributes[EvaluationRequest]={HoldAllComplete};

(* simplified syntax, using localhost and port 'e' ~ 2.7182 *)
EvaluationRequest[ expr_ ] := EvaluationRequest[ <| "Host" -> "127.0.0.1", "Port" -> "27182", "Method" -> "POST" |>, expr ];

(* simplified syntax with server object (only works in the same kernel session) *)
EvaluationRequest[ object_EvaluationServerObject, expr_ ] := EvaluationRequest[ <| "Host" -> object["Host"], "Port" -> object["Port"], "Method" -> "POST" |>, expr ]

(* simplified syntax with host/port *)
EvaluationRequest[host_,port_,expr_] := EvaluationRequest[ <| "Host" -> host, "Port" -> port, "Method" -> "POST", "Encoding" -> "Base64" |>, expr ];

(* canonical, full syntax case uses Association *)
EvaluationRequest[ assoc_Association, expr_ ] := Module[{result},
  result = Switch[ assoc["Method"],
    "GET", requestGet[ assoc, expr],
    "POST", requestPost[ assoc, expr]
  ];
  result
]

Attributes[requestGet] = {HoldAllComplete};
requestGet[ assoc_Association, expr_ ] := Module[{query,url},
  query = ToString[ HoldComplete[expr], InputForm];
  url = URLBuild[<|
    "Scheme" -> "http",
    "Domain" -> assoc["Host"],
    "Port" -> assoc["Port"] |> ,
    {"q"->query}];
  request = HTTPRequest[ url, <| Method -> "GET", "Headers" -> { "type" -> "Base64" } |> ];
  response = URLRead[request];
  result = ImportString[ response["Body"], "Base64"];
  result
]

Attributes[requestPost] = {HoldAllComplete};
requestPost[ assoc_Association, expr_ ] := Module[{query,url,request,response,result,ctype},
  Switch[ assoc["Encoding"],
    "Base64",
      Print["Base64"];
      ctype = "Base64";
      query = ExportString[HoldComplete[expr],"Base64"];
      url = URLBuild[<|"Scheme" -> "http", "Domain" -> assoc["Host"], "Port" -> assoc["Port"] |>];
      request = HTTPRequest[ url, <| Method -> "POST", "Body" -> query, "Headers" -> { "type" -> ctype } |> ];
      response = URLRead[request];
      result = ImportString[ response["Body"], "Base64"],
    "ByteArray",
      ctype = "ByteArray";
      query = BinarySerialize[HoldComplete[expr]];
      url = URLBuild[<|"Scheme" -> "http", "Domain" -> assoc["Host"], "Port" -> assoc["Port"] |>];
      request = HTTPRequest[ url, <| Method -> "POST", "Body" -> query, "ContentType" -> "binary/octetstream", "Headers" -> { "Content-Length" -> Length[query], "type" -> ctype } |> ];
      response = URLRead[request];
      result = BinaryDeserialize[ response["BodyByteArray"] ]
  ];
  result
]
