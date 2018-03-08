BeginPackage["EvaluationTools`"]

Get[ FileNameJoin[{DirectoryName[$InputFileName], "Usage.wl"}] ];

Begin["`Private`"]

Module[ {files},
  files = {"Client.wl", "Server.wl", "SocketServer.wl", "SocketClient.wl"};
  Map[ Get[ FileNameJoin[{DirectoryName[$InputFileName], #}] ] &, files ];
];

End[]

EndPackage[]
