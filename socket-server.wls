#!/usr/bin/wolframscript

ip = First @ Select[ $MachineAddresses, StringStartsQ[#,"172."]& ];

SocketServer[{ip,"8080"}];

task = First @ AsynchronousTasks[];
Print[task];
WaitAsynchronousTask[task];
