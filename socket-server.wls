#!/usr/bin/wolframscript

SocketServer[{"172.31.39.91","8080"}];

task = First @ AsynchronousTasks[];
Print[task];
WaitAsynchronousTask[task];
