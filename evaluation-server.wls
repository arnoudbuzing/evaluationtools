#!/usr/bin/wolframscript

EvaluationServer["172.31.44.86","8080"];

task = First @ AsynchronousTasks[];
Print[task];
WaitAsynchronousTask[task];
