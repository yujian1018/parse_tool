#!/usr/bin/env bash
./rebar cl
./rebar co

cp priv/t_yecc.yrl ebin
cp -rf priv/api/ ebin
erl  -pa ebin -s t_escripter escript t_proto t_def -s erlang halt


