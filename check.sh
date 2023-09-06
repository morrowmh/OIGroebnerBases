#!/usr/bin/env bash

./compile.py
M2 -e 'load "OIGroebnerBases_debug.m2"; check OIGroebnerBases'
