#!/usr/bin/env bash

./compile.py
M2 -e 'load "OIGroebnerBases.m2"; viewHelp installPackage OIGroebnerBases'
