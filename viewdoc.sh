#!/usr/bin/env bash

./compile.py
M2 -e 'load "OIGroebnerBases.m2"; uninstallPackage OIGroebnerBases; viewHelp installPackage OIGroebnerBases'
