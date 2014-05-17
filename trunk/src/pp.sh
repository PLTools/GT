#!/bin/bash

m4 macro.m4 $1 > $1.pp
camlp5o ../camlp5/pa_gt.cmo -impl $1.pp