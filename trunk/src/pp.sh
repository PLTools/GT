#!/bin/bash

m4 macro.m4 $1 > $1.pp
camlp5o pr_o.cmo ../camlp5/pa_gt.cmo -impl $1.pp