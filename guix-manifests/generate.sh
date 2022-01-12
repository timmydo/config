#!/bin/sh
guix package --export-manifest > `hostname`-packages.scm
