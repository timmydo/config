#! /usr/bin/env python2
from subprocess import check_output

def get_pass(name):
    return check_output("gpg -dq ~/.password-store/" + name + ".gpg", shell=True).strip("\n")
