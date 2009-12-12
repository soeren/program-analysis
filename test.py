"""
Small python script to automate the testing.
Loads an input file, determines the application from
the file name, and compares the output of the application
with a reference file.
"""
import sys
import os
import subprocess

testdir = "tests/"
cmd = "./Main"

def getfiles(d,e):
    for root,dirs,files in os.walk(d):
        return [m for m in files if m.endswith(e)]

def test(i):
    f = testdir+i
    print "Testing",f
    if i[:5] == "slice":
        p = subprocess.Popen([cmd,"-"+i[:1],i[7:9],f],stdout = subprocess.PIPE)
    else:
        p = subprocess.Popen([cmd,"-"+i[:1],f],stdout = subprocess.PIPE)
    out = p.stdout.readlines()
    ref = open(f[:-2]+"out","r").readlines()
    print "Success:",
    if out==ref:
        print '\033[1;32m',"Yes :-)",'\033[0m'
    else:
        print '\033[1;31m',"NO!",'\033[0m'
        print "Program output:"
        if i[:5] == "slice":
            subprocess.call([cmd,"-"+i[:1],i[7:9],f])
        else:
            subprocess.call([cmd,"-"+i[:1],f])

def check():
    if not os.access(testdir, os.R_OK):
        print '\033[1;31m',"No read access to test directory",'\033[0m'
        sys.exit(0)
    if not os.access(cmd, os.X_OK|os.R_OK):
        print '\033[1;31m',"Main binary not accessible, executable, existent",'\033[0m'
        sys.exit(0)

if __name__ == "__main__":
    check()
    input = getfiles(testdir, ".in")
    output = getfiles(testdir, ".out")
    for i in input:
        print "-----------------------------"
        test(i)
