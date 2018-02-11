from __future__ import print_function

import os,shutil

prjdir = "Fapper"
version = "2.0.0.0"
def c(s):
    print(">",s)
    err = os.system(s)
    assert not err

def nuke(pth):
    if os.path.isdir(pth):
        shutil.rmtree(pth)

nuke(prjdir + "/bin")
nuke(prjdir + "/obj")

os.chdir("%s.Test" % prjdir )
c("dotnet run")

os.chdir("../" + prjdir)
c("dotnet pack /p:Version=%s" % version)