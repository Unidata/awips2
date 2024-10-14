#!/awips2/python/bin/python

from sys import argv
from os import system
import subprocess as sub

#input path passed from pqact.conf
inPath = argv[1]

#split by '.' and cut off final extension
lstPath = inPath.split('.')[:-1]

#convert list to string with '.' separation
strPath = '.'.join(lstPath)

#gunzip the input path
syscmd = "gunzip -f %s"%inPath
system(syscmd)

#notify AWIPS-2
syscmd = "/awips2/python/bin/python /awips2/fxa/bin/src/qpidNotify/qpidNotify.py %s"%(strPath)
print(syscmd)
p = sub.Popen(syscmd,shell=True,stdout=sub.PIPE,stderr=sub.PIPE)
out,err = p.communicate()
