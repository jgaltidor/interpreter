#! /usr/bin/env python
import sys, glob, os

def usage():
  print 'usage: interpreter.py <file 1> <file 2> ... <file N>'
  exit(1);

## main method ##
if __name__ == '__main__':
  if len(sys.argv) < 2: usage()
  fileNames = [name for arg in sys.argv[1:] for name in glob.glob(arg)]
  if len(fileNames) == 0:
    print 'No existing file specified'
    usage()
  for f in fileNames:
    if not os.path.isfile(f):
      print "%s does not exists" % f
      usage()
  os.system("scala mylang.Interpreter " + " ".join(fileNames))
