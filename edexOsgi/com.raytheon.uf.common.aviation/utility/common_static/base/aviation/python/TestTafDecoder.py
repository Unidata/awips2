##
##
#TestTafDecoder.py
#
#Code snippet to test the TAF Decoder for AVNFPS in Python...
#
# Usage:
# $ python TestTafDecoder.py TafXxx.txt
# 
# where TafXxx.txt is a TAF for XXX site, for example.

##
# This is a base file that is not intended to be overridden.
##

def main():
    import sys
    import TafDecoder
    filename = sys.argv[1]
    print "filename provided:", filename   
    f = open(filename)   
    lines = f.readlines()
    f.close()
    decoder = TafDecoder.Decoder()
    print "starting decode"
    result = decoder(lines)
    print "result:\n", result
    errlist = TafDecoder.errors(result)
    print '====== Errors ======='
    for k, d in errlist['error']:
        print k, d['index'], d['error']
    print '====== Warnings ====='
    for k, d in errlist['warning']:
        print k, d['index'], d['warning']

if __name__ == '__main__':
   main()
