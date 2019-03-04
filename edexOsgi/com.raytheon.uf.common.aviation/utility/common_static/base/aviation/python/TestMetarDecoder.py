##
##
#TestMetarDecoder.py
#
#Code snippet to test the METAR Decoder for AVNFPS in Python...
#
# Usage:
# $ python TestMetarDecoder.py MtrXxx.txt
# 
# where MtrXxx.txt is a METAR for XXX site, for example.

##
# This is a base file that is not intended to be overridden.
##

def main():
    import sys
    import MetarDecoder
    filename = sys.argv[1]
    print "filename provided:", filename   
    f = open(filename)   
    lines = f.readlines()
    f.close()
    decoder = MetarDecoder.Decoder()
    print "starting decode"
    result = decoder(lines)
    print "result:\n", result
    errlist = MetarDecoder.errors(result)
    print '====== Errors ======='
    for k, d in errlist['error']:
        print k, d['index'], d['error']
    print '====== Warnings ====='
    for k, d in errlist['warning']:
        print k, d['index'], d['warning']

if __name__ == '__main__':
   main()
