##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
#
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
#
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
#
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
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
    print("filename provided:", filename)
    f = open(filename)
    lines = f.readlines()
    f.close()
    decoder = TafDecoder.Decoder()
    print("starting decode")
    result = decoder(lines)
    print("result:\n", result)
    errlist = TafDecoder.errors(result)
    print('====== Errors =======')
    for k, d in errlist['error']:
        print(k, d['index'], d['error'])
    print('====== Warnings =====')
    for k, d in errlist['warning']:
        print(k, d['index'], d['warning'])

if __name__ == '__main__':
    main()
