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
#
#    Name:
#       getEditAreas.py
#       GFS1-NHD:A10245.0000-SCRIPT;1
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1 (DELIVERED)
#         Created:  31-OCT-2008 09:30:47      OBERFIEL
#           New routine to work around awful GFESuite code
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7420
#       	Action Date:       29-DEC-2008 15:09:10
#       	Relationship Type: In Response to
#       	Status:           NEXTRELEASE
#       	Title:             AvnFPS: Use of ifpServerText to get Edit Areas negatively impacts ifpServer
#       
#
import ifpc, getopt, sys, sets
import LogStream, AFPS

class simpleFetch:
    """Returns list of all defined edit areas from GFESuite ifpServer"""
    def __init__(self):
        self.__host = None
        self.__port = None
        self.__inputfile = sys.stdin
        self.__outputfile = sys.stdout
        self.__cmdLine()

        # Connects to ifpServer through ifpc interface
        self.__client = ifpc.IFPC(self.__host, self.__port)

    def __cmdLine(self):
        optlist, oargs = getopt.getopt(sys.argv[1:], "h:p:i:o:")
        for opt in optlist:
            if opt[0] == '-h':
                self.__host = opt[1]
            elif opt[0] == '-p':
                self.__port = int(opt[1])
            elif opt[0] == '-i':
                self.__inputfile = opt[1]
            elif opt[0] == '-o':
                self.__outputfile = opt[1]

        # sanity checks, make sure all required switches are specified
        if self.__host is None or self.__port is None:
            self.__usage()
            raise SyntaxWarning, "Error: Missing host or port"

    def __usage(self):
        print """
Usage: getEditAreas -h hostname -p rpcPortNumber -i file1 -o file2

    -h host where the ifpServer is running
    -p rpc port number for the ifpServer.
    -i input file listing edit areas to be looked for on ifpServer
    -o output file listing edit areas found on server
"""
    def SetIntersection(self):        
        try:
            sAllEditAreas = sets.Set(self.__client.editAreaNames())
            #
            allTAFs = []
            try:
                fh=open(self.__inputfile)
            except TypeError:
                pass
            #
            for lne in fh:
                allTAFs.append(lne.split()[0])
            
            try:
                fh.close()
            except NameError:
                pass            
        except:
            raise
        #
        # Create set objects
        sAllTAFs = sets.Set(allTAFs)
        #
        # Find out what's common between the two.
        longstring = '-r '+' -r '.join(sAllEditAreas & sAllTAFs)
        #
        # Write result to file
        fh=open(self.__outputfile,'w')
        fh.write(longstring)
        fh.close()

def main():
    LogStream.ttyLogOn();
    LogStream.logEvent("getEditAreas Starting")
    LogStream.logEvent(AFPS.DBSubsystem_getBuildDate(),
                       AFPS.DBSubsystem_getBuiltBy(),
                       AFPS.DBSubsystem_getBuiltOn(), 
                       AFPS.DBSubsystem_getBuildVersion())

    try:
        obj = simpleFetch()
        obj.SetIntersection()
    except Exception, e:
        LogStream.logBug(LogStream.exc())
        sys.exit(1)

    LogStream.logEvent("getEditAreas Finished")
    sys.exit(0)

if __name__ == "__main__":
    main()

