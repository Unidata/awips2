#!/awips2/python/bin/python

# This program allows files containing warnings to be added to the practice VTEC table.


import sys, os, getopt, string
import logging
import traceback

from dynamicserialize.dstypes.com.raytheon.uf.common.activetable import SendPracticeProductRequest
from awips import ThriftClient
from awips import TimeUtil

class VTECDecoder(object):
    """
    Class for ingesting warnings into the practice active table.
    """

    def __init__(self):
        self._decodeCommandLine()

    def decode(self):
        """
        Sends the file to EDEX practice decoder
        """
        thriftClient = ThriftClient.ThriftClient(self._host, self._port, '/services')
        
        request = SendPracticeProductRequest()
        request.setProductText(self._getProduct())
        request.setDrtString(self._offtimeStr)
        request.setNotifyGFE(self._notifyGFE)
        
        processed = False
        try:
            thriftClient.sendRequest(request)
            log.info('Practice product %s sent to %s:%d.',
                     self._incomingFilename, self._host, self._port)
            processed = True
        except:
            log.warn("Error sending practice product %s to server:", 
                     self._incomingFilename, exc_info=True) 

        if self._deleteAfterProcessing and processed:
            os.unlink(self._incomingFilename)
            log.info("%s deleted", self._incomingFilename)

        return


    def _usage(self):
        """
        Prints out usage information if started without sufficient command
        line arguments.
        """
        s =  """
usage: VTECDecoder -f productfilename -d -a activeTableName
-f productfilename:  location of the file to be decoded
-d:                  delete input file after processing flag
-a activeTableName:  location of the active table to hold decoded info
-h: host             Send to host {host} -defaults to localhost
-p: port             Send to port {port} - defaults to 9581
"""
        sys.stderr.write(s)
        log.warn(s)

    def _decodeCommandLine(self):
        """
        Routine to decode the command line.
        """
        self._deleteAfterProcessing = False
        self._incomingFilename = None
        self._activeTableFilename = None
        self._notifyGFE = False
        self._makeBackups = True  #make backups after each "update"
        self._wmoid = None
        self._host = 'localhost'
        self._port = 9581
        self._offtimeStr = None

        if len(sys.argv) < 2:
            self._usage()
            raise getopt.GetoptError("Not enough arguments")

        log.info("Command line: %s" % " ".join(sys.argv[1:]))

        # We accept all the old parameters, but only use the ones described in usage()
        try:
            optionlist, arglist = getopt.getopt(sys.argv[1:], 'f:da:gw:nz:h:p:')
        except getopt.error, val:
            log.error("Option parsing error", exc_info=True)
            self._usage()
            raise

        for each_option in optionlist:
            if each_option[0] == '-f':
                self._incomingFilename = each_option[1]
            elif each_option[0] == '-w':
                pass
            elif each_option[0] == '-a':
                self._activeTableFilename = each_option[1]
            elif each_option[0] == '-d':
                self._deleteAfterProcessing = True
            elif each_option[0] == '-n':
                pass
            elif each_option[0] == '-g':
                self._notifyGFE = True
            elif each_option[0] == '-z':
                self._offtimeStr = each_option[1]
            elif each_option[0] == '-h':
                self._host = each_option[1] 
            elif each_option[0] == '-p':
                self._port = int(each_option[1])

        failed = False
        if self._incomingFilename is None:
            log.error("File Name Not Specified")
            failed = True
            
        if self._activeTableFilename is None:
            log.error("Active Table Not Defined")
            failed = True
        elif not self._activeTableFilename.lower().startswith("practice"):
            log.error('Active Table name must start with "practice"')
            failed = True

        if self._offtimeStr is not None:
            try:
                offset, launchstr = TimeUtil.determineDrtOffset(self._offtimeStr)
            except Exception, e:
                log.error("Invalid time offset (" + e.message + ")")
                failed = True

        if failed:
            self._usage()
            raise getopt.GetoptError("Invalid command line specified " + " ".join(sys.argv[1:]))
            
    def _getProduct(self):
        """
        Opens, reads the product
        """
        fd = None
        try:
            fd = open(self._incomingFilename, 'r')
            buf = fd.read()
        except:
            s = "Unable to open incoming file: " + self._incomingFilename
            log.error(s)
            raise
        finally:
            if fd is not None:
                fd.close()

        return buf

def main():
    global log
    logging.basicConfig(level=logging.INFO)
    log = logging.getLogger("VTECDecoder")
    try:
        log.info("VTECDecoder Starting")
        decoder = VTECDecoder()
        decoder.decode()
        decoder = None
        log.info("VTECDecoder Finished")
    except getopt.GetoptError:
        sys.exit(1)
    except:
        log.warn("Caught Exception: ", exc_info=True)
        sys.exit(1)

if __name__ == "__main__":
    main()
    sys.exit(0)
