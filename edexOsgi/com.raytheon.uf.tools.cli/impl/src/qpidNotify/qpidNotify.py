# !/bin/bash
#
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
# #
##############################################################################
# Provides a simple wrapper to the qpidNotify Python module allowing that module
# to be executed as a command line tool without requiring the .py extension.
#
# Critical: the first line of this file must point to a valid AWIPS II
#           provided installation of Python.
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer    Description
# ------------- -------- ----------- -------------------------------------------
# Sep 19, 2011  8804     MHuang       Initial creation
# Oct 09, 2012  13901    D. Friedman  Limit execution time
# Feb 20, 2013  15836    D. Friedman  Handle comma-delimited args.
#                                     Improve logging.
# May 27, 2016  16834    D. Friedman  Search for WMO heading.
# Jan 27, 2017  19698    D. Friedman  Use BROKER_HOST environment variable.
# Aug 30, 2017  6381     randerso     Return None from getMessageHeader if
#                                     WMO header not found
# Jul 23, 2019  7724     mrichardson  Upgrade Qpid to Qpid Proton
# Dec 12, 2019  7995     dgilling     Match changes to qpidingest.
# Jul 07, 2020  8187     randerso     Added program for qpid connection_id
##############################################################################

import argparse
from lib.Util import time_limit, TimeoutException
import logging
import os
import re
import sys
import time

from ufpy import qpidingest

logging.basicConfig(level=logging.INFO, datefmt='%H:%M:%S',
                        format="[%(process)s] %(asctime)s %(levelname)s: %(message)s")
log = logging.getLogger("qpidNotify")
logging.getLogger("qpidingest").setLevel(100)

wmoRE = re.compile(b"([A-Z]{3}[A-Z0-9](?:\d{2}|[A-Z]{2}) [A-Z0-9]{4} \d{6}(?: [A-Z]{3})?)[^\r\n]*[\r]*[\n]")


class EdexQpidIngest:

    @staticmethod
    def getMessageHeader(data):
        m = wmoRE.search(data)
        if m:
            return m.group(1).decode()
        else:
            return None

    def startConnection(self):
        # Find current QPID running hostname
        server = os.getenv('BROKER_HOST')
        if not server:
            server = os.getenv('DEFAULT_HOST')
            if server == 'ev':
                server = 'cpv1'
        # Make connection to QPID
        self.qpidIngest = qpidingest.IngestViaQPID(host=server, port=5672, program="qpidNotify")

    def sendToIngest(self, args):
        size = len(args)
        log.info("%d files will be sent to EDEX via qpidingest", size)
        time.sleep(5)

        fileCount = 0
        errCount = 0
        for outfile in args:
            # Make sure incoming file exists in /data_store/mhs directory
            if os.path.isfile(outfile):
                try:
                    with open(outfile, "rb") as f:
                        # Parse wmoId header info
                        header = EdexQpidIngest.getMessageHeader(f.read(256))
                        # Send message to the external dropbox queue for file to be ingested
                        self.qpidIngest.sendmessage(outfile, header)
                        log.info("Sent %s to EDEX via qpidingest with header '%s'", outfile, header)
                        fileCount += 1
                except IOError:
                    log.exception("Unable to open the file %s", outfile)
                    errCount += 1
                except qpidingest.QpidIngestException:
                    log.exception("Unable to send the file %s", outfile)
                    errCount += 1
            else:
                log.error("%s does not exist", outfile)
                errCount += 1

        try:
            self.qpidIngest.close()
        except qpidingest.QpidIngestException:
            log.warning("Failed to close connection to QPID.", exc_info=True)

        if fileCount == size:
            log.info("Successfully sent %d file(s) to EDEX via qpidingest", size)
            return 0
        elif errCount == size:
            log.error("Failed to send %d file(s) to EDEX via qpidingest", size)
            return 1
        elif errCount > 0 and fileCount < size:
            log.error("%d out of %d failed to be sent to EDEX via qpidingest", errCount, size)
            return 2


def parseArgs():
    parser = argparse.ArgumentParser(
                prog="qpidNotify",
                description='Send files to EDEX for ingest using the QPID broker.')
    parser.add_argument("files",
                        action="store",
                        nargs="+",
                        help="Files to be ingested by EDEX.")
    args = parser.parse_args()

    # If using WAN backup for the SBN, the list of files is
    # passed as a single comma-delimited argument.
    if len(args.files) == 1 and ',' in args.files[0]:
        setattr(args, "files", args.files[0].split(','))

    return args


def main():
    args = parseArgs()
    log.debug("args: %s", args)

    m = EdexQpidIngest()
    try:
        with time_limit(10):
            m.startConnection()
    except TimeoutException:
        log.error("Failed to connect to QPID broker within timeout period.")
        sys.exit(1)
    except Exception:
        log.exception("Failed to connect to QPID broker.")
        sys.exit(1)
    try:
        with time_limit(10):
            sys.exit(m.sendToIngest(args.files))
    except TimeoutException:
        log.error("Failed to send files within timeout period.")
        sys.exit(1)
    except Exception:
        log.exception("Failed to send files.")
        sys.exit(1)


if __name__ == '__main__':
    main()
