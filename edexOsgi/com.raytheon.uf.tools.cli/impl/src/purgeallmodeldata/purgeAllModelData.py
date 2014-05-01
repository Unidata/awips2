#!/usr/bin/env python

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

import logging

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.grib.request import DeleteAllModelDataRequest
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.grid.request import DeleteAllGridDataRequest
from ufpy import ThriftClient
from ufpy import UsageArgumentParser

logger = None
def __initLogger():
    global logger
    logger = logging.getLogger("purgeAllModelData")
    logger.setLevel(logging.DEBUG)
    ch = logging.StreamHandler()
    ch.setLevel(logging.INFO)
    # Uncomment line below to enable debug-level logging
    # ch.setLevel(logging.DEBUG)
    formatter = logging.Formatter("%(asctime)s %(name)s %(levelname)s:  %(message)s", "%H:%M:%S")
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    
def __parseCommandLine():
    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve", prog='purgeAllModelData')
    parser.add_argument("-h", action="store", dest="host",
                      help="Host upon which the EDEX server is running", 
                      required=True, metavar="hostname")
    parser.add_argument("-p", action="store", type=int, dest="port", 
                      help="Port on which the EDEX server is listening",
                      required=True, metavar="portNumber")
    parser.add_argument("-m", action="append", dest="models", 
                      help="Name of the model to purge. Use multiple -m arguments to delete more than 1 model, but duplicate model names will be ignored.",
                      required=True, metavar="modelName")
    options = parser.parse_args()
    # Ensure no duplicates end up in the list of models
    options.models = list(set(options.models))
    logger.debug("Command-line arguments: " + str(options))
    return options

def main():
    __initLogger()
    logger.info("Starting purgeAllModelData.")
    options = __parseCommandLine()
    
    client = ThriftClient.ThriftClient(options.host, options.port)
    for model in options.models:
        logger.info("Deleting all data for model [" + model + "]...")
        try:
            req = DeleteAllModelDataRequest(model)
            client.sendRequest(req)
            logger.info("Grib data for model [" + model + "] successfully deleted...")
        except Exception:
            logger.exception("Could not purge grib data for model [" + model + "]:")
            
        try:
            req = DeleteAllGridDataRequest(model)
            client.sendRequest(req)
            logger.info("Grid data for model [" + model + "] successfully deleted...")
        except Exception:
            logger.exception("Could not purge grid data for model [" + model + "]:")
    logger.info("purgeAllModelData is complete.")


if __name__ == '__main__':
    main()