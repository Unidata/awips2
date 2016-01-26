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

from __future__ import print_function
from ufpy.dataaccess import DataAccessLayer as DAL

import argparse
import os
import sys

#
# Standard testing and utility methods for most DAF unit test requests.
# The tests primarily check that no unexpected exceptions are thrown when
# retrieving data for DAF requests, instead of explicity asserting against
# expected outcomes.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/19/16        4795          mapeters       Initial Creation.
#    
# 
#

# Maximum number of levels, locations, times, and geometry/grid data to display
sampleDataLimit = 5

def testParameters(req):
    print("Testing getAvailableParameters()")

    params = DAL.getAvailableParameters(req)
    assert params is not None
    print(params)

    print("getAvailableParameters() complete\n")

def testIdentifiers(datatype):
    print("Testing get*Identifiers()")

    required = DAL.getRequiredIdentifiers(datatype)
    assert required is not None
    print("Required identifiers:", required)

    optional = DAL.getOptionalIdentifiers(datatype)
    assert optional is not None
    print("Optional identifiers:", optional)

    print("get*Identifiers() complete\n")

def testLevels(req):
    print("Testing getAvailableLevels()")

    levels = DAL.getAvailableLevels(req)
    assert levels is not None
    print("Number of levels: " + str(len(levels)))
    strLevels = map(str, levels[:sampleDataLimit])
    print("Sample levels:\n" + str(strLevels))

    print("getAvailableLevels() complete\n")

def testLocations(req):
    print("Testing getAvailableLocationNames()")

    locs = DAL.getAvailableLocationNames(req)
    assert locs is not None
    print("Number of location names: " + str(len(locs)))
    print("Sample location names:\n" + str(locs[:sampleDataLimit]))

    print("getAvailableLocationNames() complete\n")

def testTimes(req):
    print("Testing getAvailableTimes()")

    times = DAL.getAvailableTimes(req)
    assert times is not None
    print("Number of times: " + str(len(times)))
    strTimes = map(str, times[:sampleDataLimit])
    print("Sample times:\n" + str(strTimes))

    print("getAvailableTimes() complete\n")

def testGeometryData(req):
    print("Testing getGeometryData()")

    geomData = DAL.getGeometryData(req)
    assert geomData is not None
    print("Number of geometry records: " + str(len(geomData)))
    print("Sample geometry data:")
    for record in geomData[:sampleDataLimit]:
        print("geometry=" + str(record.getGeometry()), end="")
        for p in req.getParameters():
            print(" " + p + "=" + record.getString(p), end="")
        print()

    print("getGeometryData() complete\n")

def testGridData(req, testSameShape=True):
    """
    Test that we are able to successfully retrieve grid data for the given 
    request. Most data do not change shape, so an additional verification is
    typically made to ensure that all the retrieved data have the same shape,
    unless False is given for the testSameShape parameter.
    """
    print("Testing getGridData()")

    gridData = DAL.getGridData(req)
    assert gridData is not None
    print("Number of grid records: " + str(len(gridData)))
    if len(gridData) > 0:
        print("Sample grid data shape:\n" + str(gridData[0].getRawData().shape) + "\n")
        print("Sample grid data:\n" + str(gridData[0].getRawData()) + "\n")
        print("Sample lat-lon data:\n" + str(gridData[0].getLatLonCoords()) + "\n")

        if testSameShape:
            allGridsPopulated = True
            correctGridShape = gridData[0].getLatLonCoords()[0].shape
            for record in gridData:
                if record.getRawData() is None or record.getRawData().shape != correctGridShape:
                    allGridsPopulated = False
                    break

            print("All grid data records have correct grid data: " + str(allGridsPopulated))
            assert allGridsPopulated

    print("getGridData() complete\n")

def getParser():
    """
    Return an ArgumentParser for parsing the standard arguments: the host to run
    tests against and whether to display the data retrieved in the tests.
    """
    parser = argparse.ArgumentParser(conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host", default="localhost",
                         help="EDEX server hostname",
                         metavar="hostname")
    parser.add_argument("-v", action="store_true", dest="verbose", default=False,
                         help="Display data output")
    return parser 

def handleArgs(args):
    """
    Handle the arguments specified in getParser().
    """
    DAL.changeEDEXHost(args.host)
    # Suppress stdout unless user-requested
    if not args.verbose:
        sys.stdout = open(os.devnull, "w")

def parseAndHandleArgs():
    """
    Parses and handles the arguments specified in getParser(). Use this
    method to handle arguments when no additional arguments need to be added.
    """
    handleArgs(getParser().parse_args())
