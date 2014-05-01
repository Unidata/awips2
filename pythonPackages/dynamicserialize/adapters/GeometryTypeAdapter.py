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
# Adapter for com.vividsolutions.jts.geom.Polygon
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/20/11                      dgilling      Initial Creation.
#    
# 
#

# TODO: Implement serialization/make deserialization useful.
# Deserialization was simply implemented to allow GridLocation objects to be
# passed through thrift, but the resulting Geometry object will not be transformed into
# useful data; the base byte array is passed to a worthless Geometry class.

from dynamicserialize.dstypes.com.vividsolutions.jts.geom import Geometry

# NOTE: At the moment, EDEX serializes Polygon, MultiPolygons, Points, and
# Geometrys with the tag of the base class Geometry. Java's serialization
# adapter is smarter and can determine the exact object by reading the binary
# data. This adapter doesn't need this _yet_, so it has not been implemented.
ClassAdapter = 'com.vividsolutions.jts.geom.Geometry'

def serialize(context, coordinate):
    raise dynamicserialize.SerializationException('Not implemented yet')

def deserialize(context):
    data = context.readBinary()
    geom = Geometry()
    geom.setBinaryData(data)
    return geom

