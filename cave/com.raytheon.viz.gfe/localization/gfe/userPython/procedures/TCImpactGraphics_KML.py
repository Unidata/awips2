# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# TCImpactGraphics_KML
#
# Author: P. Santos/Joe Maloney - 4/19/2011
#
# Last edited: 27 July 2012 - Shannon/R. Anderson - made A2 compatible
# Last Modified 30 July 2012 - J Maloney/P. Santos - made it work with A2
# Modified 09 Sept 2014 - J. Maloney - for 2015 season, removed MarineThreat,
# renamed CoastalThreat -> StormSurgeThreat, InlandThreat -> FloodingRainThreat,
# removed verylow from kml styles in header
# Modified 11 Sept 2014 - J. Maloney/S. White - site ID is now automatically
# retrieved from the environment.
# Modified 16 Sept 2014 - J. Maloney/T. Lefebvre - impact statements are now
# read from TCVDictionary (in Utilities) and formatted on-the-fly! 
# Modified 21 Oct 2014 - J. Maloney - products now go in /awips2/GFESuite/hti/
# data.
#
# ----------------------------------------------------------------------------

MenuItems = ["Populate"]

import SmartScript
from numpy import *
import time
import os
import TimeRange
import AbsTime
from com.raytheon.uf.common.dataplugin.gfe.reference import ReferenceData_CoordinateType as CoordinateType
import TCVDictionary

class Procedure (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    def makeTimeRange(self):
        cTime = int(self._gmtime().unixTime()/ 3600) * 3600
        startTime = AbsTime.AbsTime(cTime - 12 * 3600)
        endTime = AbsTime.AbsTime(cTime + 12 * 3600)   # 12 hours
        tr = TimeRange.TimeRange(startTime, endTime)

        return tr

    def makeThreatKML(self,threatWEName,threatKeys,threatGrid_kml):
        
# COMMENTS CONFIG READ: the directory below is the directory where the kml txt files will be dumped.
# From there it is synchronized to the web servers along with the graphics. If you set up
# your gHLS scripts and data directories in a different place than recommended in the install
# instructions, you would need to change that directory here. Do not change .kml.txt to .kml.
# Only .txt file can be uploaded as include files. In the servers a php script will convert the
# file name so that a browser can properly interpret it as a kml file to be open with Google
# Earth or equivalent application.

# Also, make sure the ownership of the kml.txt files created below is fxa:fxalpha with permissions set
# to 666.

# You can test the kml files created by copying them outside AWIPS and renaming them .kml. Then open them with
# Google Earth.

        kml_filename = '/awips2/GFESuite/hti/data/' + threatWEName + '.kml.txt'
        kml = open(kml_filename, 'w')
        kml.write('<?xml version="1.0" encoding="UTF-8"?>\n')
        kml.write('<kml xmlns="http://www.opengis.net/kml/2.2">\n')
        kml.write('<Document><name>'+threatWEName+'.kml</name>\n<Style id="none"><PolyStyle><color>99e0e0e0</color><outline>0</outline></PolyStyle></Style>\n')
        kml.write('<Style id="low"><PolyStyle><color>9900ffff</color><outline>0</outline></PolyStyle></Style>\n')
        kml.write('<Style id="moderate"><PolyStyle><color>990fa7ff</color><outline>0</outline></PolyStyle></Style>\n')
        kml.write('<Style id="high"><PolyStyle><color>990000ff</color><outline>0</outline></PolyStyle></Style>\n')
        kml.write('<Style id="extreme"><PolyStyle><color>99cc00cc</color><outline>0</outline></PolyStyle></Style>\n')
        kml.write('<Folder><name>'+threatWEName+'</name><open>0</open>\n')
        kml.write('<ScreenOverlay><name>Product Legend</name><description>Product Legend</description><visibility>1</visibility><Icon>')

        # each threatWEName has its own separate legend
        # need site id, in lowercase
        SiteID = self.getSiteID().lower()

        if threatWEName == "StormSurgeThreat":
            kml.write('<href>http://www.nws.noaa.gov/images/ghls/' + SiteID + '/stormsurgethreatlegend.png</href>')
        elif threatWEName == "WindThreat":
            kml.write('<href>http://www.nws.noaa.gov/images/ghls/' + SiteID + '/windthreatlegend.png</href>')
        elif threatWEName == "FloodingRainThreat":
            kml.write('<href>http://www.nws.noaa.gov/images/ghls/' + SiteID + '/floodingrainthreatlegend.png</href>')
        elif threatWEName == "TornadoThreat":
            kml.write('<href>http://www.nws.noaa.gov/images/ghls/' + SiteID + '/tornadothreatlegend.png</href>')

        # Complete the kml legend
        kml.write('</Icon><overlayXY x=".02" y=".01" xunits="fraction" yunits="fraction" /><screenXY x=".02" y=".01" xunits="fraction" yunits="fraction" /><rotationXY x=".02" y=".01" xunits="fraction" yunits="fraction" /><size x="-1" y="-1" xunits="fraction" yunits="fraction" /></ScreenOverlay>')
        
        #threatKeys = self.getDiscreteKeys(threatWEName)
        #print "THREATKEYS ARE: ", threatKeys
        
        # initialize a flag.  It will only be NO for the first polygon in the file.
        flag = 'NO'
        
        for key in threatKeys:
            #print "Key:", key
            
            # get index for this key
            hazIndex = self.getIndex(key, threatKeys)
            #print "hazIndex:", hazIndex

            mask = equal(threatGrid_kml, hazIndex)
            
            #print "Number of Grid Points: ", sum(sum(mask))
                
            if sum(sum(mask)) == 0:
                continue
              
            # make an editArea from the mask
            editArea = self.decodeEditArea(mask)
           
            # extract the polygons from the edit area
            polygons = editArea.getPolygons(CoordinateType.LATLON)

            # pull out the impact statements from the TCVDictionary
            # We need to match the threatWEName to the entries found
            # in the TCVDictionary
            if threatWEName == "TornadoThreat":
                threat='Tornado'
            elif threatWEName == "StormSurgeThreat":
                threat='Storm Surge'
            elif threatWEName == "WindThreat":
                threat='Wind'
            else:
                threat='Flooding Rain'
                
            # Extract the appropriate list from the dictionary, join them
            # into a string, and make them separate bullets
            impactStatement = ""                
            impactList = TCVDictionary.PotentialImpactStatements[threat][key]
            impactStatement = "<br />* ".join(impactList)
            impactStatement = "* " + impactStatement
#            print "impactList:", impactList
#            print "impactStatement:", impactStatement
            
            if key == "None":
                kmlHeader='<Placemark><name>Threat Level - None to Little</name><description><b>Potential for None to Little Impact:</b><br />' + impactStatement + '</description>\n<styleUrl>#none</styleUrl>\n'
            
            elif key == "Elevated":
                kmlHeader='<Placemark><name>Threat Level - Elevated</name><description><b>Potential for Limited Impact:</b><br />' + impactStatement + '</description>\n<styleUrl>#low</styleUrl>\n'

            elif key == "Mod":
                kmlHeader='<Placemark><name>Threat Level - Moderate</name><description><b>Potential for Significant Impact:</b><br />' + impactStatement + '</description>\n<styleUrl>#moderate</styleUrl>\n'

            elif key == "High":
                kmlHeader='<Placemark><name>Threat Level - High</name><description><b>Potential for Extensive Impact:</b><br />' + impactStatement + '</description>\n<styleUrl>#high</styleUrl>\n'

            else:
                kmlHeader='<Placemark><name>Threat Level - Extreme</name><description><b>Potential for Devastating to Catastrophic Impact:</b><br />' + impactStatement + '</description>\n<styleUrl>#extreme</styleUrl>\n'

            for i in xrange(polygons.getNumGeometries()):
                poly = polygons.getGeometryN(i)
                shell = poly.getExteriorRing();
                if shell:
                    # If shell is true, it's a new polygon
                    if flag == 'YES':
                        # If flag is YES, this is not the first polygon we're writing out
                        # so close the previous polygon before continuing.
                        kml.write('</Polygon></Placemark>\n')
                    
                    kml.write(kmlHeader)
                    kml.write('<Polygon><outerBoundaryIs><LinearRing><coordinates>')
                    #print "Outer shell coordinates:"
                    for c in shell.getCoordinates():
                        #print "x:",c.x,"y:",c.y                  
                        line = str(c.x) + ',' + str(c.y) + ',0 \n'
                        kml.write(line)
                      
                    kml.write('</coordinates></LinearRing></outerBoundaryIs>')
                      # Now that we've written at least one polygon, set flag to YES
                    flag = 'YES'
                    
                    # CHECK FOR INNER LOOPS (HOLES)
                    for j in xrange(poly.getNumInteriorRing()):
                        hole = poly.getInteriorRingN(j)
                        #print "Hole",j,"coordinates:"
                        kml.write('<innerBoundaryIs><LinearRing><coordinates>')
                        for c in hole.getCoordinates():
                            #print "x:",c.x,"y:",c.y
                            line = str(c.x) + ',' + str(c.y) + ',0 \n'
                            kml.write(line)

                        kml.write('</coordinates></LinearRing></innerBoundaryIs>')                   

        kmlEnd='</Polygon></Placemark>\n</Folder></Document></kml>\n'
        kml.write(kmlEnd)
        kml.close()

        return

    def execute(self, varDict):
        
        tr = self.makeTimeRange()
        threatlist = ['StormSurgeThreat','WindThreat','FloodingRainThreat','TornadoThreat'] 
        #threatlist = ['TornadoThreat'] # Took out MarineThreat 8/6/12 S.O. # READDED MARINE THREAT JCM 5/21/14

        for grid in threatlist:
            threatWEName = grid
            #print "Doing grid for time range: ", tr
            threatGrid, threatKeys = self.getGrids("Fcst", threatWEName, "SFC", tr)

# COMMENTS CONFIG READ: For each threat element below, you need to change the edit area to the mask you are using 
# in the config file to generate that impact graphic. For example, for CoastalThreat that would be
# XXXCoastalThreat where XXX is the 3 letter ID for your office. For WindThreat it would be XXXWindThreat.
# And so on.

            if threatWEName == "StormSurgeThreat":
                editArea = self.getEditArea("StormSurgeWW_EditArea")
            elif threatWEName == "WindThreat":
                editArea = self.getEditArea("MFL")
            elif threatWEName == "FloodingRainThreat":
                editArea = self.getEditArea("MFL")
            elif threatWEName == "TornadoThreat":
                editArea = self.getEditArea("MFL_CWA")
            else:
                editArea = self.getEditArea("Marinezones")         

            threatEditArea = self.encodeEditArea(editArea)
            threatGrid_kml = where(threatEditArea, threatGrid, threatGrid-9.0)

            self.makeThreatKML(threatWEName,threatKeys,threatGrid_kml)

        # COMMENTS CONFIG READ: This path should be double-checked
        os.system("/awips2/GFESuite/hti/bin/kml_legend.sh")
        
        return

