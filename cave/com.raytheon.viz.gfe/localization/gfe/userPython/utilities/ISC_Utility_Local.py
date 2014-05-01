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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# ISC_Utility_Local
#
#   Allows local overrides to configuration and routines in ISC_Utility
#
#-----------------------------------------------------------------------------
import ISC_Utility
import numpy
import SmartScript
import time

class ISC_Utility_Local(ISC_Utility.ISC_Utility):
    def __init__(self, dbss, eaMgr, mdMode=None, toolType="numeric"):
        ISC_Utility.ISC_Utility.__init__(self, dbss, eaMgr, mdMode)
    #------------------------------------------------------------------------
    #        
    #  C O N F I G U R A T I O N   S E C T I O N 
    #
    # Changes to ISC_Utility configuration routine can
    # be placed here.  This starts out as an exact copy of the ISC_Utility
    # routine - but can be changed by each site.
    #
    def configuration(self):
        #
        #  points which have an elevation difference greater than this will NOT
        #  be considered in ISC statistics (in feet).  NDFD sets this to 1000ft.
        #
        self.MAXTOPODIFF=1000.0
        #
        #  NDFD checks are not performed when one side of a border is a land
        #  point and the other side is an ocean point.  To do this, an EditArea
        #  with land/sea points needs to be calculated.  With LANDEDITAREA set
        #  to None - the code will calculate the land area by a 'union' of all
        #  points found in the CWA editareas named XXX, where the XXX values
        #  are taken from all the editareas name ISC_XXX.  If you have not
        #  overridden the ISC_XXX editarea or XXX edit areas, then this will
        #  work fine.  If you HAVE overridden these edit area - use the
        #  LANDEDITAREA to specify the name of an editarea that contains just
        #  land points (all others are assumed to be ocean points).
        #
        self.LANDEDITAREA=None # or string with name of EditArea containing land
        #
        #--------------------------------------------------------------------
        #  These configuration items for Show_ISC_Info and Show_ISC_Highlights.
        #
        #  If you want the check for a particular parm to ACTUALLY check other
        #  parms, then list them here.  Vector parms need not be listed - but
        #  the threshold for Vector parms in GFE is assumed to be the threshold
        #  for the magnitude part - and the threshold for the direction part is
        #  hard-coded below
        #
        self.MultiParms={"MaxRH":("MinT","TdMrn","MaxRH"),
                    "MinRH":("MaxT","TdAft","MinRH"),
                    "RH":   ("T","Td","RH"),
                   }

        #  Minimum number of points along a border before it considers a
        #  failing average threshold "significant" (to get rid of short borders)
        #
        self.MINPOINTS=10
        #
        #------------------------------------------------------------------
        #
        #  NDFD thresholds - should not need to be modified.
        #
        #  Each entry in THRESHOLDS contains a tuple ( parmnames, thresholdinfo)
        #     parmnames can be a tuple with many parms listed that use the
        #               same threshold
        #     thresholdinfo contains (thresholdtype,thresholdvalues,
        #                             conditions,dirflag) where:
        #         thresholdtype="contant","topo" or "graduated"
        #         thresholdvalues=
        #               for "constant" type:  value
        #
        #                   differences greater than value are considered
        #                   discrepant
        #
        #               for "topo" type: (elev,lowvalue,highvalue)
        #
        #                   if the elevation difference between points is
        #                   less than elev, then the lowvalue is used as
        #                   the threshold value.  Otherwise the highvalue
        #                   is used for the threshold value
        #
        #               for "graduated" type: (bigvalue,(lessthan,value),(lessthan,value),...)
        #
        #                   bigvalue is the default threshold value. However
        #                   if the lowest of the two pair points is less than the
        #                   'lessthan', then that 'value' is used for the
        #                   threshold instead.  All 'lessthan' values are checked,
        #                   so they should be listed in decreasing order.
        #
        self.DEFAULT_THRESHOLD=("constant",5,("none",0,0),0)
        self.THRESHOLDS=[
            (("T","Td","MaxT","MinT","TdAft","TdMrn"),
             ("topo",(500,5,7),("none",0,0),0)),
            (("HeatIndex","WindChill"),
             ("topo",(500,7,9),("none",0,0),0)),
            (("PoP","PoP12","PoP6","PoP12hr","PoP6hr"),
             ("constant",20,("none",0,0),0)),
            (("WindSpd","TransWindSpd","WindGust"),
             ("graduated",(15,(20,10)),("greater_equal",12,0),0)),
            (("WindDirec","TransWindDirec"),
             ("topo",(500,45,90),("greater_equal",12,1),1)),
            (("Sky"),
             ("topo",(500,25,35),("none",0,0),0)),
            (("QPF","QPF6hr"),
             ("graduated",(1.0,(3.0,0.5),(1.5,0.25)),("greater",0.25,0),0)),
            (("SnowAmt","SnowAmt6hr"),
             ("graduated",(6,(12,4),(6,2)),("greater",2,0),0)),
            (("SnowLevel","FzLevel","MixHgt"),
             ("constant",1000,("none",0,0),0)),
            (("RH","MaxRH","MinRH"),
             ("graduated",(25,(75,20),(50,15),(25,10)),("none",0,0),0)),
            (("WaveHeight"),
             ("graduated",(10,(36,9),(32,8),(28,7),(24,6),(20,5),(16,4),(12,3),(6,2)),("greater",0,1),0)),
            (("CWR"),
             ("constant",10,("none",0,0),0)),
            (("Haines"),
             ("constant",1,("none",0,0),0)),
            ]
    # ---------  E N D   C O N F I G U R A T I O N   S E C T I O N  ----------
