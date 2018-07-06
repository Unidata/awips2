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
# serverConfig -- base GFE server configuration file
#
# NOTE: THIS FILE SHOULD NOT BE USER-MODIFIED.  INSTEAD REFER TO THE
# LOCAL CONFIG DOCUMENTATION ON HOW TO OVERRIDE SETTINGS IN THIS FILE.
#
# Baseline GFE server configuration
#
# ----------------------------------------------------------------------------
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/09/2013          #1571     randerso       Changed projections to use the Java
#                                                 ProjectionType enumeration
#    10/03/2013          #2418     dgilling       Update for new pSurge 2.0 data.
#    10/03/2013          #2424     randerso       Change localTC to use dateutil instead of pytz
#                                                 to get correct offsets for Alaska
#    01/17/2014          #2719     randerso       Added NHA domain
#    02/20/2014          #2824     randerso       Added log message when local override files are not found
#    03/11/2014          #2897     dgilling       Add new MHWM databases to default configuration.
#    03/20/2014          #2418     dgilling       Remove unneeded D2D source PHISH.
#    04/17/2014          #2934     dgilling       Remove alias for TPCSurgeProb D2D database.
#    05/09/2014          #3148     randerso       Add tpHPCndfd to D2DAccumulativeElements for HPCERP
#    06/20/2014          #3230     rferrel        Added URMA25.
#    05/29/2014          #3224     randerso       Added "SPC":8 to D2DDBVERSIONS
#    07/09/2014          #3146     randerso       Removed unused import
#    07/10/2014                    swhite         Add surge and tropical threat WEs and their dependencies
#    01/08/2015          #15035    lshi           add site WNJ
#    12/03/2014          #3866     rferrel        Added GFS20
#    01/13/2015          #3955     randerso       Added definitions for NewTerrain database
#                                                 Added Topo to ISCPARMS
#    01/19/2015          #4014     dgilling       Added ETSS.
#    02/11/2015          #4053     rferrel        Added GLWN and moved GLERL to display only for Great Lakes sites..
#    01/19/2015          #4014     dgilling       Added ETSS.
#    02/24/2015          #16692    byin           Added RTMA. Removed gfsLR and GWW233
#    03/19/2015          #4300     randerso       Remove GUMa as it is obsolete (per Shannon White)
#    03/30/2015          #17288    bhunder        Added Guam-RTMA to D2D models
#    03/30/2015          #17206    yteng          Changed some parameters that are not rate parameters
#    03/31/2015          #17288    bhunder        Added Weather Params for RTMA
#    04/03/2015          #4367     dgilling       Change WindGust's time constraints back to TC1
#                                                 for Fcst/Official.
#    04/08/2015          #4383     dgilling       Define FireWX ISC configuration parameters.
#    04/15/2015          #17383    yteng          Change localTC to fix error that time constraints
#                                                 being off
#    Apr 25, 2015         4952     njensen        Updated for new JEP API
#    04/20/2015          #4414     dgilling       Add missing NWPSTrkngCG0 weather elements.
#    05/12/2015          #17144    bhunder        Added RTMA model
#    05/29/2015          17496     ryu            Changed parm definitions for Wave1-10 and Period1-10.
#
#    05/29/2015          #17144    bhunder        Added weather Params for URMA25 and OCONUS RTMA
#    09/02/2015          #4819     rferrel        Added HWRF.
#    09/09/2015          16287     amoore         Additional validation of user input
#    10/07/2015          #4958     dgilling       Added support for NationalBlend D2D data.
#    10/13/2015          #4961     randerso       Updated NewTerrain/BaseTerrain database definitions
#    10/30/2015          #17940    jendrowski     Responded to Code Review.  Mostly syntactical changes.
#    11/05/2015          #18182    ryu            Change D2DDBVERSIONS value for HPCERP to 24
#    12/22/2015          #14152    jwatson        Added Sky, Wind to GFSLAMPGrid parms
#    1/28/2016           #13910    amoore         Wave model data should be available in 3-hrly timesteps
#    02/09/2016          #5283     nabowle        Remove NGM support.
#    02/22/2016          #18161    wkwock         Add NationalBlend model for AK, PR, HW
#    02/23/2016          #14845    jwatson        Changed NamDNG5 to NamDNG for all sources and params.
#                                                 Changed D2DModels for CONUS and Alaska to
#                                                 namdng25 and AK-NamDNG3
#    04/01/2016          18777     ryu            Replace NCF ip addresses.
#    04/22/2016          #18896    wkwock         Add more nationalBlend Model
#    06/01/2016                    JCM            removed tc3ng from officialdbs for wave/period elements;
#                                                 removed Wave_XX and Period_XX; removed Wave10, Period10;
#                                                 added databases for all sites to baseline
#    08/08/2016          #5747     randerso       Support removal of wrapper.py
#    10/05/2016          19293     randerso       Fixed units on Tropical and a few other weather elements
#    12/12/2016          #19596    bhunder        Added "tp" to NationalBlend model D2DAccumulativeElements
#    02/20/2017        DCS18966    mdavis/pjendr. NIC adjustment: name changes and removal of obsolete 
#                                                 smart inits(DCS 19490). Fixed addOptionalParms.
#    03/17/2017          19673     jmaloney       Added Rip Current Probabilities (RipProb).
#    06/29/2017          6323      randerso       Added P-ETSS model
#    07/19/2017        DCS19490    gpetrescu      Removed AKwave10, Wave10 and Period10.
#    07/12/2017          6324      randerso       Added TPCWindProb_Prelim model
#    07/12/2017          6253      randerso       Updated for Standard Terrain
#    08/03/2017          #20054    bhunder        Added changes for ETSS model and for ETSS-HiRes model.
#    10/03/2017        DR20432     arivera        Replace GFS40 with GFS in SnowRatioGFS and remove
#                                                 GLOBHwave from SJU model databases.
#    11/28/2017          6539      randerso       Made P-ETSS and TPCSurgeProb elements D2DAccumulativeElements
#    12/06/2017        DCS20267    psantos        Add NWPS Rip Current Guidance
#    12/20/2017          20510     ryu            changes to StormTotalSnow parameter
#    04/03/2018        DR20656     arivera        Missing comma: "Dune Erosion Probability" in optionalParmsDict['marine']
#    05/09/2018        DR20715     arivera        Missing comma: groups['marineSites'] after 'AVAK'
#
####################################################################################################

##
# This is an incremental override file, indicating that the files at different
# localization levels will be combined. Incremental overrides are achieved by
# creating a localConfig file at a higher priority localization level that
# imports this base file.
#
# See the Configuration Guides->Server Configuration->Syntax for localConfig.py
# section of the GFE Online Help for more information.
##



#----------------------------------------------------------------------------
# USEFUL DEFINES
#----------------------------------------------------------------------------

import siteConfig,imp
import pprint
import re
import sys
import LogStream
from collections import defaultdict
BASELINE = getattr(siteConfig, 'BASELINE', 0)

#D scfp=open('/localapps/logs/scdebug.log','w')
class dbConfig(object):
    """Class to create GFE databases from modelDict"""
    def __init__(self,modelDict):
        self.modelDict=modelDict
        self.dbs=[]
        self.D2DMODELS=[]
        self.D2DDBVERSIONS={}
        self.D2DAccumulativeElements={}
        self.INITMODULES={}
        self.INITSKIPS={}

    def addConfiguredModels(self,ignoreList=[]):
        """Setup model databases defined in dbConfigDict.
        ignoreList can be used to filter out specific models
        """
        for m in self.modelDict:
            if m in ignoreList:
                continue
            # Don't allow BC model if regular is in ignore list
            if m[-2:] == 'BC' and m[:-2] in ignoreList:
                continue
            self.addGfeDB(m,self.modelDict[m])
        return
    def addGfeDB(self,modelname,dbConfigDict):
        """Does all the work needed for adding a model to GFE from entries
        in dbConfigDict. This populates dbs and sets various self
        variables.
        """
        if "DB" in dbConfigDict and "Parms" in dbConfigDict:
            self.dbs.append((dbConfigDict["DB"],dbConfigDict["Parms"]))
        if "D2DAccumulativeElements" in dbConfigDict:
            self.D2DAccumulativeElements[modelname]=dbConfigDict["D2DAccumulativeElements"]
        if "D2DDBVERSIONS" in dbConfigDict:
            self.D2DDBVERSIONS[modelname]=dbConfigDict["D2DDBVERSIONS"]
        if "D2DMODELS" in dbConfigDict:
            self.D2DMODELS.append((dbConfigDict["D2DMODELS"],modelname))
        if "INITMODULES" in dbConfigDict:
            if type(dbConfigDict["INITMODULES"]) is tuple:
                self.INITMODULES[dbConfigDict["INITMODULES"][0]] = dbConfigDict["INITMODULES"][1]
            else:
                self.INITMODULES[dbConfigDict["INITMODULES"]]=[modelname]
        if "INITSKIPS" in dbConfigDict:
            self.INITSKIPS[modelname]=dbConfigDict["INITSKIPS"]

#===============================================================================
#          Utility methods to manage GFE configuration
#===============================================================================
def mergeModelDicts(baseDict,addDict):
    """Combine serverConfig model dict and regional modelDict into one modelDict.
    Settings in baseDict are maintained unless overridden in addDict. The merging
    is done on a key by key basis of a specific model's dictionary (baseDict and
    addDict are dictionaries of dictionaries)
    This changes baseDict in place so the object passed in as baseDict is modified
    in the caller's scope.
    """
    for m,v in addDict.iteritems():
        if m not in baseDict:
            baseDict[m]=v
        else:
            for key,val in v.iteritems():
                baseDict[m][key]=val

def updateModelDict(modelDict,model,key,value):
    """Udates a specific entry for a model in modelDict.  model and key are dictionary
    keys into modelDict and modelDict[model] respectively. If model is not defined
    in modelDict, then a new entry is created. Otherwise, value replaces any existing
    value in modelDict[model][key].
    This changes modelDict in place so the object passed in as modelDict is modified
    in the caller's scope.
    """
    if model in modelDict:
        modelDict[model][key]=value
    else:
        modelDict[model]= {key : value}

def alterModelDef(dbTuple, name=None, format=None, dbType=None, single=None,
                  official=None, numver=None, purgeAge=None):
    """Alter GFE database definition. The definition is used in the dbs setting
    and has form:
                     (name,  format,  type, single, official, numVer, purgeAge)
    i.e., Practice = ("Fcst",  GRID, "Prac",   YES,       NO,      1,   24)

    Won't use these exact names since some might conflict with builtins
    Only supply what you want to change. To clone a model definition, just
    supply name='newname'
    """
    n,f,t,s,o,v,p=dbTuple
    l=[]
    for old,new in [(n,name),(f,format),(t,dbType),(s,single),(o,official),
                    (v,numver),(p,purgeAge)]:
        if new is None:
            l.append(old)
        else:
            l.append(new)
    return tuple(l)

def createModelDict(localsDict,dbs,D2DMODELS,D2DDBVERSIONS,D2DAccumulativeElements,
                  INITMODULES,INITSKIPS):
    """Convert serverConfig model configuration to a dictionary. This allows
    legacy serverConfig settings in dbs,D2DMODELS,INITMODULES, etc. to be
    maintained and then converted into a single dictionary where all settings
    for a model are together.

    WARNING: There can only be one version of a model in the dbs list. Fcst
    practice and test databases have to be handled separately.  This is ok
    because these databases are defined after any localConfig customizations
    of the normal Fcst database.

    modelDict contains the following keys. Only define what is needed, i.e.,
    it is not required to have every key defined
    "DB": Definition of the database, i.e., the first value in a dbs entry:
          ("wrfems", GRID, "", NO,  NO,  3, 0). This must be a tuple. The name
          in the DB entry must be the same as the model name used as the key
          into the modelDict variable.

    "Parms" : Definition of the weather element parameters in the database,
          i.e., the second part of the dbs entry. This is a list of tuples.

    "D2DMODELS" : D2D metadata database name for the source model.

    "INITMODULES': Name of the SmartInit module. This should be just the module
          name as a string, not a list.

    "D2DAccumulativeElements" : List of parms that are accumulative

    "D2DDBVERSIONS" : Number of versions of a D2D model to show in the Weather
          Element Browser. Defaults to 2 if not supplied.

    "INITSKIPS" : Used to skip model cycles.

    Example for a model:

    modelDict["CMCreg"]={
         "DB": ("CMCreg", "GRID", "", NO, NO, 2, 0),
         "Parms": [([Temp, Td, RH, Wind, WindGust, Sky, MixHgt, TransWind, QPF,
                     PoP, SnowAmt, SnowRatio], TC3),
                   ([PoP6, QPF6, QPF6hr, CQPF1],TC6NG),
                   ([QPF12, PoP12],TC12NG),
                   ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
                   ([MaxT], MaxTTC), ([MinT], MinTTC),
                  ],
         "D2DMODELS": "Canadian-Reg",
         "INITMODULES": "Local_CMCreg",
         "D2DAccumulativeElements": ["tpgemreg","tprun","tp3hr","tp6hr"],
         "D2DDBVERSIONS": 3,
    }
    """
    # Create self initializing dictionary via collections.defaultdict
    modelDict=defaultdict(dict)
    parmsDict={}
    tcDict={}

    for n,v in sorted(localsDict.items()):
        if type(v) is tuple and type(v[0]) is str and v[1] in [DISCRETE,SCALAR,VECTOR,WEATHER]:
            parmsDict[n]=v
        elif type(v) is tuple and len(v)==3 and type(v[0]) is int:
            tcDict[n]=v

    # Process dbs entries, i.e., model database definition
    tcDict={}
    for item in sorted(dbs):
        plist=[]
        parmTmpDict={}
        pDict={}
        for pt in item[1]:
            parmsList=[]
            # Try to find named parm setting
            for p in pt[0]:
                pname=p[0]
                pDict[pname]=p
                parmsList.append(pname)

            # Try to get a named time constraint
            name=next((name for name,v in tcDict.iteritems() if v == pt[1]), None)
            if name is None:
                name = `pt[1]`
            tcDict[name]=pt[1]
            if name in parmTmpDict:
                parmTmpDict[name]+=parmsList
            else:
                parmTmpDict[name]=parmsList

        # This consolidates parms by time constraint and sorts parm names.
        for tc in sorted(parmTmpDict.keys()):
            theParms=[]
            for p in sorted(parmTmpDict[tc]):
               theParms.append(pDict[p])
            plist.append((theParms, tcDict[tc]))

        modelDict[item[0][0]]={'DB':item[0],'Parms':plist}

    for si,ml in INITMODULES.items():
        m=ml[0]
        modelDict[m]['INITMODULES']=si
    for m,v in D2DDBVERSIONS.items():
        modelDict[m]['D2DDBVERSIONS']=D2DDBVERSIONS[m]

    for m,v in D2DAccumulativeElements.items():
        modelDict[m]['D2DAccumulativeElements']=v
    for m,v in INITSKIPS.items():
        modelDict[m]['INITSKIPS']=v
    for item in D2DMODELS:
        if type(item) is str:
           m=item
           v=item
        else:
           v,m=item
        if m in modelDict:
            modelDict[m]['D2DMODELS']=v
        else:
            modelDict[m]={'D2DMODELS':v}
    return modelDict

def changeParm(modelDict,pname,value,modelList=['Fcst']):
    """Alter a parm that is defined in modelDict Parm setting.

    pname: name of parm. This is a string not the parm definition
    value: the parm definition tuple. If the None object, then the parm
        will be deleted.
    modelList: List of model names to check. An empty list will check all
        models in modelDict.
    Return: Nothing. modelDict is altered in place.
    """
    if not modelList:
        modelList=modelDict.keys()
    for m in modelList:
        if m not in modelDict or 'Parms' not in modelDict[m] or \
                 not checkForParm(modelDict[m]['Parms'],pname):
            continue

        newpt=[]
        # parms is tuple (parmList,TC)
        for pList,tc in modelDict[m]['Parms']:
            # This makes a copy of the list of parms, not a reference
            # this is needed because we are changing the list in place.
            theParms= list(pList)
            match=False
            for matchParm in (p for p in theParms if p[0] == pname):
                match=True
                theParms.remove(matchParm)
            if match and value is not None:
                theParms.append(value)
            if theParms:
                newpt.append((theParms,tc))
        if newpt != modelDict[m]['Parms']:
            modelDict[m]['Parms'] = newpt

def changeParmTC(modelDict,pname,newTC,modelList=['Fcst']):
    """Alter a parm in that is defined in modelDict Parm setting.

    pname: name of parm. This is a string not the parm definition
    newTC: the new Time Contraint (tuple)
    modelList: List of model names to check. An empty list will check all
        models in modelDict.
    Return: Nothing. modelDict is altered in place.
    """
    if not modelList:
        modelList=modelDict.keys()
    for m in sorted(modelList):
        if m not in modelDict or 'Parms' not in modelDict[m]:
            continue
#d        print m,"checkForParm=",checkForParm(modelDict[m]['Parms'],pname)
        if not checkForParm(modelDict[m]['Parms'],pname):
            continue

        newpt=[]
        # Parms is tuple (parmList,TC)
        for pList,tc in modelDict[m]['Parms']:
            # This makes a copy of the list of parms, not a reference
            # this is needed because we are changing the list in place.
            theParms= list(pList)
            matchParm=next((p for p in theParms if p[0] == pname),None)
#d            print m,matchParm,tc,newTC,len(theParms)
            if matchParm:
                theParms.remove(matchParm)
                newpt.append(([matchParm],newTC))
#d                print "Added",matchParm,newTC
            if theParms:
#d                print "restored",theParms," to",tc
                newpt.append((theParms,tc))
        if newpt != modelDict[m]['Parms']:
#d            print 'Updated model',m
            modelDict[m]['Parms'] = newpt
#d            print modelDict[m]['Parms'],'\n'

def checkForParm(parmDef,pname):
    """Check a model parm definition if a parm named pname is in it.

    parmDef: list of tuples, each tuple is a list of parms and a time
        contraint. Call with modelDict[modelname]['Parms].
    pname: Name of parm (string).
    Returns: Boolean True if found, or False
    """
    for item in parmDef:
        t=next((pt for pt in item[0] if pt[0] == pname),None)
        if t is not None:
            return True
    return False

def getParmNames(parmsDef):
    """Return a list of parm names in a model parm definition

    parmsDef: list of tuples, each tuple is a list of parms and a time
        constraint. Call with modelDict[modelname]['Parms].
    Returns: List of string parameter names

    Here's an example of how to remove unused parms from Fcst, this can
    run in localConfig:

    parmsToRemove=[]
    for p in getParmNames(modelDict['Fcst']):
        pl=p.lower()
        for t in ['period','swell','wave','surf', 'surge']:
            if t in pl:
                parmsToRemove.append(p)
                break
    removeParms(modelDict,'Fcst',parmsToRemove)
    """
    result=[]
    for pList,tc in parmsDef:
        # p is the parmDef tuple where first item is the parm name
        newParms=[p[0] for p in pList]
        result+=newParms
    return sorted(result)

def printServerConfig(moduleObj,localsDict, logFile="/awips2/edex/logs/localConfig.log"):
    """Dump out ServerConfig final settings. localsDict is a dictionary of
    local variables in localConfig, normally locals().
    """
    # serverConfig log text
    scText=""
    try:
        with open(logFile,"w") as fp:
            # Print out dbs entries, i.e., model database definition
            fp.write("Configuration for %s\n" % localsDict['SID'])
            dbs=DATABASES
            for item in sorted(dbs):
                scText += "\ndbs[%s]: %s\n" % (item[0][0], str(item[0]))
                scText += _dumpParms(item[1])

            # Dump out serverConfig settings likely to be modified by localConfig
            scvars=["D2DMODELS", "INITMODULES",
                    "D2DDBVERSIONS", "D2DAccumulativeElements",
                    "REQUEST_ISC", "SEND_ISC_ON_SAVE",
                    "SEND_ISC_ON_PUBLISH", "REQUESTED_ISC_PARMS",
                    "ExtraWEPrecision", "INITSKIPS",
                    "HazardKeys",
                    "MAX_USER_BACKGROUND_PROCESSES",
                    "AdditionalISCRouting",
                    "ignoreDatabases",
                   ]

            for item in scvars:
                scText += "\n%s:\n" % item
                obj=getattr(moduleObj,item,None)
                if type(obj) is list:
                    obj.sort()
                scText += pprint.pformat(obj) +'\n'

            # This prints out all variables named parms*, i.e., parmsNAM12
            for k in sorted(localsDict.keys()):
                if k == "OFFICIALDBS" or re.match("parms[A-Z]+",k) is not None or \
                         k == "extraISCparms":
                    scText += "\n%s:\n" % k
                    scText += _dumpParms(localsDict[k])
            scText += printModelDict(localsDict)
            fp.write(scText)
    except IOError as e:
        LogStream.logProblem("printServerConfig open file problem "+logFile+" - log not created\n" +LogStream.exc(e))

def printModelDict(localsDict):
    """Convert serverConfig model configuration to a dictionary. This writes
    the dictionary as text. This does not create a usable modelDict, just one to
    use to print out the dictionary as python code."""

    modelDict={}
    parmsDict={}
    tcDict={}
    dbs=DATABASES
    scText=""
    for n,v in localsDict.items():
        if type(v) is tuple and type(v[0]) is str and v[1] in [DISCRETE,SCALAR,VECTOR,WEATHER]:
            parmsDict[n]=v
        elif type(v) is tuple and len(v)==3 and type(v[0]) is int:
            tcDict[n]=v

    scText += '\n'
    for n in sorted(parmsDict):
        scText += 'parmVar: %s = %s\n' % (n,`parmsDict[n]`)
    scText += '\n'
    for n in sorted(tcDict):
        scText += 'TC: %s = %s\n' % (n,`tcDict[n]`)
    scText += '\n'

    # Print out dbs entries, i.e., model database definition
    for item in sorted(dbs):
        plist=[]
        parmTmpDict={}
        for pt in item[1]:
            parmsList=[]
            # Try to find named parm setting
            for p in pt[0]:
                name=next((name for name,v in parmsDict.iteritems() if v == p), None)
                if name is not None:
                    parmsList.append(name)
                else:
                    parmsList.append(p[0])
            theParms='&nlq(['+', '.join(parmsList)+'], '
            # Try to get a named time constraint
            name=next((name for name,v in tcDict.iteritems() if v == pt[1]), None)
            if name is None:
                name = `pt[1]`
            if name in parmTmpDict:
                parmTmpDict[name]+=parmsList
            else:
                parmTmpDict[name]=parmsList
        # This consolidates parms by time constraint and sorts parm names.
        for tc in sorted(parmTmpDict.keys()):
            parmTmpDict[tc]=sorted(parmTmpDict[tc])
            theParms='&nlq(['+', '.join(parmTmpDict[tc])+'], '
            plist.append(theParms + tc +')&nrq')

        modelDict[item[0][0]]={'DB':item[0],'Parms':plist}
    for si,ml in INITMODULES.items():
        m=ml[0]
        entry=si
        if len(ml) > 1:
            # Multiple d2d models for smartinit
            # Try to get model from si name
            if si.find('Local_') == 0:
                m=si[6:]
            entry=(si,ml)
        if m in modelDict:
            # If a model has multiple SmartInit modules, try to best match which
            # Smartinit module to assign to the model.
            if 'INITMODULES' not in modelDict[m] or m in si:
                modelDict[m]['INITMODULES']=entry
        else:
            modelDict[m]={'INITMODULES':entry}

    for m,v in D2DDBVERSIONS.items():
        if m in modelDict:
            modelDict[m]['D2DDBVERSIONS']=D2DDBVERSIONS[m]
        else:
            modelDict[m]={'D2DDBVERSIONS':D2DDBVERSIONS[m]}

    for m,v in D2DAccumulativeElements.items():
        if m in modelDict:
            modelDict[m]['D2DAccumulativeElements']=v
        else:
            modelDict[m]={'D2DAccumulativeElements':v}
    for m,v in INITSKIPS.items():
        if m in modelDict:
            modelDict[m]['INITSKIPS']=v
        else:
            modelDict[m]={'INITSKIPS':v}
    for item in D2DMODELS:
        if type(item) is str:
           m=item
           v=item
        else:
           v,m=item
        if m in modelDict:
            modelDict[m]['D2DMODELS']=v
        else:
            modelDict[m]={'D2DMODELS':v}

    for m in sorted(modelDict):
        text=pprint.pformat(modelDict[m],width=80,indent=0)
        text=text.replace("'&nlq",'')
        text=text.replace("&nrq'",'')
        text=text.replace('"&nlq','')
        text=text.replace('&nrq"','')
        text=text.replace(", 'INITMODULES':",",\n'INITMODULES':")
        text=text.replace(')]}','),\n         ]\n}')
        text=text.replace('\n','\n            ')
        scText += "modelDict['%s'] = {\n            %s\n\n" % (m,text[1:])
    return scText

def _dumpParms(parms):
    """Pretty prints parms."""
    pDict={}
    result=""
    for item in parms:
        if type(item) is not tuple:
            # Not a parm definition!
            return
        pList,tc = item
        for p in pList:
            pDict[p[0]]=(p,tc)
    for k in sorted(pDict.keys()):
        result += "    %s\n" % repr(pDict[k])
    return result

def addOptionalParms(defaultTC,tcParmDict,parmDict,modelDict):
    """Adds parms from optionalParmsDict to the Fcst database.
    This is a convience function if most parms use the default time constraint.
    Otherwise, its just as easy to hard code what needs to be added for a
    optionalParmsDict entry.

    defaultTC: Default time constraint to if a parameter specific TC is not
               defined in tcParmDict.
    tcParmDict: Dictionary with keys of time constraints. Value is a list of
                parameter names to be added with that time constraint. Empty
                dictionary ok if everything should use the default. Example:
                tcParmDict={TC6NG:['IceLineAcc','IceFlatAcc',]}
    parmDict: Parameter dictionary with keys of parameter name and value is
              the parameter definition tuple. Keys must match keys in tcParmDict.
    modelDict: The serverConfig modelDict dictionary. Must already have Fcst
               defined. Changed in place.
    Returns: The parameter definition added to Fcst
    """

    tcParms={defaultTC:[]}
    for tc in tcParmDict:
        tcParms[tc]=[]
    if len(tcParmDict) == 0:
        tcParmDict['dummyTC']=['dummyParm']
    for pname,value in parmDict.iteritems():
        # Find the time constrait to use for this parm
        theTC=defaultTC
        for tc in tcParmDict:
            if pname in tcParmDict[tc]:
                theTC=tc
                break
        tcParms[theTC].append(value)

    theParms=[]
    for tc in tcParms:
        theParms.append((tcParms[tc],tc))
    modelDict['Fcst']['Parms'] += theParms
    return theParms

def addPowt(modelDict):
    """This sets up PoWT parameters for in Fcst database.
    """
    defaultTC=TC1
    # Use value of time constraint and string name of parm in tcParmDict
    tcParmDict={TC6NG:['IceLineAcc','IceFlatAcc',]
               }
    return addOptionalParms(defaultTC,tcParmDict,
                            optionalParmsDict['powt'],modelDict)

def addWinterWeatherProbs(modelDict):
    """This sets up ER Winter Weather Probability parameters in the Fcst database.
    """
    defaultTC=TC1
    # Use value of time constraint and string name of parm in tcParmDict
    tcParmDict={}
    return addOptionalParms(defaultTC,tcParmDict,
                            optionalParmsDict['winterProbs'],modelDict)

def addRainfallProbs(modelDict):
    """This sets up WPC rainfall probability parameters in the Fcst database.
    """
    defaultTC=TC1
    # Use value of time constraint and string name of parm in tcParmDict
    tcParmDict={}
    return addOptionalParms(defaultTC,tcParmDict,
                            optionalParmsDict['rainfallProb'],modelDict)

# Local-time based time constraints.  Does not automatically account for
# daylight savings time.  The dst flag is 0 for standard time and manually
# set to 1 for daylight time (if desired).  The start is specified in
# seconds local time, e.g., 6*HOUR would indicate 6am.
def localTC(start,repeat,duration,dst):
    timezone = SITES[GFESUITE_SITEID][3]
    import dateutil.tz, datetime
    tz = dateutil.tz.gettz(timezone)
    local = datetime.datetime.now(tz)
    delta = tz.utcoffset(local) - tz.dst(local)
    offset = delta.days*86400 + delta.seconds
    start = start - offset
    if dst == 1:
        start = start - 3600     #daylight savings flag
    if start >= 3600 * 24:
        start = start - 3600 * 24
    elif start < 0:
        start = start + 3600 * 24
    return (start, repeat, duration)

# imports the named module.  If the module
# does not exist, it is just ignored.  But
# if it exists and has an error, the exception
# is thrown.  If the module was imported returns
# true.
def siteImport(modName):
    try:
        fp, path, des = imp.find_module(modName)
        if fp:
            fp.close()
    except ImportError:
        LogStream.logEvent("No " + modName + " file found, using baseline settings.");
        return 0
    globals()[modName] = __import__(modName)
    return 1

def doIt():
    # Import the local site configuration file (if it exists)
    import doConfig
    import VTECPartners
    (models, projections, vis, wx, desDef, allSites, domain, siteId, timeZone,officeTypes) = \
      doConfig.parse(GFESUITE_SITEID, DATABASES, types, visibilities, SITES,
      allProjections)
    IFPConfigServer.models                  = models
    IFPConfigServer.projectionData          = projections
    IFPConfigServer.weatherVisibilities     = vis
    IFPConfigServer.weatherTypes            = wx
    IFPConfigServer.discreteDefinitions     = desDef
    IFPConfigServer.allSites                = allSites
    IFPConfigServer.officeTypes             = officeTypes
    IFPConfigServer.siteID                  = siteId
    IFPConfigServer.timeZone                = timeZone
    IFPConfigServer.d2dModels               = doConfig.d2dParse(D2DMODELS)
    IFPConfigServer.netCDFDirs              = doConfig.netcdfParse(NETCDFDIRS)
    IFPConfigServer.satData                 = doConfig.parseSat(SATDATA)
    IFPConfigServer.domain                  = domain

    (serverHost, mhsid, \
    rpcPort, \
    initMethods, accumulativeD2DElements, \
    initSkips, d2dVersions, \
    logFilePurgeAfter, \
    prdDir, baseDir, \
    extraWEPrecision, \
    tableFetchTime, \
    autoConfigureNotifyTextProd, \
    iscRoutingTableAddress, \
    requestedISCsites, requestISC, \
    sendiscOnSave, sendiscOnPublish, \
    requestedISCparms, \
    transmitScript) \
       = doConfig.otherParse(SITES.keys(), \
      GFESUITE_SERVER, GFESUITE_MHSID, \
      GFESUITE_PORT, INITMODULES,
      D2DAccumulativeElements,
      INITSKIPS, D2DDBVERSIONS, LOG_FILE_PURGE_AFTER,
      GFESUITE_PRDDIR, GFESUITE_HOME,
      ExtraWEPrecision, VTECPartners.VTEC_REMOTE_TABLE_FETCH_TIME,
      AUTO_CONFIGURE_NOTIFYTEXTPROD, ISC_ROUTING_TABLE_ADDRESS,
      REQUESTED_ISC_SITES, REQUEST_ISC, SEND_ISC_ON_SAVE, SEND_ISC_ON_PUBLISH,
      REQUESTED_ISC_PARMS, TRANSMIT_SCRIPT)
    IFPConfigServer.serverHost = serverHost
    IFPConfigServer.mhsid = mhsid
    IFPConfigServer.rpcPort = rpcPort
    IFPConfigServer.initMethods = initMethods
    IFPConfigServer.accumulativeD2DElements = accumulativeD2DElements
    IFPConfigServer.initSkips = initSkips
    IFPConfigServer.d2dVersions =  d2dVersions
    IFPConfigServer.logFilePurgeAfter = logFilePurgeAfter
    IFPConfigServer.prdDir = prdDir
    IFPConfigServer.baseDir = baseDir
    IFPConfigServer.extraWEPrecision = extraWEPrecision
    IFPConfigServer.tableFetchTime = tableFetchTime
    IFPConfigServer.autoConfigureNotifyTextProd =  autoConfigureNotifyTextProd
    IFPConfigServer.iscRoutingTableAddress = iscRoutingTableAddress
    IFPConfigServer.requestedISCsites = requestedISCsites
    IFPConfigServer.requestISC = requestISC
    IFPConfigServer.sendiscOnSave = sendiscOnSave
    IFPConfigServer.sendiscOnPublish = sendiscOnPublish
    IFPConfigServer.requestedISCparms = requestedISCparms
    IFPConfigServer.transmitScript = transmitScript
    IFPConfigServer.iscRoutingConfig = doConfig.parseAdditionalISCRouting(AdditionalISCRouting)

def getSimpleConfig():
    return IFPConfigServer

GFESUITE_SITEID = siteConfig.GFESUITE_SITEID
GFESUITE_MHSID = siteConfig.GFESUITE_MHSID
GFESUITE_SERVER =  siteConfig.GFESUITE_SERVER
GFESUITE_HOME   = siteConfig.GFESUITE_HOME
GFESUITE_PORT   = int(siteConfig.GFESUITE_PORT)
#GFESUITE_DATDIR = siteConfig.GFESUITE_DATDIR
GFESUITE_LOGDIR = siteConfig.GFESUITE_LOGDIR
GFESUITE_PRDDIR = siteConfig.GFESUITE_PRDDIR
#GFESUITE_SHPDIR = siteConfig.GFESUITE_SHPDIR
#GFESUITE_TOPODIR = siteConfig.GFESUITE_TOPODIR
#GFESUITE_VTECDIR = siteConfig.GFESUITE_VTECDIR

SID = GFESUITE_SITEID

# modelDict is a master configuration dictionary for all GFE databases
# Create self initializing dictionary via collections.defaultdict
modelDict=defaultdict(dict)

# ignoreDatabases is used when executing the final configuration to ignore
# certain models. The new paradigm with modelDict is to have one master
# modelDict and ignore datasets for specific regions or groups. Sites can
# add to or remove from ignoreDatabases in their localConfig.
ignoreDatabases=[]

# Groups are a way of setting up groups of parms for special or optionally used
# methodology. For example, the Probability of Weather Type methodology.
groups={}
groups['ALASKA_SITES'] = ['AFG', 'AJK', 'ALU', 'AER', 'ACR', 'AFC', 'VRH', 'AAWU', 'AVAK']
groups['GreatLake_SITES'] = ['LOT', 'MKX', 'GRB', 'DLH', 'MQT', 'APX', 'GRR', 'DTX',
                             'IWX', 'CLE', 'BUF', 'PBZ', 'ILN', 'IND', 'ILX', 'MPX', 'FGF']
groups['CONUS_EAST_SITES'] = ['ALY', 'AKQ', 'APX', 'BGM', 'BMX', 'BOX', 'BTV', 'BUF',
                              'CAE', 'CAR', 'CHS', 'CLE', 'CTP', 'DTX', 'FFC', 'GRR',
                              'GSP', 'GYX', 'ILM', 'ILN', 'IND', 'JAN', 'JAX', 'JKL',
                              'LCH', 'LMK', 'LWX', 'MEG', 'MFL', 'MHX', 'MLB', 'MOB',
                              'MQT', 'MRX', 'OKX', 'PAH', 'PBZ', 'PHI', 'RAH', 'RLX',
                              'RNK', 'TAE', 'TBW', 'ALR', 'RHA', 'TAR', 'TIR']
groups['RFC_SITES'] = ["ACR", "ALR", "FWR", "KRF", "MSR", "ORN", "PTR",
                       "RHA", "RSA", "STR", "TAR", "TIR", "TUA"]

siteRegion={}
# need to account for RFCs?
siteRegion['AR'] = groups['ALASKA_SITES']
siteRegion['CR'] = ['ABR','APX','ARX','BIS','BOU','CYS','DDC','DLH','DMX','DTX',
                    'DVN','EAX','FGF','FSD','GID','GJT','GLD','GRB','GRR','ICT',
                    'ILX','IND','IWX','JKL','LBF','LMK','LOT','LSX','MKX','MPX',
                    'MQT','OAX','PAH','PUB','RIW','SGF','TOP','UNR']
siteRegion['ER'] = ['AKQ','ALY','BGM','BOX','BTV','BUF','CAE','CAR','CHS','CLE',
                    'CTP','GSP','GYX','ILM','ILN','LWX','MHX','OKX','PBZ','PHI',
                    'RAH','RLX','RNK']
siteRegion['PR'] = ['GUM','HFO','PBP','PPG']
siteRegion['SR'] = ['ABQ','AMA','BMX','BRO','CRP','EPZ','EWX','FFC','FWD','HGX',
                    'HUN','JAN','JAX','KEY','LCH','LIX','LUB','LZK','MAF','MEG',
                    'MFL','MLB','MOB','MRX','OHX','OUN','SHV','SJT','SJU','TAE',
                    'TBW','TSA']
siteRegion['WR'] = ['BOI','BYZ','EKA','FGZ','GGW','HNX','LKN','LOX','MFR','MSO',
                    'MTR','OTX','PDT','PIH','PQR','PSR','REV','SEW','SGX','SLC',
                    'STO','TFX','TWC','VEF']

groups['OCONUS_SITES'] = groups['ALASKA_SITES'] + siteRegion['PR'] + ['SJU']

myRegion='ALL'
for r in siteRegion:
    if SID in siteRegion[r]:
        myRegion=r
        break

groups['powt']=list(siteRegion['CR'])
groups['marineSites']=[
                       # CONUS WFOs
                       "CAR","GYX","BOX","OKX","PHI","LWX","AKQ","MHX","ILM","CHS",
                       "BRO","CRP","HGX","LCH","LIX","MOB","TAE","TBW","KEY","MFL",
                       "MLB","JAX","SJU",
                       "SEW","PQR","MFR","EKA","MTR","LOX","SGX",
                       # AR sites
                       'AFC', 'AFG', 'AJK', 'AER', 'ALU', 'VRH', 'AVAK',
                       # OPC Atlantic and Pacific
                       'ONA', 'ONP',
                       # NHC/TAFB Pacific and Atlantic, Storm Surge
                       'NH1', 'NH2', 'NHA',
                       # HFO Marine, GUM
                       'HFO', 'HPA', 'GUM',
                      ]

groups['winterProbs']= [
            # ER sites
            'AKQ','ALY','BGM','BOX','BTV','BUF','CAE','CAR','CHS','CLE',
            'CTP','GSP','GYX','ILM','ILN','LWX','MHX','OKX','PBZ','PHI',
            'RAH','RLX','RNK',
            #CR sites
            'ABR','BIS','BOU','CYS','DDC','DMX','FGF','FSD','GLD','GRB',
            'ICT','IND','IWX','JKL','LMK','LOT','MKX','MPX','MQT','OAX',
            'PAH','PUB','SGF','GJT',
            #SR sites
            'FFC','LUB','MRX','OUN','TSA',
            #WR sites
            'FGZ','GGW','HNX','LKN','MFR','MSO','OTX','PDT','REV','SEW',
            'SGX','SLC','STO'
           ]

groups['rainfallProbs'] = ["BOX"]

#---------------------------------------------------------------------------
#
#  Weather Element configuration section.
#
#---------------------------------------------------------------------------

SCALAR  = 'Scalar'
VECTOR  = 'Vector'
WEATHER = 'Weather'
DISCRETE = 'Discrete'
YES = 1
NO = 0

#SCALAR, VECTOR
# name/type/units/description/max/min/precision/rateParm/
#WEATHER
# name/WEATHER/units/description/
#DISCRETE
# keyDef = [(keySym, keyDesc), (keySym, keyDesc)]
# name/DISCRETE/units/description/overlapCapable/keyDef/

# Standard Public Weather Elements
SID = GFESUITE_SITEID

maxTempVal=140.0
minTempVal=-100.0
maxTdVal=140.0
minTdVal=-100.0
maxQpfVal=10.0
maxIceVal=5.0
Temp =    ("T", SCALAR, "F", "Surface Temperature", maxTempVal, minTempVal, 0, NO)
Td =      ("Td", SCALAR, "F", "Dewpoint", maxTdVal, minTdVal, 0, NO)
MaxT =    ("MaxT", SCALAR, "F", "Maximum Temperature", maxTempVal, minTempVal, 0, NO)
MinT =    ("MinT", SCALAR, "F", "Minimum Temperature", maxTempVal, minTempVal, 0, NO)
HeatIndex = ("HeatIndex", SCALAR, "F", "Heat Index", maxTempVal, -80.0, 0, NO)
WindChill = ("WindChill", SCALAR, "F", "Wind Chill", 120.0, -120.0, 0, NO)
QPF =     ("QPF", SCALAR, "in", "QPF", maxQpfVal, 0.0, 2, YES)
Wind =    ("Wind", VECTOR, "kts", "Surface Wind", 125.0, 0.0, 0, NO)
WindGust = ("WindGust", SCALAR, "kts", "Wind Gust", 125.0, 0.0, 0, NO)
# special for TPC hurricane winds
HiWind =    ("Wind", VECTOR, "kts", "Surface Wind", 200.0, 0.0, 0, NO)
Weather = ("Wx", WEATHER, "wx", "Weather")
IceAcc = ("IceAccum", SCALAR, "in", "Ice Accumulation", maxIceVal, 0.0, 2, YES)
StormTotalIce = ('StormTotalIce', SCALAR, 'in', 'Storm Total Ice', maxIceVal, 0.0, 2, YES)
SnowAmt = ("SnowAmt", SCALAR, "in", "Snowfall amount", 20.0, 0.0, 1, YES)
StormTotalSnow = ("StormTotalSnow", SCALAR, "in","Storm Total Snow", 180.0, 0.0, 1, NO)
PoP     = ("PoP", SCALAR, "%", "Prob of Precip", 100.0, 0.0, 0, NO)
PoP6    = ("PoP6", SCALAR, "%", "Prob of Precip (6hr)", 100.0, 0.0, 0, NO)
PoP12   = ("PoP12", SCALAR, "%", "Prob of Precip (12hr)", 100.0, 0.0, 0, NO)
TstmPrb3 = ("TstmPrb3", SCALAR, "%", "Prob of Tstorm (3hr)", 100.0, 0.0, 0, NO)
TstmPrb6 = ("TstmPrb6", SCALAR, "%", "Prob of Tstorm (6hr)", 100.0, 0.0, 0, NO)
TstmPrb12 = ("TstmPrb12", SCALAR, "%", "Prob of Tstorm (12hr)", 100.0, 0.0, 0, NO)
Sky     = ("Sky", SCALAR, "%", "Sky Condition", 100.0, 0.0, 0, NO)
FzLevel = ("FzLevel", SCALAR, "ft", "Freezing level", 30000.0, 0.0, 0, NO)
SnowLevel = ("SnowLevel", SCALAR, "ft", "Snow Level", 18000.0, 0.0, 0, NO)
RH      = ("RH", SCALAR, "%", "Relative Humidity", 100.0, 0.0, 0, NO)

# DR20541 and 20482 - add collaborate PoP, SnowAmt, QPF and ndfd QPF tools
PoP12hr = ("PoP12hr", SCALAR, "%", "12 hr Chance of Precip", 100.0, 0.0, 0, NO)
QPF6hr = ("QPF6hr", SCALAR, "in", "6 hr Precipitation (in)", maxQpfVal, 0.0, 2, YES)
SnowAmt6hr = ("SnowAmt6hr", SCALAR, "in", "6 hr Snowfall", 30.0, 0.0, 1, YES)

# Cobb SnowTool included.
SnowRatio = ('SnowRatio', SCALAR, 'none', 'Snow Ratio', 40.0, 0.0, 1, NO)
#totalVV = ('totalVV', SCALAR, 'ubar/s', 'Total VV', 400.0, 0.0, 0, YES)
cape = ("cape", SCALAR, "1unit", "CAPE", 8000.0, 0.0, 1, NO)
ApparentT = ("ApparentT", SCALAR, "F", "Apparent Temperature", maxTempVal, -120.0, 0, NO)
LkSfcT = ("LkSfcT", SCALAR, "C", "Lake Surface T", 40.0, -2.0, 1, NO)
SnowMap = ("SnowMap", SCALAR, "in", "Snowfall Map", 20.0, 0.0, 1, NO)
StormTotalQPF = ('StormTotalQPF', SCALAR, 'in', 'Storm Total QPF (in)', 36.0, 0.0, 2, NO)
SeasonTotalSnow = ('SeasonTotalSnow', SCALAR, 'in', 'Season Total Snow (in)', 150.0, 0.0, 2, NO)

# Fire Weather Weather Elements
LAL = ("LAL", SCALAR, "cat", "Lightning Activity Level", 6.0, 1.0, 0, NO)
CWR = ("CWR", SCALAR, "%", "Chance of Wetting Rain", 100.0, 0.0, 0, NO)
Haines = ("Haines", SCALAR, "cat", "Haines Index", 6.0, 2.0, 0, NO)
MixHgt = ("MixHgt", SCALAR, "ft", "Mixing Height", 20000.0, 0.0, 0, NO)
Wind20ft =    ("Wind20ft", VECTOR, "kts", "20ft. Wind", 125.0, 0.0, 0, NO)
FreeWind = ("FreeWind", VECTOR, "kts", "Free Air Wind", 125.0, 0.0, 0, NO)
TransWind = ("TransWind", VECTOR, "kts", "Transport Wind", 125.0, 0.0, 0, NO)
Stability = ("Stability",SCALAR,"cat","Stability", 6.0,1.0,0, NO)
HrsOfSun = ("HrsOfSun",SCALAR,"hrs","Hours of Sun",24.0,0.0,1, NO)
MarineLayer = ("MarineLayer",SCALAR,"ft","Depth of Marine Layer", 20000.0,0.0,0,NO)
InvBurnOffTemp = ("InvBurnOffTemp",SCALAR,"F","Inversion Burn-off Temperature", 120.0,-30.0,0, NO)
VentRate = ("VentRate", SCALAR, "kt*ft", "VentRate", 500000.0, 0.0, 0, NO)
DSI = ("DSI", SCALAR, "index", "DSI", 6.0, 0.0, 0, NO)
MaxRH      = ("MaxRH", SCALAR, "%", "Maximum Relative Humidity", 100.0, 0.0, 0, NO)
MinRH      = ("MinRH", SCALAR, "%", "Minimum Relative Humidity", 100.0, 0.0, 0, NO)
Wetflag = ("Wetflag", SCALAR, "yn", "1300LT WetFlag", 1.0, 0.0, 0, NO)
Ttrend = ("Ttrend", SCALAR, "F", "24hr Temperature Trend", 50.0, -50.0, 0, NO)
RHtrend = ("RHtrend", SCALAR, "F", "24hr Relative Humidity Trend", 100.0, -100.0, 0, NO)

# HPC Delta weather elements
DeltaMinT = ('DeltaMinT', SCALAR, 'F', 'DeltaMinT', 130.0, -80.0, 0, NO)
DeltaMaxT = ('DeltaMaxT', SCALAR, 'F', 'DeltaMaxT', 130.0, -80.0, 0, NO)
DeltaWind = ("DeltaWind", VECTOR, "kts", "Surface Delta Wind", 125.0, 0.0, 0, NO)
DeltaSky = ("DeltaSky", SCALAR, "%", "Delta Sky Condition", 100.0, -100.0, 0, NO)
DeltaPoP = ("DeltaPoP", SCALAR, "%", "Delta Prob of Precip", 100.0, -100.0, 0, NO)

# Special LAPS parms
Radar = ("Radar", SCALAR, "dbz", "Radar Reflectivity", 80.0, -20.0, 0, NO)

# RTMA parms
QPE =     ("QPE", SCALAR, "in", "QPE", maxQpfVal, 0.0, 2, YES)
#if SID in groups['ALASKA_SITES']: - not sure if this needs to be like that
if SID in groups['OCONUS_SITES']:
    TUnc =     ("TUnc", SCALAR, "F", "Temperature Anl Uncertainty", 20.0, 0.0, 0, NO)
    TdUnc =    ("TdUnc", SCALAR, "F", "Dewpoint Anl Uncertainty", 25.0, 0.0, 0, NO)
else:
    TUnc =     ("TUnc", SCALAR, "F", "Temperature Anl Uncertainty", 10.0, 0.0, 0, NO)
    TdUnc =    ("TdUnc", SCALAR, "F", "Dewpoint Anl Uncertainty", 15.0, 0.0, 0, NO)
# DR17144
SkyUnc  =  ("SkyUnc", SCALAR, "%", "Sky Condition Uncertainty", 100.0, 0.0, 0, NO)
WSpdUnc =  ("WSpdUnc", SCALAR, "kts", "WSpd Anl Uncertainty", 12.0, 0.0, 0, NO)
WDirUnc =  ("WDirUnc", SCALAR, "deg", "WDir Anl Uncertainty", 10.0, 0.0, 0, NO)
VisUnc  =  ("VisUnc", SCALAR, "SM", "Vsby Anl Uncertainty", 10.0, 0.0, 2, NO)
# DCS 17288
PressUnc = ("PressUnc", SCALAR, "Pa", "Press Anl Uncertainty", 110000.0, 0.0, 2, NO)
Pressure = ("Pressure", SCALAR, "Pa", "Pressure", 110000.0, 0.0, 2, NO)
WGustUnc =  ("WGustUnc", SCALAR, "kts", "WGust Anl Uncertainty", 12.0, 0.0, 0, NO)

# NamDNG parms
QPF3 =     ("QPF3", SCALAR, "in", "3HR QPF", maxQpfVal, 0.0, 2, YES)
QPF6 =     ("QPF6", SCALAR, "in", "6HR QPF", maxQpfVal, 0.0, 2, YES)
QPF12 =    ("QPF12", SCALAR, "in", "12HR QPF", maxQpfVal, 0.0, 2, YES)
Vis =      ("Vis", SCALAR, "SM", "Visibility", 10.0, 0.0, 2, NO)
SnowAmt6 = ("SnowAmt6", SCALAR, "in", "Snowfall amount (6hr)", 20.0, 0.0, 1, YES)

MaxT3 =  ("MaxT3", SCALAR, "F", "3hr Maximum Temperature", maxTempVal, minTempVal, 0, NO)
MinT3 =  ("MinT3", SCALAR, "F", "3hr Minimum Temperature", maxTempVal, minTempVal, 0, NO)
MaxRH3 = ("MaxRH3", SCALAR, "%", "3hr Maximum Relative Humidity", 100.0, 0.0, 0, NO)

# Parms for ,'SAT',Satellite
SatVisE  = ("VisibleE", SCALAR, "count", "Satellite Albdo %", 255.0, 0.0, 0, NO)
SatIR11E = ("IR11E", SCALAR, "C", "11 micron temperature", 58.0, -111.0, 0, NO)
SatIR13E = ("IR13E", SCALAR, "C", "13 micron temperature", 50.0, -111.0, 0, NO)
SatIR39E = ("IR39E", SCALAR, "C", "3.9 micron temperature", 50.0, -111.0, 0, NO)
SatWVE   = ("WaterVaporE", SCALAR, "C", "water vapor temperature", -11.0, -62.0, 0, NO)
SatFogE  = ("FogE", SCALAR, "C", "ir11 - ir39", 50.0, -111.0, 0, NO)

SatVisW  = ("VisibleW", SCALAR, "count", "Satellite Albdo %", 255.0, 0.0, 0, NO)
SatIR11W = ("IR11W", SCALAR, "C", "11 micron temperature", 58.0, -111.0, 0, NO)
SatIR13W = ("IR13W", SCALAR, "C", "13 micron temperature", 50.0, -111.0, 0, NO)
SatIR39W = ("IR39W", SCALAR, "C", "3.9 micron temperature", 50.0, -111.0, 0, NO)
SatWVW   = ("WaterVaporW", SCALAR, "C", "water vapor temperature", -11.0, -62.0, 0, NO)
SatFogW  = ("FogW", SCALAR, "C", "ir11 - ir39", 50.0, -111.0, 0, NO)

# TPC Wind Probability parms
prob34 = ("prob34", SCALAR, "%", "WS34 CPROB", 100.0, 0.0, 0, NO)
prob50 = ("prob50", SCALAR, "%", "WS50 CPROB", 100.0, 0.0, 0, NO)
prob64 = ("prob64", SCALAR, "%", "WS64 CPROB", 100.0, 0.0, 0, NO)
pws34 = ("pws34", SCALAR, "%", "34WSIPROB", 100.0, 0.0, 0, NO)
pws50 = ("pws50", SCALAR, "%", "50WSIPROB", 100.0, 0.0, 0, NO)
pws64 = ("pws64", SCALAR, "%", "64WSIPROB", 100.0, 0.0, 0, NO)
pwsD34 = ("pwsD34", SCALAR, "%", "Day34WSIPROB", 100.0, 0.0, 0, NO)
pwsN34 = ("pwsN34", SCALAR, "%", "Night34WSIPROB", 100.0, 0.0, 0, NO)
pwsD64 = ("pwsD64", SCALAR, "%", "Day64WSIPROB", 100.0, 0.0, 0, NO)
pwsN64 = ("pwsN64", SCALAR, "%", "Night64WSI PROB", 100.0, 0.0, 0, NO)
pws34int = ("pws34int", SCALAR, "%", "34WSIntPROB", 100.0, 0.0, 0, NO)
pws64int = ("pws64int", SCALAR, "%", "64WSIntPROB", 100.0, 0.0, 0, NO)

# Surge parms for HLS/TCV
InundationMax = ("InundationMax", SCALAR, "ft", "Max Inundation", 30.0, -100.0, 1, NO)
InundationTiming = ("InundationTiming", SCALAR, "ft", "Incremental Inundation", 30.0, -100.0, 1, NO)
SurgeHtPlusTideMSL = ("SurgeHtPlusTideMSL", SCALAR, "ft", "Surge above MSL", 30.0, -100.0, 1, NO)
SurgeHtPlusTideMLLW = ("SurgeHtPlusTideMLLW", SCALAR, "ft", "Surge above MLLW", 30.0, -100.0, 1, NO)
SurgeHtPlusTideMHHW = ("SurgeHtPlusTideMHHW", SCALAR, "ft", "Surge above MHHW", 30.0, -100.0, 1, NO)
SurgeHtPlusTideNAVD = ("SurgeHtPlusTideNAVD", SCALAR, "ft", "Surge above NAVD88", 30.0, -100.0, 1, NO)

# parms for storm surge collaboration
SShazardKeys = [("<None>",""), ("SS.A", "STORM SURGE WATCH"), ("SS.W", "STORM SURGE WARNING")]
ProposedSS = ("ProposedSS", DISCRETE, "wwa", "Proposed StormSurge Hazards", YES, SShazardKeys, 7)
tempProposedSS = ("tempProposedSS", DISCRETE, "wwa", "Temp Proposed StormSurge Hazards",
              YES, SShazardKeys, 4)
InitialSS = ("InitialSS", DISCRETE, "wwa", "Initial StormSurge Hazards",
              YES, SShazardKeys, 4)
DiffSS = ("DiffSS", SCALAR, "None", "Difference StormSurge Hazards", 2.0, -1.0, 0, NO)

# parms for tropical cyclone threat graphics
Threat4Keys = [("None","None to Little"), ("Elevated","Elevated"), ("Mod", "Moderate"), ("High", "High"), ("Extreme","Extreme"),]

FloodingRainThreat = ("FloodingRainThreat", DISCRETE, "cat", "Flooding Rain Threat", NO, Threat4Keys,2)
StormSurgeThreat = ("StormSurgeThreat", DISCRETE, "cat", "Storm Surge Threat", NO, Threat4Keys,2)
WindThreat = ("WindThreat", DISCRETE, "cat", "Wind Threat", NO, Threat4Keys,2)
TornadoThreat = ("TornadoThreat", DISCRETE, "cat", "Tornado Threat", NO, Threat4Keys,2)
#    09/13/2016      JCM    changed precision of QPFtoFFGRatio to 2, max from 8 to 1000
QPFtoFFGRatio = ("QPFtoFFGRatio", SCALAR, "1", "QPF to FFG Ratio", 1000.0, 0.0, 2, NO)

# Hazards
HazardKeys = []
HazardKeys.append(("<None>", ""))  #1st one must be None
import VTECTable
kys = VTECTable.VTECTable.keys()
kys.sort()
for k in kys:
    HazardKeys.append((k, VTECTable.VTECTable[k]['hdln']))

#H-VTEC keys - will someday add these back in
#("hydroER", "Hydro - Excessive Rainfall"),
#("hydroSM", "Hydro - Snow melt"),
#("hydroRS", "Rain and Snow melt"),
#("hydroDM", "Dam or Levee Failure"),
#("hydroGO", "Glacier-Dammed Lake Outburst"),
#("hydroIJ", "Ice Jam"),
#("hydroIC", "Rain and/or Snow melt and/or Ice Jam"),

Hazards = ("Hazards", DISCRETE, "wwa", "Hazards", YES, HazardKeys, 4)

# Scalar/Vector Weather Elements that Require Extra Precision (due to their
# use in calculations) Either form may be used.
ExtraWEPrecision = []

# Parms for ESTOFS
AstroTide = ("AstroTide", SCALAR, "ft", "Astro Tide", 20.0, -8.0, 1, NO)
StormSurge = ("StormSurge", SCALAR, "ft", "Storm Surge", 30.0, -5.0, 1, NO)

# Parms for ETSS and ETSSHiRes
SurgeTide = ("SurgeTide", SCALAR, "ft", "Surge Tide", 20.0, -8.0, 1, NO)

# Parm for Aviation/GFSLAMPGrid
CigHgt=("CigHgt",SCALAR,"ft","Ceiling Height",25000.0,-100.0,0,NO)

# Parms for NationalBlend
QPF1=("QPF1", SCALAR, "in", "1HR QPF", maxQpfVal, 0.0, 2, YES)
PPI01=('PPI01', SCALAR, '%', '1-H Precip Potential Index', 100.0, 0.0, 0, NO)
PPI06=('PPI06', SCALAR, '%', '6-H Precip Potential Index', 100.0, 0.0, 0, NO)
PositiveEnergyAloft=("PositiveEnergyAloft" , SCALAR, "j/kg", "Positive energy aloft" , 500.0, 0.0, 1, NO)
NegativeEnergyLowLevel=("NegativeEnergyLowLevel" , SCALAR, "j/kg", "Negative energy in the low levels" , 0.0, -500.0, 1, NO)
PoTIP=('PotSleet', SCALAR, '%', 'Prob of Sleet', 100.0, 0.0, 0, NO)
PoTR=('PotRain', SCALAR, '%', 'Prob of Rain', 100.0, 0.0, 0, NO)
PoTS=('PotSnow', SCALAR, '%', 'Prob of Snow', 100.0, 0.0, 0, NO)
PoTZR=('PotFreezingRain', SCALAR, '%', 'Prob of Freezing Rain', 100.0, 0.0, 0, NO)
MaxTwAloft=("MaxTwAloft", SCALAR, 'C', 'Max Wet-Bulb Temp in Warm Nose', 40.0, -20.0, 1, NO)
ProbIcePresent=("ProbIcePresent", SCALAR, "%", "Prob of Ice Present", 100.0, 0.0, 0, NO)
ProbRefreezeSleet=("ProbRefreezeSleet", SCALAR, "%", "Prob of Refreeze into Sleet", 100.0, 0.0, 0, NO)



#---------------------------------------------------------------------------
#
#  Weather configuration section
#
#---------------------------------------------------------------------------

# list of possible visibilities
visibilities = ['<NoVis>', '0SM', '1/4SM', '1/2SM', '3/4SM', '1SM', '11/2SM',
                '2SM', '21/2SM', '3SM', '4SM', '5SM', '6SM', 'P6SM']

# list of possible coverages and probabilities
NOCOV = ('<NoCov>', 'No Coverage')
ISOD = ('Iso', 'Isolated')
SCT = ('Sct', 'Scattered')
NUM = ('Num', 'Numerous')
WIDE = ('Wide', 'Widespread')
OCNL = ('Ocnl', 'Occasional')
SCHC = ('SChc', 'Slight Chance Of')
CHC = ('Chc', 'Chance Of')
LKLY = ('Lkly', 'Likely')
DEFN = ('Def', 'Definite')
PATCHY = ('Patchy', 'Patchy')
AREAS = ('Areas', 'Areas of')
FQT = ('Frq', 'Frequent')
BRIEF = ('Brf', 'Brief')
PERIODS = ('Pds', 'Periods of')
INTM = ('Inter', 'Intermittent')

# list of possible intensities
INTEN_NONE = ('<NoInten>', 'No intensity')
INTEN_VERYLIGHT = ('--', 'Very Light')
INTEN_LIGHT = ('-', 'Light')
INTEN_MOD = ('m', 'Moderate')
INTEN_HEAVY = ('+', 'Heavy')
INTEN_SEVERE = ('+', 'Severe')
INTEN_DENSE = ('+', 'Dense')

# list of optional attributes
FQTLTG = ('FL', 'Frequent Lightning')
GUSTS = ('GW', 'Gusty Winds')
HVYRAFL = ('HvyRn', 'Heavy Rainfall')
DMGWND = ('DmgW', 'Damaging Winds')
SMALLH = ('SmA', 'Small Hail')
LARGEH = ('LgA', 'Large Hail')
OUTLYNG = ('OLA','in the outlying areas')
GRASSY  = ('OGA','on grassy areas')
OVRPASS = ('OBO','on bridges and overpasses')
OR = ('OR', 'or')
DRY = ('Dry', 'dry')
PRIMARY = ('Primary', 'Highest Ranking')
MENTION = ('Mention', 'Include Unconditionally')
TORNADO = ('TOR', 'Tornadoes')

# list of each weather types
NOWX = ('<NoWx>', 'No Weather',
          [NOCOV],
          [INTEN_NONE],
          [])
THUNDER = ('T', 'Thunderstorms',
          [ISOD, SCT, NUM, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF,
            PERIODS, INTM],
          [INTEN_NONE, INTEN_SEVERE],
          [PRIMARY, MENTION, FQTLTG, HVYRAFL, GUSTS, DMGWND, DRY,
            LARGEH, SMALLH, TORNADO])
RAIN = ('R', 'Rain',
          [WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
RAINSHOWERS = ('RW', 'Rain Showers',
          [ISOD, SCT, NUM, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF,
            PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
DRIZZLE = ('L', 'Drizzle',
          [PATCHY, AREAS, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT,
            BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
FZRAIN = ('ZR', 'Freezing Rain',
          [WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
FZDRIZZLE = ('ZL', 'Freezing Drizzle',
          [PATCHY, AREAS, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT,
            BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
SNOW = ('S', 'Snow',
          [WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
SNOWSHOWERS = ('SW', 'Snow Showers',
          [ISOD, SCT, NUM, WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT,
            BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
SLEET = ('IP', 'Sleet',
          [WIDE, SCHC, CHC, LKLY, DEFN, OCNL, FQT, BRIEF, PERIODS, INTM],
          [INTEN_VERYLIGHT, INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION, OR])
FOG = ('F', 'Fog',
          [PATCHY, AREAS, WIDE],
          [INTEN_NONE, INTEN_DENSE],
          [PRIMARY, MENTION])
FREEZEFOG = ('ZF', 'Freezing Fog',
          [PATCHY, AREAS, WIDE],
          [INTEN_NONE, INTEN_DENSE],
          [PRIMARY, MENTION])
ICEFOG = ('IF', 'Ice Fog',
          [PATCHY, AREAS, WIDE],
          [INTEN_NONE],
          [PRIMARY, MENTION])
ICECRYSTAL = ('IC', 'Ice Crystals',
          [PATCHY, AREAS, WIDE],
          [INTEN_NONE],
          [PRIMARY, MENTION])
HAZE = ('H', 'Haze',
          [DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
BLWGSNOW = ('BS', 'Blowing Snow',
          [PATCHY, AREAS, DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
BLWGSAND = ('BN', 'Blowing Sand',
          [PATCHY, AREAS, DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
SMOKE = ('K', 'Smoke',
          [PATCHY, AREAS, DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
BLWGDUST = ('BD', 'Blowing Dust',
          [PATCHY, AREAS, DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])
FROST = ('FR','Frost',
          [PATCHY, AREAS, WIDE],
          [INTEN_NONE],
          [PRIMARY, MENTION, OUTLYNG])
FRZSPRAY = ('ZY','Freezing Spray',
          [ISOD, SCT, NUM, WIDE, SCHC, CHC, LKLY, DEFN, OCNL],
          [INTEN_LIGHT, INTEN_MOD, INTEN_HEAVY],
          [PRIMARY, MENTION])
VOLASH = ('VA','Volcanic Ash',
          [NOCOV],
          [INTEN_NONE],
          [PRIMARY, MENTION])
WATERSPOUT = ('WP','Waterspouts',
          [ISOD, SCHC, CHC, LKLY, DEFN],
          [INTEN_NONE],
          [PRIMARY, MENTION])


types = [NOWX, THUNDER, WATERSPOUT, RAIN, RAINSHOWERS,
         DRIZZLE, FZRAIN, FZDRIZZLE, SNOW, SNOWSHOWERS,
         SLEET, FOG, FREEZEFOG, ICEFOG, ICECRYSTAL ,HAZE, BLWGSNOW,
         BLWGSAND, SMOKE, BLWGDUST, FROST, FRZSPRAY, VOLASH]


# PARMS FROM NwsInitsConfig
#-------------------------------------------------------------------------------
# Discrete Keys
#-------------------------------------------------------------------------------
#
AirKeys = [("<None>","None"), ("Watch","Watch"), ("Advisory","Advisory"),("Warning", "Warning"),]
ThreatKeys=[('<None>', 'None'), ('Very Low', 'Very Low'), ('Low', 'Low'),
            ('Moderate', 'Moderate'), ('High', 'High'), ('Extreme','Extreme')]
#
SevereKeys = [('NONE', '0'), ('TSTM', '2'), ('MRGL', '3'), ('SLGT', '4'), ('ENH', '5'), ('MOD', '6'), ('HIGH', '8')]

AirQuality = ('AirQuality', DISCRETE, 'cat', 'Air Quality', NO, AirKeys)
BasinFFP = ('BasinFFP', DISCRETE, 'none', 'Basin Flash Flood Potential', NO,
                         [('Dry', 'Dry'), ('Low', 'Low'), ('Moderate', 'Moderate'), ('High', 'High'), ('Very High', 'Very High')])
CLRIndx = ('CLRIndx', SCALAR, 'none', 'Clearing Index', 1050.0, 0.0, 0, NO)
CQPF1 = ('CQPF1', SCALAR, 'in', '6hr Cont QPF', maxQpfVal, 0.0, 2, NO)
Ceiling = ('Ceiling', SCALAR, 'ft', 'Lowest Cloud Base Height', 25000.0, -30000.0, 0, NO)
CigHgtCat = ('CigHgtCat', SCALAR, 'index', 'Cloud Ceiling Height Category', 6.0, 0.0, 0, NO)
CloudBaseConditional = ('CloudBaseConditional', SCALAR, '100ft', 'Conditional Cloud Base Height', 250.0, 0.0, 0, NO)
CloudBasePrimary = ('CloudBasePrimary', SCALAR, '100ft', 'Primary Cloud Base Height', 250.0, 0.0, 0, NO)
CloudBaseSecondary = ('CloudBaseSecondary', SCALAR, '100ft', 'Secondary Cloud Base Height', 250.0, 0.0, 0, NO)
ClimoET = ('ClimoET', SCALAR, 'in', 'ClimoET', 0.75, 0.0, 2, NO)
ClimoETAprA = ('ClimoETAprA', SCALAR, 'in', 'ClimoET AprA', 0.75, 0.0, 2, NO)
ClimoETAprB = ('ClimoETAprB', SCALAR, 'in', 'ClimoET AprB', 0.75, 0.0, 2, NO)
ClimoETAugA = ('ClimoETAugA', SCALAR, 'in', 'ClimoET AugA', 0.75, 0.0, 2, NO)
ClimoETAugB = ('ClimoETAugB', SCALAR, 'in', 'ClimoET AugB', 0.75, 0.0, 2, NO)
ClimoETDecA = ('ClimoETDecA', SCALAR, 'in', 'ClimoET DecA', 0.75, 0.0, 2, NO)
ClimoETDecB = ('ClimoETDecB', SCALAR, 'in', 'ClimoET DecB', 0.75, 0.0, 2, NO)
ClimoETFebA = ('ClimoETFebA', SCALAR, 'in', 'ClimoET FebA', 0.75, 0.0, 2, NO)
ClimoETFebB = ('ClimoETFebB', SCALAR, 'in', 'ClimoET FebB', 0.75, 0.0, 2, NO)
ClimoETJanA = ('ClimoETJanA', SCALAR, 'in', 'ClimoET JanA', 0.75, 0.0, 2, NO)
ClimoETJanB = ('ClimoETJanB', SCALAR, 'in', 'ClimoET JanB', 0.75, 0.0, 2, NO)
ClimoETJulA = ('ClimoETJulA', SCALAR, 'in', 'ClimoET JulA', 0.75, 0.0, 2, NO)
ClimoETJulB = ('ClimoETJulB', SCALAR, 'in', 'ClimoET JulB', 0.75, 0.0, 2, NO)
ClimoETJunA = ('ClimoETJunA', SCALAR, 'in', 'ClimoET JunA', 0.75, 0.0, 2, NO)
ClimoETJunB = ('ClimoETJunB', SCALAR, 'in', 'ClimoET JunB', 0.75, 0.0, 2, NO)
ClimoETMarA = ('ClimoETMarA', SCALAR, 'in', 'ClimoET MarA', 0.75, 0.0, 2, NO)
ClimoETMarB = ('ClimoETMarB', SCALAR, 'in', 'ClimoET MarB', 0.75, 0.0, 2, NO)
ClimoETMayA = ('ClimoETMayA', SCALAR, 'in', 'ClimoET MayA', 0.75, 0.0, 2, NO)
ClimoETMayB = ('ClimoETMayB', SCALAR, 'in', 'ClimoET MayB', 0.75, 0.0, 2, NO)
ClimoETNovA = ('ClimoETNovA', SCALAR, 'in', 'ClimoET NovA', 0.75, 0.0, 2, NO)
ClimoETNovB = ('ClimoETNovB', SCALAR, 'in', 'ClimoET NovB', 0.75, 0.0, 2, NO)
ClimoETOctA = ('ClimoETOctA', SCALAR, 'in', 'ClimoET OctA', 0.75, 0.0, 2, NO)
ClimoETOctB = ('ClimoETOctB', SCALAR, 'in', 'ClimoET OctB', 0.75, 0.0, 2, NO)
ClimoETSepA = ('ClimoETSepA', SCALAR, 'in', 'ClimoET SepA', 0.75, 0.0, 2, NO)
ClimoETSepB = ('ClimoETSepB', SCALAR, 'in', 'ClimoET SepB', 0.75, 0.0, 2, NO)
ClimoPoP = ('ClimoPoP', SCALAR, '%', 'ClimoPoP', 100.0, 0.0, 0, NO)
ClimoPoPAprA = ('ClimoPoPAprA', SCALAR, '%', 'ClimoPoP AprA', 100.0, 0.0, 0, NO)
ClimoPoPAprB = ('ClimoPoPAprB', SCALAR, '%', 'ClimoPoP AprB', 100.0, 0.0, 0, NO)
ClimoPoPAugA = ('ClimoPoPAugA', SCALAR, '%', 'ClimoPoP AugA', 100.0, 0.0, 0, NO)
ClimoPoPAugB = ('ClimoPoPAugB', SCALAR, '%', 'ClimoPoP AugB', 100.0, 0.0, 0, NO)
ClimoPoPDecA = ('ClimoPoPDecA', SCALAR, '%', 'ClimoPoP DecA', 100.0, 0.0, 0, NO)
ClimoPoPDecB = ('ClimoPoPDecB', SCALAR, '%', 'ClimoPoP DecB', 100.0, 0.0, 0, NO)
ClimoPoPFG = ('ClimoPoPFG', SCALAR, '%', 'ClimoPoP First Guess', 100.0, 0.0, 0, NO)
ClimoPoPFebA = ('ClimoPoPFebA', SCALAR, '%', 'ClimoPoP FebA', 100.0, 0.0, 0, NO)
ClimoPoPFebB = ('ClimoPoPFebB', SCALAR, '%', 'ClimoPoP FebB', 100.0, 0.0, 0, NO)
ClimoPoPJanA = ('ClimoPoPJanA', SCALAR, '%', 'ClimoPoP JanA', 100.0, 0.0, 0, NO)
ClimoPoPJanB = ('ClimoPoPJanB', SCALAR, '%', 'ClimoPoP JanB', 100.0, 0.0, 0, NO)
ClimoPoPJulA = ('ClimoPoPJulA', SCALAR, '%', 'ClimoPoP JulA', 100.0, 0.0, 0, NO)
ClimoPoPJulB = ('ClimoPoPJulB', SCALAR, '%', 'ClimoPoP JulB', 100.0, 0.0, 0, NO)
ClimoPoPJunA = ('ClimoPoPJunA', SCALAR, '%', 'ClimoPoP JunA', 100.0, 0.0, 0, NO)
ClimoPoPJunB = ('ClimoPoPJunB', SCALAR, '%', 'ClimoPoP JunB', 100.0, 0.0, 0, NO)
ClimoPoPMarA = ('ClimoPoPMarA', SCALAR, '%', 'ClimoPoP MarA', 100.0, 0.0, 0, NO)
ClimoPoPMarB = ('ClimoPoPMarB', SCALAR, '%', 'ClimoPoP MarB', 100.0, 0.0, 0, NO)
ClimoPoPMayA = ('ClimoPoPMayA', SCALAR, '%', 'ClimoPoP MayA', 100.0, 0.0, 0, NO)
ClimoPoPMayB = ('ClimoPoPMayB', SCALAR, '%', 'ClimoPoP MayB', 100.0, 0.0, 0, NO)
ClimoPoPNovA = ('ClimoPoPNovA', SCALAR, '%', 'ClimoPoP NovA', 100.0, 0.0, 0, NO)
ClimoPoPNovB = ('ClimoPoPNovB', SCALAR, '%', 'ClimoPoP NovB', 100.0, 0.0, 0, NO)
ClimoPoPOctA = ('ClimoPoPOctA', SCALAR, '%', 'ClimoPoP OctA', 100.0, 0.0, 0, NO)
ClimoPoPOctB = ('ClimoPoPOctB', SCALAR, '%', 'ClimoPoP OctB', 100.0, 0.0, 0, NO)
ClimoPoPSepA = ('ClimoPoPSepA', SCALAR, '%', 'ClimoPoP SepA', 100.0, 0.0, 0, NO)
ClimoPoPSepB = ('ClimoPoPSepB', SCALAR, '%', 'ClimoPoP SepB', 100.0, 0.0, 0, NO)
CoastalFlood = ('CoastalFlood', DISCRETE, 'cat', 'Coastal Flood', NO, ThreatKeys)
CondPredHgt = ('CondPredHgt', SCALAR, '100ft', 'Conditional Predominant Cloud Height', 250.0, 0.0, 0, NO)
CondPredVsby = ('CondPredVsby', SCALAR, 'mi', 'Conditional Predominant Visibility', 10.0, 0.0, 2, NO)
DenseFogSmoke = ('DenseFogSmoke', DISCRETE, 'cat', 'Dense Fog', NO, ThreatKeys)
DepartNormFRET = ('DepartNormFRET', SCALAR, 'in', 'DepartNormFRET', 0.35, -0.35, 2, NO)
Dryness = ('Dryness', DISCRETE, 'none', 'EGB Fuel Dryness', NO,
           [('NoData', 'NoData'), ('Moist', 'Moist'), ('Dry', 'Dry'), ('VeryDry', 'VeryDry')])
ExcessiveCold = ('ExcessiveCold', DISCRETE, 'cat', 'Extreme Cold', NO, ThreatKeys)
ExcessiveHeat = ('ExcessiveHeat', DISCRETE, 'cat', 'Excessive Heat', NO, ThreatKeys)
FFP = ('FFP', DISCRETE, 'none', 'Flash Flood Potential', NO,
       [('Dry', 'Dry'), ('Low', 'Low'), ('Moderate', 'Moderate'), ('High', 'High'), ('Very High', 'Very High')])
FFPI = ('FFPI', SCALAR, 'index', 'Flash Flood Potential Index', 10.0, 0.0, 2, NO)
FRET = ('FRET', SCALAR, 'in', 'Forecast Reference ET', 0.75, 0.0, 2, NO)
FRET7Day = ('FRET7Day', SCALAR, 'in/week', 'Weekly Forecast Reference ET', 5.0, 0.0, 2, NO)
FireWeather = ('FireWeather', DISCRETE, 'cat', 'Wild Fire', NO, ThreatKeys)
FlashFlood = ('FlashFlood', DISCRETE, 'cat', 'Flash Flood', NO, ThreatKeys)
Flood = ('Flood', DISCRETE, 'cat', 'River Flood', NO, ThreatKeys)
FrostFreeze = ('FrostFreeze', DISCRETE, 'cat', 'Frost/Freeze', NO, ThreatKeys)
FuelMstr = ('FuelMstr', SCALAR, 'none', '10 Hour Fuel Moisture', 40.0, 1.0, 0, NO)
HainesMid = ('HainesMid', SCALAR, 'cat', 'Mid Level Haines Index', 6.0, 2.0, 0, NO)
HeatImpactLevels = ('HeatImpactLevels', SCALAR, 'none', 'HeatImpactLevels', 4.0, 0.0, 0, NO)
HeatImpactLevelsMaxT = ('HeatImpactLevelsMaxT', SCALAR, 'none', 'HeatImpactLevelsMaxT', 4.0, 0.0, 0, NO)
HeatImpactLevelsMinT = ('HeatImpactLevelsMinT', SCALAR, 'none', 'HeatImpactLevelsMinT', 4.0, 0.0, 0, NO)
HeatOrangeMaxT = ('HeatOrangeMaxT', SCALAR, 'F', 'Heat Orange MaxT', maxTempVal, minTempVal, 0, NO)
HeatOrangeMinT = ('HeatOrangeMinT', SCALAR, 'F', 'Heat Orange MinT', maxTempVal, minTempVal, 0, NO)
HeatRedMaxT = ('HeatRedMaxT', SCALAR, 'F', 'Heat Red MaxT', maxTempVal, minTempVal, 0, NO)
HeatRedMinT = ('HeatRedMinT', SCALAR, 'F', 'Heat Red MinT', maxTempVal, minTempVal, 0, NO)
HeatYellowMaxT = ('HeatYellowMaxT', SCALAR, 'F', 'Heat Yellow MaxT', maxTempVal, minTempVal, 0, NO)
HeatYellowMinT = ('HeatYellowMinT', SCALAR, 'F', 'Heat Yellow MinT', maxTempVal, minTempVal, 0, NO)
HighWind = ('HighWind', DISCRETE, 'cat', 'High Wind', NO, ThreatKeys)
IceAccum6hr = ('IceAccum6hr', SCALAR, 'in', '6-hr Ice Accumulation', 2.0, 0.0, 2, NO)
LLWS = ('LLWS', VECTOR, 'kts', 'Low Level Wind Shear', 125.0, 0.0, 0, NO)
LLWSHgt = ('LLWSHgt', SCALAR, '100 ft', 'Wind Shear Height', 20.0, 0.0, 0, NO)
LTG = ('LTG', SCALAR, 'CNT', 'LTG', 100.0, 0.0, 0, NO)
LTG12 = ('LTG12', SCALAR, 'CNT', 'LTG12', 100.0, 0.0, 0, NO)
LTG24 = ('LTG24', SCALAR, 'CNT', 'LTG24', 100.0, 0.0, 0, NO)
Lightning = ('Lightning', DISCRETE, 'cat', 'Lightning', NO, ThreatKeys)
Max3 = ('Max3', SCALAR, 'F', '3hr Maximum Temperature', maxTempVal, minTempVal, 0, NO)
Max6 = ('Max6', SCALAR, 'F', '6hr Maximum Temperature', maxTempVal, minTempVal, 0, NO)
MaxApT = ('MaxApT', SCALAR, 'F', 'Max Apparent Temperature', maxTempVal, -120.0, 0, NO)
MaxRHError = ('MaxRHError', SCALAR, '%', 'Maximum Relative Humidity Error', 100.0, -100.0, 0, NO)
MaxRHFcst = ('MaxRHFcst', SCALAR, '%', 'Forecast Maximum Relative Humidity', 100.0, 0.0, 0, NO)
MaxRHOb = ('MaxRHOb', SCALAR, '%', 'Observed Maximum Relative Humidity', 100.0, 0.0, 0, NO)
MaxRHObs = ('MaxRHObs', SCALAR, '%', 'Maximum Observed RH', 100.0, 0.0, 0, NO)
MaxT10 = ('MaxT10', SCALAR, 'F', '10th Percentile for MaxT', maxTempVal, minTempVal, 0, NO)
MaxT50 = ('MaxT50', SCALAR, 'F', '50th Percentile for MaxT', maxTempVal, minTempVal, 0, NO)
MaxT90 = ('MaxT90', SCALAR, 'F', '90th Percentile for MaxT', maxTempVal, minTempVal, 0, NO)
MaxTAloft = ('MaxTAloft', SCALAR, 'C', 'Max Temp in Warm Nose', 40.0, -20.0, 1, NO)
MaxTError = ('MaxTError', SCALAR, 'F', 'Maximum Temperature Error', 120.0, -120.0, 0, NO)
MaxTFcst = ('MaxTFcst', SCALAR, 'F', 'Observed Maximum Temperature', maxTempVal, minTempVal, 0, NO)
MaxTOb = ('MaxTOb', SCALAR, 'F', 'Observed Maximum Temperature', maxTempVal, minTempVal, 0, NO)
MaxTObs = ('MaxTObs', SCALAR, 'F', 'Maximum Temperature Obs', maxTempVal, minTempVal, 0, NO)
Min3 = ('Min3', SCALAR, 'F', '3hr Minimum Temperature', maxTempVal, minTempVal, 0, NO)
Min6 = ('Min6', SCALAR, 'F', '6hr Minimum Temperature', maxTempVal, minTempVal, 0, NO)
MinApT = ('MinApT', SCALAR, 'F', 'Min Apparent Temperature', maxTempVal, -120.0, 0, NO)
MinRH3 = ('MinRH3', SCALAR, '%', '3hr Minimum Relative Humidity', 100.0, 0.0, 0, NO)
MinRHError = ('MinRHError', SCALAR, '%', 'Minimum Relative Humidity Error', 100.0, -100.0, 0, NO)
MinRHFcst = ('MinRHFcst', SCALAR, '%', 'Forecast Minimum Relative Humidity', 100.0, 0.0, 0, NO)
MinRHOb = ('MinRHOb', SCALAR, '%', 'Observed Minimum Relative Humidity', 100.0, 0.0, 0, NO)
MinRHObs = ('MinRHObs', SCALAR, '%', 'Minimum Observed RH', 100.0, 0.0, 0, NO)
MinT10 = ('MinT10', SCALAR, 'F', '10th Percentile for MinT', maxTempVal, minTempVal, 0, NO)
MinT50 = ('MinT50', SCALAR, 'F', '50th Percentile for MinT', maxTempVal, minTempVal, 0, NO)
MinT6 = ('MinT6', SCALAR, 'F', 'Minimum Temperature 6Hr', maxTempVal, minTempVal, 0, NO)
MinT90 = ('MinT90', SCALAR, 'F', '90th Percentile for MinT', maxTempVal, minTempVal, 0, NO)
MinTError = ('MinTError', SCALAR, 'F', 'Minimum Temperature Error', 120.0, -120.0, 0, NO)
MinTFcst = ('MinTFcst', SCALAR, 'F', 'Forecast Minimum Temperature', maxTempVal, minTempVal, 0, NO)
MinTOb = ('MinTOb', SCALAR, 'F', 'Observed Minimum Temperature', maxTempVal, minTempVal, 0, NO)
MinTObs = ('MinTObs', SCALAR, 'F', 'Minimum Temperature Obs', maxTempVal, minTempVal, 0, NO)
MixHgtAve = ('MixHgtAve', SCALAR, 'ft', 'Mixing Hgt Average', 20000.0, 0.0, 0, NO)
MixHgtMSL = ('MixHgtMSL', SCALAR, 'ft', 'Mixing Height above sea level', 30000.0, 0.0, 0, NO)
MixT1700 = ('MixT1700', SCALAR, 'F', '1700Foot MixingTemp', 110.0, -10.0, 0, NO)
P95MaxT = ('P95MaxT', SCALAR, 'F', 'P95MaxT', maxTempVal, minTempVal, 0, NO)
P95MinT = ('P95MinT', SCALAR, 'F', 'P95MinT', maxTempVal, minTempVal, 0, NO)
     # EKDMOS
PQPF06001 = ('PQPF06001', SCALAR, '%', '6hr Prob QPF > 0.01', 100.0, 0.0, 0, NO)
PQPF06005 = ('PQPF06005', SCALAR, '%', '6hr Prob QPF > 0.05', 100.0, 0.0, 0, NO)
PQPF06010 = ('PQPF06010', SCALAR, '%', '6hr Prob QPF > 0.10', 100.0, 0.0, 0, NO)
PQPF06015 = ('PQPF06015', SCALAR, '%', '6hr Prob QPF > 0.15', 100.0, 0.0, 0, NO)
PQPF06020 = ('PQPF06020', SCALAR, '%', '6hr Prob QPF > 0.20', 100.0, 0.0, 0, NO)
PQPF06025 = ('PQPF06025', SCALAR, '%', '6hr Prob QPF > 0.25', 100.0, 0.0, 0, NO)
PQPF06030 = ('PQPF06030', SCALAR, '%', '6hr Prob QPF > 0.30', 100.0, 0.0, 0, NO)
PQPF06040 = ('PQPF06040', SCALAR, '%', '6hr Prob QPF > 0.40', 100.0, 0.0, 0, NO)
PQPF06050 = ('PQPF06050', SCALAR, '%', '6hr Prob QPF > 0.50', 100.0, 0.0, 0, NO)
PQPF06075 = ('PQPF06075', SCALAR, '%', '6hr Prob QPF > 0.75', 100.0, 0.0, 0, NO)
PQPF06100 = ('PQPF06100', SCALAR, '%', '6hr Prob QPF > 1.00', 100.0, 0.0, 0, NO)
PQPF06150 = ('PQPF06150', SCALAR, '%', '6hr Prob QPF > 1.50', 100.0, 0.0, 0, NO)
PoP12Fcst = ('PoP12Fcst', SCALAR, '%', 'Forecast Prob. of Precip.', 100.0, 0.0, 0, NO)
PoP3 = ('PoP3', SCALAR, '%', 'PoP3', 100.0, 0.0, 0, NO)
PoPPCECMWF = ('PoPPatternClimoECMWF', SCALAR, '%', 'PatternClimoECMWF', 100.0, 0.0, 0, NO)
PoPPCFIM = ('PoPPatternClimoFIM', SCALAR, '%', 'PatternClimoFIM', 100.0, 0.0, 0, NO)
PoPPCGEM = ('PoPPatternClimoGEM', SCALAR, '%', 'PatternClimoGEM', 100.0, 0.0, 0, NO)
PoPPCGFS = ('PoPPatternClimoGFS', SCALAR, '%', 'PatternClimoGFS', 100.0, 0.0, 0, NO)
PoPPattern1 = ('PoPNortherlyFlow', SCALAR, '%', 'NortherlyFlow', 100.0, 0.0, 0, NO)
PoPPattern10 = ('PoPRockiesRidge', SCALAR, '%', 'RockiesRidge', 100.0, 0.0, 0, NO)
PoPPattern11 = ('PoPSouthernFirehose', SCALAR, '%', 'SouthernFirehose', 100.0, 0.0, 0, NO)
PoPPattern12 = ('PoPNorthernFirehose', SCALAR, '%', 'NorthernFirehose', 100.0, 0.0, 0, NO)
PoPPattern2 = ('PoPGreatBasinLow', SCALAR, '%', 'GreatBasinLow', 100.0, 0.0, 0, NO)
PoPPattern3 = ('PoPBroadCyclonicFlow', SCALAR, '%', 'BroadCyclonicFlow', 100.0, 0.0, 0, NO)
PoPPattern4 = ('PoPCoastalRidge', SCALAR, '%', 'CoastalRidge', 100.0, 0.0, 0, NO)
PoPPattern5 = ('PoPNorthwestFlow', SCALAR, '%', 'NorthwestFlow', 100.0, 0.0, 0, NO)
PoPPattern6 = ('PoPZonalFlow', SCALAR, '%', 'ZonalFlow', 100.0, 0.0, 0, NO)
PoPPattern7 = ('PoPBroadAntiCyclonicFlow', SCALAR, '%', 'BroadAntiCyclonicFlow', 100.0, 0.0, 0, NO)
PoPPattern8 = ('PoPDiffluentOnshoreFlow', SCALAR, '%', 'DiffluentOnshoreFlow', 100.0, 0.0, 0, NO)
PoPPattern9 = ('PoPSouthwestFlow', SCALAR, '%', 'SouthwestFlow', 100.0, 0.0, 0, NO)
PoPWG = ('PoPWG', SCALAR, '%', 'Climo PoP Work Grid', 30.0, -30.0, 0, NO)
PPFFG = ("PPFFG", SCALAR, "%", "Prob of Excessive Rain in %", 100.0, 0.0 ,0, NO)
PrecipDur = ('PrecipDur', SCALAR, 'hrs', 'Precipitation Duration', 12.0, 0.0, 1, YES)
PredHgt = ('PredHgt', SCALAR, '100ft', 'Predominant Cloud Height', 250.0, 0.0, 0, NO)
PredHgtCat = ('PredHgtCat', SCALAR, 'index', 'Predominant Cloud Height Category', 6.0, 0.0, 0, NO)
PredHgtRH = ('PredHgtRH', SCALAR, '100ft', 'Pred Cloud Height from RH', 250.0, 1.0, 0, NO)
PredHgtTempo = ('PredHgtTempo', SCALAR, '100ft', 'Predominant Cloud Height Tempo', 250.0, 0.0, 0, NO)
PredVsby = ('PredVsby', SCALAR, 'mi', 'Predominant Visibility', 10.0, 0.0, 2, NO)
Pres = ('Pres', SCALAR, 'mb', 'Pressure', 1100.0, 900.0, 2, NO)
ProbDmgWind = ('ProbDmgWind', SCALAR, '%', 'Probability of Damaging Wind', 100.0, 0.0, 0, NO)
ProbExtrmDmgWind = ('ProbExtrmDmgWind', SCALAR, '%', 'Probability of Extreme Damaging Wind', 100.0, 0.0, 0, NO)
ProbExtrmHail = ('ProbExtrmHail', SCALAR, '%', 'Probability of Extreme Hail', 100.0, 0.0, 0, NO)
ProbExtrmSvr = ('ProbExtrmSvr', SCALAR, '%', 'Probability of Extreme Severe', 100.0, 0.0, 0, NO)
ProbExtrmTor = ('ProbExtrmTor', SCALAR, '%', 'Probability of Extreme Tornado', 100.0, 0.0, 0, NO)
ProbSvrHail = ('ProbSvrHail', SCALAR, '%', 'Probability of Severe Hail', 100.0, 0.0, 0, NO)
ProbTor = ('ProbTor', SCALAR, '%', 'Probability of Tornado', 100.0, 0.0, 0, NO)
ProbTotSvr = ('ProbTotSvr', SCALAR, '%', 'Probability of Severe', 100.0, 0.0, 0, NO)
ProbSnowGTT = ("ProbSnowGTT", SCALAR, "%", "Prob. snow > trace", 100.0, 0.0, 0, NO)
ProbSnowGT1 = ("ProbSnowGT1", SCALAR, "%", "Prob. snow > 1 inch", 100.0, 0.0, 0, NO)
ProbSnowGT2 = ("ProbSnowGT2", SCALAR, "%", "Prob. snow > 2 inches ", 100.0, 0.0, 0, NO)
ProbSnowGT4 = ("ProbSnowGT4", SCALAR, "%", "Prob. snow > 4 inches ", 100.0, 0.0, 0, NO)
ProbSnowGT6 = ("ProbSnowGT6", SCALAR, "%", "Prob. snow > 6 inches ", 100.0, 0.0, 0, NO)
ProbSnowGT8 = ("ProbSnowGT8", SCALAR, "%", "Prob. snow > 8 inches", 100.0, 0.0, 0, NO)
ProbSnowGT12 = ("ProbSnowGT12", SCALAR, "%", "Prob. snow > 12 inches", 100.0, 0.0, 0, NO)
ProbSnowGT18 = ("ProbSnowGT18", SCALAR, "%", "Prob. snow > 18 inches", 100.0, 0.0, 0, NO)
ProbSnowRT1 = ("ProbSnowRT1", SCALAR, "%", "Prob. snow T-1 inch", 100.0, 0.0, 0, NO)
ProbSnowR12 = ("ProbSnowR12", SCALAR, "%", "Prob. snow 1-2 inches", 100.0, 0.0, 0, NO)
ProbSnowR24 = ("ProbSnowR24", SCALAR, "%", "Prob. snow 2-4 inches ", 100.0, 0.0, 0, NO)
ProbSnowR48 = ("ProbSnowR48", SCALAR, "%", "Prob. snow 4-8 inches ", 100.0, 0.0, 0, NO)
ProbSnowR812 = ("ProbSnowR812", SCALAR, "%", "Prob. snow 8-12 inches ", 100.0, 0.0, 0, NO)
ProbSnowR1218 = ("ProbSnowR1218", SCALAR, "%", "Prob. snow 12-18 inches", 100.0, 0.0, 0, NO)
ProbSnowR18 = ("ProbSnowR18", SCALAR, "%", "Prob. snow > 18 inches", 100.0, 0.0, 0, NO)
QPE06 = ('QPE06', SCALAR, 'in', 'QPE06', maxQpfVal, 0.0, 2, YES)
QPE06Ob = ('QPE06Ob', SCALAR, 'in', 'Observed Precip', 20.0, 0.0, 2, NO)
QPE12 = ('QPE12', SCALAR, 'in', 'QPE12', 15.0, 0.0, 2, YES)
QPE24 = ('QPE24', SCALAR, 'in', 'QPE24', 15.0, 0.0, 2, YES)
QPFDS = ('QPFDS', SCALAR, 'in', 'QPFDS', maxQpfVal, 0.0, 2, YES)
QPFFcst = ('QPFFcst', SCALAR, 'in', 'Forecast Precip.', 10.0, 0.0, 2, NO)
QPFPCECMWF = ('QPFPatternClimoECMWF', SCALAR, 'in', 'PatternClimoECMWF', maxQpfVal, 0.0, 2, NO)
QPFPCFIM = ('QPFPatternClimoFIM', SCALAR, 'in', 'PatternClimoFIM', maxQpfVal, 0.0, 2, NO)
QPFPCGEM = ('QPFPatternClimoGEM', SCALAR, 'in', 'PatternClimoGEM', maxQpfVal, 0.0, 2, NO)
QPFPCGFS = ('QPFPatternClimoGFS', SCALAR, 'in', 'PatternClimoGFS', maxQpfVal, 0.0, 2, NO)
QPFPattern1 = ('QPFNortherlyFlow', SCALAR, 'in', 'NortherlyFlow', maxQpfVal, 0.0, 2, NO)
QPFPattern10 = ('QPFRockiesRidge', SCALAR, 'in', 'RockiesRidge', maxQpfVal, 0.0, 2, NO)
QPFPattern11 = ('QPFSouthernFirehose', SCALAR, 'in', 'SouthernFirehose', maxQpfVal, 0.0, 2, NO)
QPFPattern12 = ('QPFNorthernFirehose', SCALAR, 'in', 'NorthernFirehose', maxQpfVal, 0.0, 2, NO)
QPFPattern2 = ('QPFGreatBasinLow', SCALAR, 'in', 'GreatBasinLow', maxQpfVal, 0.0, 2, NO)
QPFPattern3 = ('QPFBroadCyclonicFlow', SCALAR, 'in', 'BroadCyclonicFlow', maxQpfVal, 0.0, 2, NO)
QPFPattern4 = ('QPFCoastalRidge', SCALAR, 'in', 'CoastalRidge', maxQpfVal, 0.0, 2, NO)
QPFPattern5 = ('QPFNorthwestFlow', SCALAR, 'in', 'NorthwestFlow', maxQpfVal, 0.0, 2, NO)
QPFPattern6 = ('QPFZonalFlow', SCALAR, 'in', 'ZonalFlow', maxQpfVal, 0.0, 2, NO)
QPFPattern7 = ('QPFBroadAntiCyclonicFlow', SCALAR, 'in', 'BroadAntiCyclonicFlow', maxQpfVal, 0.0, 2, NO)
QPFPattern8 = ('QPFDiffluentOnshoreFlow', SCALAR, 'in', 'DiffluentOnshoreFlow', maxQpfVal, 0.0, 2, NO)
QPFPattern9 = ('QPFSouthwestFlow', SCALAR, 'in', 'SouthwestFlow', maxQpfVal, 0.0, 2, NO)
QPFPct = ('QPFPct', SCALAR, '%', 'QPFPct', 300.0, 0.0, 1, YES)
QPFPctMonthlyClimo = ('QPFPctMonthlyClimo', SCALAR, '%', 'QPF Pct Monthly PRISMClimo', 200.0, 0.0, 0, NO)
QPFRaw = ('QPFRaw', SCALAR, 'in', 'QPFRaw', maxQpfVal, 0.0, 2, YES)
QSE06 = ('QSE06', SCALAR, 'in', 'QSE06', 100.0, 0.0, 1, YES)
RipCurrent = ('RipCurrent', DISCRETE, 'cat', 'Rip Current', NO, ThreatKeys)
RipCurrentIndex = ('RipCurrentIndex', SCALAR, 'ft', 'Rip Current Index', 16.0, -1.0, 1, NO)
RipRisk = ("RipRisk", SCALAR, "none", "Rip Current Risk", 3.0, 0.0, 0, NO)
SPC12hrLP1 = ('SPC12hrLP1', SCALAR, '%', 'SPC 12HR Lightning Probability (1)', 100.0, 0.0, 0, NO)
SPC12hrLP10 = ('SPC12hrLP10', SCALAR, '%', 'SPC 12HR Lightning Probability (10)', 100.0, 0.0, 0, NO)
SPC12hrLP100 = ('SPC12hrLP100', SCALAR, '%', 'SPC 12HR Lightning Probability (100)', 100.0, 0.0, 0, NO)
SPC24hrLP1 = ('SPC24hrLP1', SCALAR, '%', 'SPC 24HR Lightning Probability (1)', 100.0, 0.0, 0, NO)
SPC24hrLP10 = ('SPC24hrLP10', SCALAR, '%', 'SPC 24HR Lightning Probability (10)', 100.0, 0.0, 0, NO)
SPC24hrLP100 = ('SPC24hrLP100', SCALAR, '%', 'SPC 24HR Lightning Probability (100)', 100.0, 0.0, 0, NO)
SPC3hrLP1 = ('SPC3hrLP1', SCALAR, '%', 'SPC 3HR Lightning Probability (1)', 100.0, 0.0, 0, NO)
SPC3hrLP10 = ('SPC3hrLP10', SCALAR, '%', 'SPC 3HR Lightning Probability (10)', 100.0, 0.0, 0, NO)
SPC3hrLP100 = ('SPC3hrLP100', SCALAR, '%', 'SPC 3HR Lightning Probability (100)', 100.0, 0.0, 0, NO)
SevereHail = ('SevereHail', DISCRETE, 'cat', 'Severe Hail', NO, ThreatKeys)
SevereTstmWind = ('SevereTstmWind', DISCRETE, 'cat', 'SevereTstmWind', NO, ThreatKeys)
SnowAmt10Prcntl = ('SnowAmt10Prcntl', SCALAR, 'in', 'min case', 50.0, 0.0, 1, NO)
SnowAmt50Prcntl = ('SnowAmt50Prcntl', SCALAR, 'in', 'avg case', 50.0, 0.0, 1, NO)
SnowAmt90Prcntl = ('SnowAmt90Prcntl', SCALAR, 'in', 'max case', 50.0, 0.0, 1, NO)
SnowDepth = ('SnowDepth', SCALAR, 'in', 'Snow Depth', 50.0, 0.0, 0, NO)
SnowRatioCLIMO = ('SnowRatioCLIMO', SCALAR, '%', 'Snow Ratio Climatology SON-DJF-MAM', 40.0, 0.0, 1, YES)
SnowRatioGFS = ('SnowRatioGFS', SCALAR, '%', 'Snow Ratio from GFS', 40.0, 0.0, 1, YES)
SnowRatioHPCMEAN = ('SnowRatioHPCMEAN', SCALAR, '%', 'Snow Ratio from HPC MEAN', 40.0, 0.0, 1, YES)
SnowRatioNAM = ('SnowRatioNAM', SCALAR, '%', 'Snow Ratio from NAM40', 40.0, 0.0, 1, YES)
T10 = ('T10', SCALAR, 'F', '10th Percentile for T', maxTempVal, minTempVal, 0, NO)
T50 = ('T50', SCALAR, 'F', '50th Percentile for T', maxTempVal, minTempVal, 0, NO)
T90 = ('T90', SCALAR, 'F', '90th Percentile for T', maxTempVal, minTempVal, 0, NO)
TAloft = ('TAloft', SCALAR, 'F', 'Temperature Aloft', 120.0, -50.0, 1, NO)
Td10 = ('Td10', SCALAR, 'F', '10th Percentile for DpT', maxTdVal, minTdVal, 0, NO)
Td50 = ('Td50', SCALAR, 'F', '50th Percentile for DpT', maxTdVal, minTdVal, 0, NO)
Td90 = ('Td90', SCALAR, 'F', '90th Percentile for DpT', maxTdVal, minTdVal, 0, NO)
TdAft = ('TdAft', SCALAR, 'F', 'Afternoon Dewpoint', maxTdVal, minTdVal, 0, NO)
TdAftError = ('TdAftError', SCALAR, 'F', 'Afternoon Dewpoint Error', 120.0, -120.0, 0, NO)
TdAftFcst = ('TdAftFcst', SCALAR, 'F', 'Forecast Afternoon Dewpoint', maxTdVal, minTdVal, 0, NO)
TdAftOb = ('TdAftOb', SCALAR, 'F', 'Observed Afternoon Dewpoint', maxTdVal, minTdVal, 0, NO)
TdAftObs = ('TdAftObs', SCALAR, 'F', 'Afternoon Dewpoint Obs', maxTdVal, minTdVal, 0, NO)
TdMrn = ('TdMrn', SCALAR, 'F', 'Morning Dewpoint', maxTdVal, minTdVal, 0, NO)
TdMrnError = ('TdMrnError', SCALAR, 'F', 'Morning Dewpoint Error', 120.0, -120.0, 0, NO)
TdMrnFcst = ('TdMrnFcst', SCALAR, 'F', 'Forecast Morning Dewpoint', maxTdVal, minTdVal, 0, NO)
TdMrnOb = ('TdMrnOb', SCALAR, 'F', 'Observed Morning Dewpoint', maxTdVal, minTdVal, 0, NO)
TdMrnObs = ('TdMrnObs', SCALAR, 'F', 'Morning Dewpoint Obs', maxTdVal, minTdVal, 0, NO)
Tornado = ('Tornado', DISCRETE, 'cat', 'Tornado', NO, ThreatKeys)
TransWindAve = ('TransWindAve', VECTOR, 'mph', 'Transport Wind Average', 125.0, 0.0, 0, NO)
Tw = ('Tw', SCALAR, 'F', 'Surface Wet Bulb Temp', 80.0, -50.0, 0, NO)
VentRateAve = ('VentRateAve', SCALAR, 'mph-ft', 'Vent Rate Average', 500000.0, 0.0, 0, NO)
Visibility = ('Visibility', SCALAR, 'SM', 'Visibility', 10.0, 0.0, 2, NO)
VisibilityConditional = ('VisibilityConditional', SCALAR, 'SM', 'Conditional Visibility', 10.0, 0.0, 2, NO)
Vsby = ('Vsby', SCALAR, 'mi', 'Visibility', 10.0, 0.0, 2, NO)
WG1 = ('WG1', SCALAR, 'none', 'WorkGrid1', 100.0, -100.0, 0, NO)
WinterWx = ('WinterWx', DISCRETE, 'cat', 'Winter Weather', NO, ThreatKeys)

#** Parameter sets for specific functionality
optionalParmsDict = {}

# Marine Weather Elements
optionalParmsDict['marine']={
    'WaveDir' : ("WaveDir", VECTOR, "m/s", "Wave Direction", 5.0, 0.0, 2, NO),
    'WindWaveHeight' : ("WindWaveHgt", SCALAR, "ft", "Wind Wave Height", 100.0, 0.0, 0, NO),
    'WaveHeight' : ("WaveHeight", SCALAR, "ft", "Total Wave Height", 100.0, 0.0, 0, NO),
    'Swell' : ("Swell", VECTOR, "ft", "Primary Swell", 100.0, 0.0, 0, NO),
    'Swell2' : ("Swell2", VECTOR, "ft", "Secondary Swell", 100.0, 0.0, 0, NO),
    'Period' : ("Period", SCALAR, "sec", "Primary Period", 30.0, 0.0, 0, NO),
    'IceCoverage' : ("IceCoverage", SCALAR, "%", "Ice Coverage Amount", 100.0, 0.0, 0, NO),
    'SurfHeight' : ("SurfHeight", SCALAR, "ft", "Total Wave Height", 100.0, 0.0, 0, NO),
    ##########DCS3499
    'SigWaveHgt' : ("SigWaveHgt", SCALAR, "ft",
                    "Significant wave height of combined wind waves and swells",
                    30.0, 0.0, 0, NO),
    'PeakWaveDir' : ("PeakWaveDir", VECTOR, "degree", "Direction of peak wave", 100.0, 0.0, 0, NO),
    'WindWaveHgt' : ("WindWaveHgt", SCALAR, "ft", "Significant wave height of wind waves", 30.0, 0.0, 0, NO),
    'WindWavePeriod' : ("WindWavePeriod", SCALAR, "sec.", "Wind wave peak period", 20.0, 0.0, 0, NO),
    'WindWaveDir' : ("WindWaveDir", VECTOR, "degree", "Direction of wind waves", 100.0, 0.0, 0, NO),
    'NWPSwind' : ("NWPSwind", VECTOR, "kts", "NWPSwind", 150.0, 0.0, 0, NO),
    'UWaveDir' : ("UWaveDir", SCALAR, "m/s", "U WaveDir Comp", 0.50, -0.50, 3, NO),
    'VWaveDir' : ("VWaveDir", SCALAR, "m/s", "V WaveDir Comp", 0.50, -0.50, 3, NO),
    'SwanSwell' : ("SwanSwell", SCALAR, "ft", "Total Significant Swell Height", 40.0, 0.0, 2, NO),
    'SST' : ("SST", SCALAR, "F", "Sea Sfc Temp", 100.0, 25.0, 0, NO),
    'StormTide' : ('StormTide', SCALAR, 'ft', 'Storm Tide', 30.0, -8.0, 1, NO),
    #Fcst Grids - for partitioned wave groups
    'Wave1' : ("Wave1", VECTOR, "ft", "WAVE1", 50.0, 0.0, 0, NO),
    'Wave2' : ("Wave2", VECTOR, "ft", "WAVE2", 50.0, 0.0, 0, NO),
    'Wave3' : ("Wave3", VECTOR, "ft", "WAVE3", 50.0, 0.0, 0, NO),
    'Wave4' : ("Wave4", VECTOR, "ft", "WAVE4", 50.0, 0.0, 0, NO),
    'Wave5' : ("Wave5", VECTOR, "ft", "WAVE5", 50.0, 0.0, 0, NO),
    'Wave6' : ("Wave6", VECTOR, "ft", "WAVE6", 50.0, 0.0, 0, NO),
    'Wave7' : ("Wave7", VECTOR, "ft", "Wave7", 50.0, 0.0, 0, NO),
    'Wave8' : ("Wave8", VECTOR, "ft", "Wave8", 50.0, 0.0, 0, NO),
    'Wave9' : ("Wave9", VECTOR, "ft", "Wave9", 50.0, 0.0, 0, NO),
    #Fcst Grids - for partitioned wave groups
    'Period1' : ("Period1", SCALAR, "sec", "Period1", 30.0, 0.0, 0, NO),
    'Period2' : ("Period2", SCALAR, "sec", "Period2", 30.0, 0.0, 0, NO),
    'Period3' : ("Period3", SCALAR, "sec", "Period3", 30.0, 0.0, 0, NO),
    'Period4' : ("Period4", SCALAR, "sec", "Period4", 30.0, 0.0, 0, NO),
    'Period5' : ("Period5", SCALAR, "sec", "Period5", 30.0, 0.0, 0, NO),
    'Period6' : ("Period6", SCALAR, "sec", "Period6", 30.0, 0.0, 0, NO),
    'Period7' : ("Period7", SCALAR, "sec", "Period7", 30.0, 0.0, 0, NO),
    'Period8' : ("Period8", SCALAR, "sec", "Period8", 30.0, 0.0, 0, NO),
    'Period9' : ("Period9", SCALAR, "sec", "Period9", 30.0, 0.0, 0, NO),
    'RipProb' : ("RipProb", SCALAR, "%", "Rip Current Probability", 100.0, 0.0, 0, NO),
    'ErosionProb' : ("ErosionProb", SCALAR, "%", "Dune Erosion Probability", 100.0, 0.0, 0, NO),
    'OverwashProb' : ("OverwashProb", SCALAR, "%", "Dune Overwash Probability", 100.0, 0.0, 0, NO)
}
if SID in groups['GreatLake_SITES']:
    #  Redefine the WaveHeight field to include a decimal point
    optionalParmsDict['marine'].update({'WaveHeight' :
                 ("WaveHeight", SCALAR, "ft", "Wave Height", 40.0, 0.0, 1, NO)})

# Parameter set for Probability of weather type, Optional for sites.
optionalParmsDict['powt']={
     'PoTBD': ('PotBlowingDust', SCALAR, '%', 'Prob of Blowing Dust', 100.0, 0.0, 0, NO),
     'PoTBN': ('PotBlowingSand', SCALAR, '%', 'Prob of Blowing Sand', 100.0, 0.0, 0, NO),
     'PoTBS': ('PotBlowingSnow', SCALAR, '%', 'Prob of Blowing Snow', 100.0, 0.0, 0, NO),
     'PoTF': ('PotFog', SCALAR, '%', 'Prob of Fog', 100.0, 0.0, 0, NO),
     'PoTFR': ('PotFrost', SCALAR, '%', 'Prob of Frost', 100.0, 0.0, 0, NO),
     'PoTFl': ('PotFlurries', SCALAR, '%', 'Prob of Flurries', 100.0, 0.0, 0, NO),
     'PoTH': ('PotHaze', SCALAR, '%', 'Prob of Haze', 100.0, 0.0, 0, NO),
     'PoTIC': ('PotIceCrystals', SCALAR, '%', 'Prob of Ice Crystals', 100.0, 0.0, 0, NO),
     'PoTIF': ('PotIceFog', SCALAR, '%', 'Prob of Ice Fog', 100.0, 0.0, 0, NO),
     'PoTIP': ('PotSleet', SCALAR, '%', 'Prob of Sleet', 100.0, 0.0, 0, NO),
     'PoTK': ('PotSmoke', SCALAR, '%', 'Prob of Smoke', 100.0, 0.0, 0, NO),
     'PoTL': ('PotDrizzle', SCALAR, '%', 'Prob of Drizzle', 100.0, 0.0, 0, NO),
     'PoTR': ('PotRain', SCALAR, '%', 'Prob of Rain', 100.0, 0.0, 0, NO),
     'PoTRW': ('PotRainShowers', SCALAR, '%', 'Prob of Rain Showers', 100.0, 0.0, 0, NO),
     'PoTS': ('PotSnow', SCALAR, '%', 'Prob of Snow', 100.0, 0.0, 0, NO),
     'PoTSW': ('PotSnowShowers', SCALAR, '%', 'Prob of Snow Showers', 100.0, 0.0, 0, NO),
     'PoTSp': ('PotSprinkles', SCALAR, '%', 'Prob of Sprinkles', 100.0, 0.0, 0, NO),
     'PoTSvr': ('PotSevere', SCALAR, '%', 'Prob of Severe Storms', 100.0, 0.0, 0, NO),
     'PoTT': ('PotThunder', SCALAR, '%', 'Prob of Thunder', 100.0, 0.0, 0, NO),
     'PoTVA': ('PotVolcanicAsh', SCALAR, '%', 'Prob of Volcanic Ash', 100.0, 0.0, 0, NO),
     'PoTWP': ('PotWaterspout', SCALAR, '%', 'Prob of Waterspout', 100.0, 0.0, 0, NO),
     'PoTZF': ('PotFreezingFog', SCALAR, '%', 'Prob of Freezing Fog', 100.0, 0.0, 0, NO),
     'PoTZL': ('PotFreezingDrizzle', SCALAR, '%', 'Prob of Freezing Drizzle', 100.0, 0.0, 0, NO),
     'PoTZR': ('PotFreezingRain', SCALAR, '%', 'Prob of Freezing Rain', 100.0, 0.0, 0, NO),
     'PoTZY': ('PotFreezingSpray', SCALAR, '%', 'Prob of Freezing Spray', 100.0, 0.0, 0, NO),
     'PoTHZY': ('PotHeavyFreezingSpray', SCALAR, '%', 'Prob of Heavy Freezing Spray', 100.0, 0.0, 0, NO),
     'RoadTemp' : ("RoadTemp", SCALAR, "F", "Road Temperature", 120.0, -50.0, 0, NO),
     'MaxTwAloft' : ("MaxTwAloft", SCALAR, 'C', 'Max Wet-Bulb Temp in Warm Nose', 40.0, -20.0, 1, NO),
     'ProbIcePresent': ("ProbIcePresent", SCALAR, "%", "Prob of Ice Present", 100.0, 0.0, 0, NO),
     'ProbRefreezeSleet': ("ProbRefreezeSleet", SCALAR, "%", "Prob of Refreeze into Sleet", 100.0, 0.0, 0, NO),
     'SleetAmt': ("SleetAmt", SCALAR, "in", "Sleet Accumulation", 5.0, 0.0, 1, YES),
     'IceFlatAcc': ('IceFlatAccum', SCALAR, 'in', 'Flat Ice Accumulation', maxIceVal, 0.0, 2, YES),
     'IceLineAcc': ('IceLineAccum', SCALAR, 'in', 'Line Ice Accumulation', maxIceVal, 0.0, 2, YES),
}

# Parameter set for Winter Weather probabilities, Optional for sites.
#****** Winter 2017 changes
optionalParmsDict['winterProbs']={
    # Storm Total Snow related
    'StormTotalSnowWPC' : ("StormTotalSnowWPC", SCALAR, "in","WPC Storm Total Snow", 50.0, 0.0, 1, NO),

    # Snow Percentiles
    'SnowAmt5Prcntl' : ("SnowAmt5Prcntl", SCALAR, "in","5 percentile", 100.0, -40.0, 1, NO),
    'SnowAmt10Prcntl' : ("SnowAmt10Prcntl", SCALAR, "in","10 percentile", 100.0, -40.0, 1, NO),
    'SnowAmt25Prcntl' : ("SnowAmt25Prcntl", SCALAR, "in","25 percentile", 100.0, -40.0, 1, NO),
    'SnowAmt50Prcntl' : ("SnowAmt50Prcntl", SCALAR, "in","50 percentile", 100.0, -40.0, 1, NO),
    'SnowAmt75Prcntl' : ("SnowAmt75Prcntl", SCALAR, "in","75 percentile", 100.0, -40.0, 1, NO),
    'SnowAmt90Prcntl' : ("SnowAmt90Prcntl", SCALAR, "in","90 percentile", 100.0, -40.0, 1, NO),
    'SnowAmt95Prcntl' : ("SnowAmt95Prcntl", SCALAR, "in","95 percentile", 100.0, -40.0, 1, NO),

    # Snow Exceedance Probabilities (Add others as needed)
    'ProbSnowGET' : ("ProbSnowGET", SCALAR, "%", "Prob. snow >= trace", 100.0, 0.0, 0, NO),
    'ProbSnowGE1' : ("ProbSnowGE1", SCALAR, "%", "Prob. snow >= 1 inch", 100.0, 0.0, 0, NO),
    'ProbSnowGE2' : ("ProbSnowGE2", SCALAR, "%", "Prob. snow >= 2 inches", 100.0, 0.0, 0, NO),
    'ProbSnowGE4' : ("ProbSnowGE4", SCALAR, "%", "Prob. snow >= 4 inches", 100.0, 0.0, 0, NO),
    'ProbSnowGE6' : ("ProbSnowGE6", SCALAR, "%", "Prob. snow >= 6 inches", 100.0, 0.0, 0, NO),
    'ProbSnowGE8' : ("ProbSnowGE8", SCALAR, "%", "Prob. snow >= 8 inches", 100.0, 0.0, 0, NO),
    'ProbSnowGE12' : ("ProbSnowGE12", SCALAR, "%", "Prob. snow >= 12 inches", 100.0, 0.0, 0, NO),
    'ProbSnowGE18' : ("ProbSnowGE18", SCALAR, "%", "Prob. snow >= 18 inches", 100.0, 0.0, 0, NO),

    # Freezing Rain Percentiles
    'IceAccum5Prcntl' : ("IceAccum5Prcntl", SCALAR, "in","5 percentile", 5.0, -4.0, 2, NO),
    'IceAccum10Prcntl' : ("IceAccum10Prcntl", SCALAR, "in","10 percentile", 5.0, -4.0, 2, NO),
    'IceAccum25Prcntl' : ("IceAccum25Prcntl", SCALAR, "in","25 percentile", 5.0, -4.0, 2, NO),
    'IceAccum50Prcntl' : ("IceAccum50Prcntl", SCALAR, "in","50 percentile", 5.0, -4.0, 2, NO),
    'IceAccum75Prcntl' : ("IceAccum75Prcntl", SCALAR, "in","75 percentile", 5.0, -4.0, 2, NO),
    'IceAccum90Prcntl' : ("IceAccum90Prcntl", SCALAR, "in","90 percentile", 5.0, -4.0, 2, NO),
    'IceAccum95Prcntl' : ("IceAccum95Prcntl", SCALAR, "in","95 percentile", 5.0, -4.0, 2, NO),

    # Freezing rain accretion probabilities
    'ProbIceGE001' : ("ProbIceGE001", SCALAR, "%", "Prob. ice >= 0.01", 100.0, 0.0, 0, NO),
    'ProbIceGE010' : ("ProbIceGE010", SCALAR, "%", "Prob. ice >= 0.10", 100.0, 0.0, 0, NO),
    'ProbIceGE025' : ("ProbIceGE025", SCALAR, "%", "Prob. ice >= 0.25", 100.0, 0.0, 0, NO),
    'ProbIceGE050' : ("ProbIceGE050", SCALAR, "%", "Prob. ice >= 0.50", 100.0, 0.0, 0, NO),

# Persist WPC snow prob grids
    'SnowAmt5PrcntlWPC' : ("SnowAmt5PrcntlWPC", SCALAR, "in","WPC 5th percentile snow amount", 100.0, -40.0, 1, NO),
    'SnowAmt10PrcntlWPC' : ("SnowAmt10PrcntlWPC", SCALAR, "in","WPC 10th percentile snow amount", 100.0, -40.0, 1, NO),
    'SnowAmt25PrcntlWPC' : ("SnowAmt25PrcntlWPC", SCALAR, "in","WPC 25th percentile snow amount", 100.0, -40.0, 1, NO),
    'SnowAmt50PrcntlWPC' : ("SnowAmt50PrcntlWPC", SCALAR, "in","WPC 50th percentile snow amount", 100.0, -40.0, 1, NO),
    'SnowAmt75PrcntlWPC' : ("SnowAmt75PrcntlWPC", SCALAR, "in","WPC 75th percentile snow amount", 100.0, -40.0, 1, NO),
    'SnowAmt90PrcntlWPC' : ("SnowAmt90PrcntlWPC", SCALAR, "in","WPC 90th percentile snow amount", 100.0, -40.0, 1, NO),
    'SnowAmt95PrcntlWPC' : ("SnowAmt95PrcntlWPC", SCALAR, "in","WPC 95th percentile snow amount", 100.0, -40.0, 1, NO),
    'ProbSnowGETWPC' : ("ProbSnowGETWPC", SCALAR, "%", "WPC Prob. snow >= trace", 100.0, 0.0, 0, NO),
    'ProbSnowGE1WPC' : ("ProbSnowGE1WPC", SCALAR, "%", "WPC Prob. snow >= 1 in", 100.0, 0.0, 0, NO),
    'ProbSnowGE2WPC' : ("ProbSnowGE2WPC", SCALAR, "%", "WPC Prob. snow >= 2 in", 100.0, 0.0, 0, NO),
    'ProbSnowGE4WPC' : ("ProbSnowGE4WPC", SCALAR, "%", "WPC Prob. snow >= 4 in", 100.0, 0.0, 0, NO),
    'ProbSnowGE6WPC' : ("ProbSnowGE6WPC", SCALAR, "%", "WPC Prob. snow >= 6 in", 100.0, 0.0, 0, NO),
    'ProbSnowGE8WPC' : ("ProbSnowGE8WPC", SCALAR, "%", "WPC Prob. snow >= 8 in", 100.0, 0.0, 0, NO),
    'ProbSnowGE12WPC' : ("ProbSnowGE12WPC", SCALAR, "%", "WPC Prob. snow >= 12 in", 100.0, 0.0, 0, NO),
    'ProbSnowGE18WPC' : ("ProbSnowGE18WPC", SCALAR, "%", "WPC Prob. snow >= 18 in", 100.0, 0.0, 0, NO),
}

# Add rainfall probability definitions
optionalParmsDict['rainfallProb']={
    # Rain Percentiles
    'QPF5Prcntl' : ("QPF5Prcntl", SCALAR, "in","5 percentile", 36.0, -24.0, 2, NO),
    'QPF10Prcntl' : ("QPF10Prcntl", SCALAR, "in","10 percentile", 36.0, -24.0, 2, NO),
    'QPF25Prcntl' : ("QPF25Prcntl", SCALAR, "in","25 percentile", 36.0, -24.0, 2, NO),
    'QPF50Prcntl' : ("QPF50Prcntl", SCALAR, "in","50 percentile", 36.0, -24.0, 2, NO),
    'QPF75Prcntl' : ("QPF75Prcntl", SCALAR, "in","75 percentile", 36.0, -24.0, 2, NO),
    'QPF90Prcntl' : ("QPF90Prcntl", SCALAR, "in","90 percentile", 36.0, -24.0, 2, NO),
    'QPF95Prcntl' : ("QPF95Prcntl", SCALAR, "in","95 percentile", 36.0, -24.0, 2, NO),

    # Rain Exceedance Probabilities (Add others as needed)
    'ProbRainGE001' : ("ProbRainGE001", SCALAR, "%", "Prob. Rain >= 0.01 in", 100.0, 0.0, 0, NO),
    'ProbRainGE010' : ("ProbRainGE010", SCALAR, "%", "Prob. Rain >= 0.10 in", 100.0, 0.0, 0, NO),
    'ProbRainGE025' : ("ProbRainGE025", SCALAR, "%", "Prob. Rain >= 0.25 in", 100.0, 0.0, 0, NO),
    'ProbRainGE050' : ("ProbRainGE050", SCALAR, "%", "Prob. Rain >= 0.50 in", 100.0, 0.0, 0, NO),
    'ProbRainGE075' : ("ProbRainGE075", SCALAR, "%", "Prob. Rain >= 0.75 in", 100.0, 0.0, 0, NO),
    'ProbRainGE100' : ("ProbRainGE100", SCALAR, "%", "Prob. Rain >= 1.00 in", 100.0, 0.0, 0, NO),
    'ProbRainGE150' : ("ProbRainGE150", SCALAR, "%", "Prob. Rain >= 1.50 in", 100.0, 0.0, 0, NO),
    'ProbRainGE200' : ("ProbRainGE200", SCALAR, "%", "Prob. Rain >= 2.00 in", 100.0, 0.0, 0, NO),
    'ProbRainGE250' : ("ProbRainGE250", SCALAR, "%", "Prob. Rain >= 2.50 in", 100.0, 0.0, 0, NO),
    'ProbRainGE300' : ("ProbRainGE300", SCALAR, "%", "Prob. Rain >= 3.00 in", 100.0, 0.0, 0, NO),
}


# Make all optional parms available as variables.
for optionalParmKey in optionalParmsDict:
    for pname,parm in optionalParmsDict[optionalParmKey].iteritems():
        setattr(sys.modules[__name__],pname,parm)

#-----------------------------------
# DO NOT CHANGE THE FOLLOWING SECTION
#------------------------------------
if not BASELINE and siteImport('localWxConfig'):
    types = localWxConfig.types


#---------------------------------------------------------------------------
#
#  Projection Configuration section.
#
#---------------------------------------------------------------------------
from com.raytheon.uf.common.dataplugin.gfe.config import ProjectionData
ProjectionType = ProjectionData.ProjectionType
NONE = ProjectionType.NONE
LAMBERT_CONFORMAL = ProjectionType.LAMBERT_CONFORMAL
MERCATOR = ProjectionType.MERCATOR
POLAR_STEREOGRAPHIC = ProjectionType.POLAR_STEREOGRAPHIC
LATLON = ProjectionType.LATLON

# projectionID / projectionType / latLonLL / latLonUR /
# latLonOrigin / stdParallelOne / stdParallelTwo / gridPointLL / gridPointUR
# latIntersect / lonCenter / lonOrigin

Grid201 = ('Grid201',POLAR_STEREOGRAPHIC,
      (-150.00, -20.826), (-20.90846, 30.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (65, 65), 0.0, 0.0, -105.0)

Grid202 = ('Grid202', POLAR_STEREOGRAPHIC,
      (-141.028, 7.838), (-18.576, 35.617),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (65, 43), 0.0, 0.0, -105.0)

Grid203 = ('Grid203', POLAR_STEREOGRAPHIC,
      (-185.837, 19.132), (-53.660, 57.634),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (45, 39), 0.0, 0.0, -150.0)

Grid204 = ('Grid204', MERCATOR,
      (-250.0, -25.0), (-109.129, 60.644),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (93, 68), 0.0, -179.564, 0.0)

Grid205 = ('Grid205', POLAR_STEREOGRAPHIC,
      (-84.904, 0.616), (-15.000, 45.620),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (45, 39), 0.0, 0.0, -60.0)

Grid206 = ('Grid206', LAMBERT_CONFORMAL,
      (-117.991, 22.289), (-73.182, 51.072),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (51, 41), 0.0, 0.0, 0.0)

Grid207 = ('Grid207', POLAR_STEREOGRAPHIC,
      (-175.641, 42.085), (-93.689, 63.976),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (49, 35), 0.0, 0.0, -150.0)

Grid208 = ('Grid208', MERCATOR,
      (-166.219, 10.656), (-147.844, 27.917),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (25, 25), 0.0, -157.082, 0.0)

Grid209 = ('Grid209', LAMBERT_CONFORMAL,
      (-117.991, 22.289), (-73.182, 51.072),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (101, 81), 0.0, 0.0, 0.0)

Grid210 = ('Grid210', MERCATOR,
      (-77.000, 9.000), (-58.625, 26.422),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (25, 25), 0.0, -67.812, 0.0)

Grid211 = ('Grid211', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (93, 65), 0.0, 0.0, 0.0)

Grid212 = ('Grid212', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (185, 129), 0.0, 0.0, 0.0)

Grid213 = ('Grid213', POLAR_STEREOGRAPHIC,
      (-141.028, 7.838), (-18.577, 35.617),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (129, 85), 0.0, 0.0, -105.0)

Grid214 = ('Grid214', POLAR_STEREOGRAPHIC,
      (-175.641, 42.085), (-93.689, 63.975),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (97, 69), 0.0, 0.0, -150.0)

# (new alaska grid)
Grid214AK = ('Grid214AK', POLAR_STEREOGRAPHIC,
             (-178.571, 40.5301), (-93.689, 63.975),
             (0.0, 0.0), 0.0, 0.0, (1,1), (104, 70), 0.0, 0.0, -150.0)

Grid215 = ('Grid215', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (369, 257), 0.0, 0.0, 0.0)

Grid216 = ('Grid216', POLAR_STEREOGRAPHIC,
      (-173.000, 30.000), (-62.850, 70.111),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (139, 107), 0.0, 0.0, -135.0)

Grid217 = ('Grid217', POLAR_STEREOGRAPHIC,
      (-173.000, 30.000), (-62.850, 70.111),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (277, 213), 0.0, 0.0, -135.0)

Grid218 = ('Grid218', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (614, 428), 0.0, 0.0, 0.0)

Grid219 = ('Grid219', POLAR_STEREOGRAPHIC,
      (-119.559, 25.008), (60.339, 24.028),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (385, 465), 0.0, 0.0, -80.0)

Grid221 = ('Grid221', LAMBERT_CONFORMAL,
      (-145.500, 1.000), (-2.566, 46.352),
      (-107.0, 50.0), 50.0, 50.0, (1, 1), (349, 277), 0.0, 0.0, 0.0)

Grid222 = ('Grid222', LAMBERT_CONFORMAL,
      (-145.500, 1.000), (-2.566, 46.352),
      (-107.0, 50.0), 50.0, 50.0, (1, 1), (59, 47), 0.0, 0.0, 0.0)

Grid225 = ('Grid225', MERCATOR,
      (-250.0, -25.0), (-109.129, 60.644),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (185, 135), 0.0, -179.564, 0.0)

Grid226 = ('Grid226', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (737, 513), 0.0, 0.0, 0.0)

Grid227 = ('Grid227', LAMBERT_CONFORMAL,
      (-133.459, 12.190), (-49.385, 57.290),
      (-95.0, 25.0), 25.0, 25.0, (1, 1), (1473, 1025), 0.0, 0.0, 0.0)

Grid228 = ('Grid228', LATLON,
      (0.0, 90.0), (359.0, -90.0), (0.0, 0.0), 0.0, 0.0,
      (1, 1), (144, 73), 0.0, 0.0, 0.0)

Grid229 = ('Grid229', LATLON,
      (0.0, 90.0), (359.0, -90.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (360, 181), 0.0, 0.0, 0.0)

Grid230 = ('Grid230', LATLON,
      (0.0, 90.0), (359.5, -90.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (720, 361), 0.0, 0.0, 0.0)

Grid231 = ('Grid231', LATLON,
      (0.0, 0.0), (359.5, 90.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (720, 181), 0.0, 0.0, 0.0)

Grid232 = ('Grid232', LATLON,
      (0.0, 0.0), (359.0, 90.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (360, 91), 0.0, 0.0, 0.0)

Grid233 = ('Grid233', LATLON,
      (0.0, -78.0), (358.750, 78.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (288, 157), 0.0, 0.0, 0.0)

Grid234 = ('Grid234', LATLON,
      (-98.000, 15.0), (-65.000, -45.0),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (133, 121), 0.0, 0.0, 0.0)

Grid235 = ('Grid235', LATLON,
      (0.250, 89.750), (359.750, -89.750),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (720, 360), 0.0, 0.0, 0.0)

HRAP = ('HRAP', POLAR_STEREOGRAPHIC,
      (-119.036, 23.097), (-75.945396, 53.480095),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (801, 881), 0.0, 0.0, -105.0)

NDFD_Oceanic_10K = ('NDFD_Oceanic_10km', MERCATOR,
      (-230.094, -30.4192), (10.71, 80.01),
      (0.0, 0.0), 0.0, 0.0, (1, 1), (2517, 1793), 0.0, -109.962, 0.0)

#  Add a new domain for NHC purposes
GridForNHA = ('GridForNHA', LAMBERT_CONFORMAL,
      (-103.929, 20.164), (-50.8894, 42.9545),
      (-95.0, 35.0), 35.0, 35.0, (1, 1), (1833,1241), 0.0, 0.0, 0.0)

# list of all projections
allProjections = [Grid201, Grid202, Grid203, Grid204, Grid205, Grid206,
 Grid207, Grid208, Grid209, Grid210, Grid211, Grid212, Grid213, Grid214,
 Grid214AK, Grid215, Grid216, Grid217, Grid218, Grid219, Grid221, Grid222,
 Grid225, Grid226, Grid227, Grid228, Grid229, Grid230, Grid231, Grid232,
 Grid233, Grid234, Grid235, HRAP, NDFD_Oceanic_10K, GridForNHA]

#---------------------------------------------------------------------------
#
#  Grid Domain configuration section
#
#---------------------------------------------------------------------------
#
# xdim/ydim:  Defines the dimensions of the grids. (GFE grid size)
#
# origin:  Defines the lower-left corner of the grid (point 0,0) in
#   world coordinates.
#
# extent:  Defines the "size" of the grid in world coordinates.  The upper
#   right corner is the origin+extent.
#
# TimeZone: Defines the timezone used by this site in standard TZ format.
# Refer to /usr/share/zoneinfo/zone.tab for the correct settings.
#
# Projection:  Defines the projection identifier to be used for this domain.

# Note that all parameters for an existing database must use the same
# projection, though not necessarily the same grid size and location.

# These values are set up for AWIPS.  There is a script at the end
# of this section that adjusts the resolution for the RPP sites.

#         [xdim, ydim] / (origin) /( extent)  / TimeZone / Projection / OfficeType

SITES = {
#WFOs
    # Experimental combined AFC site
    'AFC' : ([1057, 449], (1.0, 19.00),  (66.0, 28.0), 'America/Anchorage', Grid214AK, "wfo"),
    'ABQ' : ([145, 145], (36.00, 22.00), (9.0, 9.0), 'MST7MDT', Grid211,"wfo"),
    'ABR' : ([145, 145], (45.00, 35.00), (9.0, 9.0), 'CST6CDT', Grid211,"wfo"),
    'AER' : ([369, 337], (44.00, 23.00), (23.0, 21.0), 'America/Anchorage', Grid214AK, "wfo"),
    'AFG' : ([641, 497], (27.0, 38.0),   (40.0, 31.0), 'America/Anchorage', Grid214AK, "wfo"),
    'AJK' : ([337, 241], (62.0, 23.0),   (21.0, 15.0), 'America/Juneau', Grid214AK, "wfo"),
    'AKQ' : ([145, 145], (68.00, 25.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'ALU' : ([865, 449], (1.0, 19.0),    (54.0, 28.0), 'America/Anchorage', Grid214AK, "wfo"),
    'ALY' : ([145, 145], (70.00, 33.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'AMA' : ([145, 145], (41.00, 21.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'APX' : ([145, 145], (58.00, 34.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'ARX' : ([145, 145], (52.00, 33.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'BGM' : ([145, 145], (68.00, 33.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'BIS' : ([145, 145], (43.00, 37.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'BMX' : ([145, 145], (58.00, 19.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'BOI' : ([177, 177], (25.00, 34.00), (11.0, 11.0), 'MST7MDT', Grid211, "wfo"),
    'BOU' : ([145, 145], (38.00, 27.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'BOX' : ([187, 154], (75.375,34.59375), (5.8125,4.78125), "EST5EDT", Grid211, "wfo"),
    'BRO' : ([145, 145], (44.00, 10.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'BTV' : ([193, 157], (72.00, 37.15), (6.0, 4.875), 'EST5EDT', Grid211, "wfo"),
    'BUF' : ([145, 145], (66.00, 32.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'BYZ' : ([145, 145], (36.00, 37.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'CAE' : ([145, 145], (65.00, 20.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CAR' : ([145, 145], (75.00, 39.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CHS' : ([145, 145], (65.00, 18.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CLE' : ([145, 145], (62.00, 30.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CRP' : ([145, 145], (45.00, 11.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'CTP' : ([145, 145], (67.00, 30.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'CYS' : ([145, 145], (37.00, 31.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'DDC' : ([145, 145], (43.00, 24.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'DLH' : ([145, 145], (50.00, 37.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'DMX' : ([145, 145], (49.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'DTX' : ([161, 161], (57.00, 34.00), (10.0, 10.0), 'EST5EDT', Grid211, "wfo"),
    'DVN' : ([145, 145], (52.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'EAX' : ([145, 145], (50.00, 27.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'EKA' : ([145, 145], (20.00, 31.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'EPZ' : ([145, 145], (36.00, 16.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'EWX' : ([145, 145], (44.00, 12.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'FFC' : ([145, 145], (61.00, 18.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'FGF' : ([145, 145], (45.00, 39.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'FGZ' : ([145, 145], (29.00, 23.00), (9.0, 9.0), 'US/Arizona', Grid211, "wfo"),
    'FSD' : ([177, 177], (43.00, 32.00), (11.0, 11.0), 'CST6CDT', Grid211, "wfo"),
    'FWD' : ([145, 145], (45.00, 17.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'GGW' : ([145, 145], (36.00, 39.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'GID' : ([145, 145], (44.00, 28.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'GJT' : ([145, 145], (34.00, 27.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'GLD' : ([145, 145], (41.00, 26.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'GRB' : ([145, 145], (54.00, 35.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'GRR' : ([145, 145], (58.00, 33.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'GSP' : ([145, 145], (63.00, 21.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'GUM' : ([193, 193], (23.0, 26.0), (3.0, 3.0), 'Pacific/Guam', Grid204, "wfo"),
    'GYX' : ([193,209],  (76.00, 37.375), (6.0, 6.5), 'EST5EDT', Grid211, "wfo"),
    'HFO' : ([321, 225], (58.78125,29.875),(5.0,3.5), 'Pacific/Honolulu', Grid204, 'wfo'),
    'HGX' : ([145, 145], (48.00, 13.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'HNX' : ([145, 145], (22.00, 24.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'HUN' : ([161, 161], (60.0, 22.0),   (5.0, 5.0), 'CST6CDT', Grid211, "wfo"),
    'ICT' : ([145, 145], (45.00, 25.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'ILM' : ([145, 145], (67.00, 21.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'ILN' : ([145, 145], (60.00, 27.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'ILX' : ([145, 145], (55.00, 27.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'IND' : ([145, 145], (58.00, 27.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'IWX' : ([145, 145], (58.00, 30.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'JAN' : ([145, 145], (54.00, 18.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'JAX' : ([145, 145], (64.00, 14.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'JKL' : ([145, 145], (61.00, 25.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'KEY' : ([145, 145], (66.00, 8.00),  (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'LBF' : ([145, 145], (43.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LCH' : ([145, 145], (52.00, 15.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LIX' : ([145, 145], (54.00, 14.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LKN' : ([145, 145], (25.00, 30.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'LMK' : ([145, 145], (59.00, 25.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'LOT' : ([145, 145], (55.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LOX' : ([145, 145], (21.00, 23.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'LSX' : ([145, 145], (52.00, 25.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LUB' : ([145, 145], (39.00, 17.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'LWX' : ([145, 145], (67.00, 27.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'LZK' : ([145, 145], (51.00, 20.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MAF' : ([205,247],  (40.375, 16.8125), (6.375, 7.6875), 'CST6CDT', Grid211, "wfo"),
    'MEG' : ([145, 145], (54.00, 22.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MFL' : ([145, 145], (66.00, 9.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MFR' : ([145, 145], (20.00, 34.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'MHX' : ([145, 145], (68.00, 22.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MKX' : ([145, 145], (55.00, 33.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MLB' : ([145, 145], (66.00, 12.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MOB' : ([145, 145], (57.00, 16.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MPX' : ([145, 145], (50.00, 34.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'MQT' : ([145, 145], (56.00, 36.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MRX' : ([145, 145], (61.00, 22.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'MSO' : ([145, 145], (29.00, 39.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'MTR' : ([145, 145], (20.00, 26.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'OAX' : ([145, 145], (45.00, 30.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'OHX' : ([145, 145], (58.00, 22.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'OKX' : ([145, 145], (71.00, 30.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'OTX' : ([145, 145], (25.00, 40.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'OUN' : ([145, 145], (44.00, 21.00), (9.0, 9.0), 'CST6CDT',  Grid211, "wfo"),
    'PAH' : ([145, 145], (56.00, 24.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'PBZ' : ([145, 145], (65.00, 29.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'PDT' : ([145, 145], (23.00, 38.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'PHI' : ([145, 145], (70.00, 28.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'PIH' : ([145, 145], (30.00, 34.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'PQR' : ([145, 145], (19.00, 38.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'PSR' : ([145, 145], (28.00, 20.00), (9.0, 9.0), 'US/Arizona', Grid211, "wfo"),
    'PUB' : ([145, 145], (38.00, 26.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'RAH' : ([145, 145], (66.00, 22.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'REV' : ([145, 145], (23.00, 29.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'RIW' : ([145, 145], (35.00, 33.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'RLX' : ([145, 145], (63.00, 26.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'RNK' : ([161, 161], (67.0,  26.00), (5.0, 5.0), 'EST5EDT', Grid211, 'wfo'),
    'SEW' : ([145, 145], (21.00, 42.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'SGF' : ([145, 145], (51.00, 24.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'SGX' : ([145, 145], (24.00, 21.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'SHV' : ([145, 145], (50.00, 17.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'SJT' : ([145, 145], (43.00, 16.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'SJU' : ([32, 28], (10.0, 10.0), (8.0, 7.0), 'America/Puerto_Rico',Grid210, "wfo"),
    'SLC' : ([161, 161], (30.00, 28.00), (10.0, 10.0), 'MST7MDT', Grid211, "wfo"),
    'STO' : ([145, 145], (20.00, 28.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
    'TAE' : ([145, 145], (60.00, 15.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'TBW' : ([145, 145], (64.00, 11.00), (9.0, 9.0), 'EST5EDT', Grid211, "wfo"),
    'TFX' : ([145, 145], (32.00, 39.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'TOP' : ([145, 145], (47.00, 26.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'TSA' : ([145, 145], (48.00, 22.00), (9.0, 9.0), 'CST6CDT', Grid211, "wfo"),
    'TWC' : ([145, 145], (29.00, 20.00), (9.0, 9.0), 'US/Arizona', Grid211, "wfo"),
    'UNR' : ([145, 145], (40.00, 34.00), (9.0, 9.0), 'MST7MDT', Grid211, "wfo"),
    'VEF' : ([145, 145], (26.00, 25.00), (9.0, 9.0), 'PST8PDT', Grid211, "wfo"),
#RFCs
    'ACR' : ([565, 415], (26.0, 19.0), (60.0, 44.0), 'America/Anchorage', Grid214AK, "rfc"),
    'ALR' : ([299, 278], (59.0, 11.0), (17.0, 19.0), 'CST6CDT', Grid211, "rfc"),
    'FWR' : ([362, 334], (36.0, 11.0), (20.0, 20.0), 'CST6CDT', Grid211, "rfc"),
    'KRF' : ([408, 356], (33.0, 27.0), (26.0, 22.0), 'CST6CDT', Grid211, "rfc"),
    'MSR' : ([381, 304], (43.0, 28.0), (24.0, 20.0), 'CST6CDT', Grid211, "rfc"),
    'ORN' : ([303, 216], (51.0, 16.0), (18.0, 14.0), 'CST6CDT', Grid211, "rfc"),
    'PTR' : ([218, 308], (21.0, 35.0), (17.0, 19.0), 'PST8PDT', Grid211, "rfc"),
    'RHA' : ([132, 140], (69.0, 28.0), (7.0, 10.0), 'EST5EDT', Grid211, "rfc"),
    'RSA' : ([140, 296], (21.0, 23.0), (12.0, 17.0), 'PST8PDT', Grid211, "rfc"),
    'STR' : ([171, 307], (29.0, 20.0), (13.0, 18.0), 'MST7MDT', Grid211, "rfc"),
    'TAR' : ([226, 164], (69.0, 34.0), (13.0, 13.0), 'EST5EDT', Grid211, "rfc"),
    'TIR' : ([220, 171], (59.0, 25.0), (13.0, 12.0), 'EST5EDT', Grid211, "rfc"),
    'TUA' : ([281, 168], (39.0, 22.0), (18.0, 10.0), 'CST6CDT', Grid211, "rfc"),

#Special Sites - Added Hawaiian High Seas domain
    'US' : ([267, 159], (18.0, 9.5), (67.0, 40.0), 'EDT5EDT', Grid211, "other"),
    'FSL' : ([161, 145], (38.50, 27.00), (10.0, 9.0), 'MST7MDT', Grid211, "other"),
    'NH1' : ([838, 577], (887.0, 121.0), (837.0, 576.0), 'EST5EDT', NDFD_Oceanic_10K, "wfo"),
    'NH2' : ([1188, 363], (1328.0, 365.0), (1187.0, 362.0), 'EST5EDT', NDFD_Oceanic_10K, "wfo"),
    'ONA' : ([244, 383], (68.9375, 19.5625), (15.1875, 23.875), 'EST5EDT', Grid211, "wfo"),
    'ONP' : ([396, 415], (8.1875, 21.5625), (24.6875, 25.875), 'PST8PDT', Grid211, "wfo"),
    'HPA' : ([899, 671], (284.0, 30.0), (898.0, 670.0), 'Pacific/Honolulu', NDFD_Oceanic_10K, "wfo"),
    'WNJ' : ([301, 346], (1000.0, 475.0), (300.0, 345.0), 'CST6CDT', NDFD_Oceanic_10K, "wfo"),

#Aviation Domains for AAWU
    'AAWU' : ([705, 457], (1.0, 11.0), (88.0, 57.0), 'America/Anchorage', Grid214AK, 'nc'),
    'AVAK' : ([465, 417], (8.0, 12.0), (29.0, 26.0), 'America/Anchorage', Grid203, 'nc'),

#Regional Offices
    'VUY' : ([337,449], (62.00, 19.00), (21.0, 28.0), 'EST5EDT', Grid211, "ro"),
    'BCQ' : ([145,145], (50.00, 27.00), (9.0, 9.0), 'CST6CDT', Grid211, "ro"),
    'EHU' : ([657,321], (36.00, 9.50), (41.0, 20.0), 'CST6CDT', Grid211, "ro"),
    'VHW' : ([161,161], (30.00, 28.00), (10.0, 10.0), 'MST7MDT', Grid211, "ro"),
    'PBP' : ([321,225], (7.00, 11.00), (10.0, 7.0), 'Pacific/Honolulu', Grid208, "ro"),
    'VRH' : ([1409, 913], (1.0, 11.0), (88.0, 57.0), 'America/Anchorage', Grid214AK, 'nc'),

#National Centers
    'HAK' : ( [825,553], ( 1.0, 1.0), (103.0, 69.0), 'EST5EDT', Grid214AK, "nc"),
    'HUS' : ([1073,689], (19.0, 8.0), ( 67.0, 43.0), 'EST5EDT', Grid211,   "nc"),
    'NHA' : ([1873,1361], (35.5, 3.5), (58.5, 42.5), 'EST5EDT', Grid211, "nc"),

}

# Get list of valid office types, for validation.
VALID_OFFICE_TYPES = []
# List of all values of all sites.
for siteValues in SITES.values():
    # Office type is the 5th element of each site's values
    officeType = siteValues[5]
    if officeType not in VALID_OFFICE_TYPES:
        # A new office type
        VALID_OFFICE_TYPES.append(officeType)

#---------------------------------------------------------------------------
#
#  Time Constraint configuration section
#
#---------------------------------------------------------------------------
HOUR = 3600
DAY  = 24 * HOUR

# Start: is the number of seconds since 0000z for the first grid of the day
# Repeat: is the number of seconds from start until the next grid starts
# Duration: is the length of the grid in number of seconds

# Examples of constraints:
# Hourly temperatures
#     HrTemp = (0, HOUR, HOUR)
# QPF that is 6 hours long, aligned on 0000z, exists for every 6 hours
#     Q = (0, HOUR*6, HOUR*6)
#

# fixed time constraints: start / repeat / duration
TC_1M    = (0, 60, 60) # 1 minute
TC1      = (0, HOUR, HOUR)
TC3      = (0, 3 * HOUR, HOUR)
TC6      = (0, 6 * HOUR, HOUR)
TC12     = (0, 12 * HOUR, HOUR)
TC3NG    = (0, 3 * HOUR, 3 * HOUR)
TC6NG    = (0, 6 * HOUR, 6 * HOUR)
TC12NG   = (0, 12 * HOUR, 12 * HOUR)
TC24NG   = (0, 24 * HOUR, 24 * HOUR)
TC061212 = (6 * HOUR, 12 * HOUR, 12 * HOUR)
Persistent = (0, 0, 0)     # special time constraint


# The following time constraints are based on local standard time.
# Change the last parameter from 0 to 1 to force daylight savings time
# always.
# PWS TCs changed in OB9.3 for new 6 hour data from NHC
MaxTTC     = localTC(7*HOUR, 24*HOUR, 13*HOUR, 0)
MinTTC     = localTC(19*HOUR, 24*HOUR, 14*HOUR, 0)
MaxRHTC    = localTC(15*HOUR, 24*HOUR, 18*HOUR, 0)
MinRHTC    = localTC(3*HOUR, 24*HOUR, 18*HOUR, 0)
LT3NG      = localTC(0*HOUR, 3*HOUR, 3*HOUR, 0)
LT6NG      = localTC(0*HOUR, 6*HOUR, 6*HOUR, 0)
LT12NG     = localTC(6*HOUR, 12*HOUR, 12*HOUR, 0)
LTMOS      = localTC(6*HOUR, 12*HOUR, 12*HOUR, 0)  #special MOS local time
MaxTTCMOS  = localTC(6*HOUR, 24*HOUR, 12*HOUR, 0)  #special MOS maxT
MinTTCMOS  = localTC(18*HOUR, 24*HOUR, 12*HOUR, 0)  #special MOS minT
LT24       = localTC(0*HOUR, 24*HOUR, 24*HOUR, 0)
FireWx1300TC = localTC(13*HOUR, 24*HOUR, 1*HOUR, 0)   #special FireWx 1pm snap
#DR3511 DeltaMaxTTC  = localTC(7*HOUR, 24*HOUR, 16*HOUR, 0)  # just for HPCdeltaMaxT
PWSDTC     = localTC(11*HOUR, 24*HOUR, 12*HOUR, 0)
PWSNTC     = localTC(23*HOUR, 24*HOUR, 12*HOUR, 0)
# Alaska OCONUS
if SID in siteRegion['AR']:
    MaxTTC     = localTC(5*HOUR, 24*HOUR, 15*HOUR, 0)
    MinTTC     = localTC(17*HOUR, 24*HOUR, 18*HOUR, 0)

# From NwsInitsConfig
LT24APT  = localTC(7*HOUR, 24*HOUR, 24*HOUR, 0)
FireWxAvgTC = localTC( 12*HOUR,  24*HOUR,  6*HOUR, 0)
LT4HH = localTC(11*HOUR, 24*HOUR, 4*HOUR, 0)
SPC24 = (12*HOUR, 24*HOUR, 24*HOUR)
# For WR
TC0624NG=(6*HOUR,24*HOUR,24*HOUR)
TC12NG6=(6*HOUR,12*HOUR,12*HOUR)
# HIL Time Constraint
HILTC=(6*HOUR,24*HOUR,24*HOUR)

#---------------------------------------------------------------------------
#
#  Database/(Model) Attribute Configuration
#
#---------------------------------------------------------------------------
#
# name:  The model name of the database
#
# format:  Either 'GRID' or 'DFM'
#
# type:  Optional type of the database
#
# single:  YES or NO. YES if this database always exists and is not
#   based on model-times.  NO if this database is created/destroyed and
#   is based on model-runs.  When created, the names of these databases have
#   time stamps.
#
# official:  YES or NO.  YES if this is an official database from which
#   products can be generated.  NO if this is a conventional database.
#
# numVer:  Number of versions of this database to retain.
#
# purgeAge: Number of hours in the past before grids will be automatically
#   purged from the database.  If 0, then purging is disabled.
#

YES = 1
NO = 0
GRID = 'GRID'
# name /  format / type / single / official / numVer / purgeAge

Fcst        = ('Fcst',         GRID,   '', YES, NO,  1, 24)
Practice    = ('Fcst',         GRID,   'Prac', YES, NO,  1, 24)
TestFcst    = ('Fcst',         GRID,   'Test', YES, NO,  1, 24)
Restore     = ('Restore',      GRID,   '', YES, NO,  1, 24)
Test        = ('Test',         GRID,   'test', NO, NO,  1, 0)
Official    = ('Official',     GRID,   '', YES, YES, 1, 24)
ISC         = ('ISC',          GRID,   '', YES, NO,  1, 12)


#---------------------------------------------------------------------------
#
#  Search path for netCDF data files.
#  NOTE: This feature was implemented only backward compatibility with existing A1 datasets.
#        New datasets should be generated in a from that can be ingested by A2
#        It shoudl only be used for static datasets.
#        New files will not be recognized without a server restart.
#
#---------------------------------------------------------------------------
# Alaska OCONUS
if SID in groups['ALASKA_SITES']:
    NETCDFDIRS = [('/awips2/edex/data/gfe/climo/PRISMAK'),
                  ('/awips2/edex/data/gfe/climo/PRISMAK800'),
                  ]

# Hawaii OCONUS
elif SID == "HFO":
    NETCDFDIRS = [('/awips2/edex/data/gfe/topo/NED3ARCSTOPO','CRMTopo'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPONEW','NED'),
                  ('/awips2/edex/data/gfe/topo/StdTerrain/Hawaii', 'StdTerrain'),
                  ]

# San Juan OCONUS
elif SID == "SJU":
    NETCDFDIRS = [('/awips2/edex/data/gfe/topo/NED3ARCSTOPO','CRMTopo'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPONEW','NED'),
                  ('/awips2/edex/data/gfe/topo/VDATUMS','VDATUMS'),
                  ('/awips2/edex/data/gfe/topo/StdTerrain/PuertoRico', 'StdTerrain')
                  ]

# Guam OCONUS
elif SID == "GUM":
    NETCDFDIRS = []

#CONUS sites
elif SID in groups['CONUS_EAST_SITES']:
    NETCDFDIRS = [('/awips2/edex/data/gfe/climo/PRISM'),
                  ('/awips2/edex/data/gfe/climo/NCDC'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPO','CRMTopo'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPONEW','NED'),
                  ('/awips2/edex/data/gfe/topo/VDATUMS','VDATUMS'),
                  ('/awips2/edex/data/gfe/topo/StdTerrain/CONUS', 'StdTerrain'),
                  ]

else:   #######DCS3501 WEST_CONUS
    NETCDFDIRS = [('/awips2/edex/data/gfe/climo/PRISM'),
                  ('/awips2/edex/data/gfe/climo/NCDC'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPO','CRMTopo'),
                  ('/awips2/edex/data/gfe/topo/NED3ARCSTOPONEW','NED'),
                  ('/awips2/edex/data/gfe/topo/VDATUMS','VDATUMS'),
                  ('/awips2/edex/data/gfe/topo/StdTerrain/CONUS', 'StdTerrain'),
                  ]

#---------------------------------------------------------------------------
#
# Where to find (and what to call) satellite data.
#
#---------------------------------------------------------------------------
#

# This table contains product ID and weather element names for satellite data
#
# A product ID consists of the sector ID and physical element of the
# satellite product.
#
# Examples:
#
#   "East CONUS/Imager Visible"
#   "East CONUS/Imager 11 micron IR"
#   "East CONUS/Imager 13 micron (IR)"
#   "East CONUS/Imager 3.9 micron IR"
#

# Alaska OCONUS
if SID in groups['ALASKA_SITES']:
    SATDATA = []

# Hawaii OCONUS
elif SID == "HFO":
    SATDATA = []

# San Juan OCONUS
elif SID == "SJU":
    SATDATA = [("East CONUS/Imager Visible", "visibleEast"),
               ("East CONUS/Imager 11 micron IR", "ir11East"),
               ("East CONUS/Imager 13 micron (IR)", "ir13East"),
               ("East CONUS/Imager 3.9 micron IR", "ir39East"),
               ("East CONUS/Imager 6.7-6.5 micron IR (WV)", "waterVaporEast")]

# Guam OCONUS
elif SID == "GUM":
    SATDATA = []

#CONUS sites
else:
    SATDATA = [("West CONUS/Imager Visible", "visibleWest"),
               ("West CONUS/Imager 11 micron IR", "ir11West"),
               ("West CONUS/Imager 13 micron (IR)", "ir13West"),
               ("West CONUS/Imager 3.9 micron IR", "ir39West"),
               ("West CONUS/Imager 6.7-6.5 micron IR (WV)", "waterVaporWest"),
               ("East CONUS/Imager Visible", "visibleEast"),
               ("East CONUS/Imager 11 micron IR", "ir11East"),
               ("East CONUS/Imager 13 micron (IR)", "ir13East"),
               ("East CONUS/Imager 3.9 micron IR", "ir39East"),
               ("East CONUS/Imager 6.7-6.5 micron IR (WV)", "waterVaporEast")]

#---------------------------------------------------------------------------
#
#  Intersite Coordination Configurations
#
#---------------------------------------------------------------------------
# base urls for the ISC Routing Table
ISC_ROUTING_TABLE_ADDRESS = {
    "ANCF" : "http://localhost:8080/irt",
    "BNCF" : "http://localhost:8080/irt"
    }


# list of sites that from which you want ISC data (If None, ifpServer will
# automatically calculate the list.)  Should always include your own site.
REQUESTED_ISC_SITES = None

# Overall ISC request flag.  Must be set to 1 in order to request and receive
# ISC data.  Must be 1 to register with the IRT.
REQUEST_ISC = 0

# Sending control flag.  Set to 1 to send isc when data is saved.
SEND_ISC_ON_SAVE = 0

# Sending control flag.  Set to 1 to send isc when data is published.
SEND_ISC_ON_PUBLISH = 0

# List of weather elements to request for ISC.  If set to None, it defaults
# to the list of all weather elements in the Fcst database.
REQUESTED_ISC_PARMS = None

# Transmission script for sending data.  This is the script that iscExtract
# and other routines (e.g., vtec table sharing) will call to perform the
# actual transmission of data.
TRANSMIT_SCRIPT = GFESUITE_HOME + '/bin/gfe_msg_send -s %SUBJECT -a %ADDRESSES -i %WMOID -c 11 -p 0 -e %ATTACHMENTS'


# Extra ISC parms (weather elements).  These are a list of the baseline
# weather elements to be added as extra parms to the ISC database.  This
# is necessary when receiving ISC grids from a site that is a different
# office type than your own.  You never need to add weather elements
# to the ISC database that is your own office type.  The format of this
# entry is a list of tuples.  The tuple is a list of weather elements
# objects (such as Temp and not "T"), and an office type, such as "rfc".
EXTRA_ISC_PARMS = [([QPF,FloodingRainThreat], 'rfc'), ([QPF,FloodingRainThreat], 'wfo'), ([ProposedSS,Hazards,InundationMax,InundationTiming,SurgeHtPlusTideMSL,SurgeHtPlusTideMLLW,SurgeHtPlusTideMHHW,SurgeHtPlusTideNAVD], 'nc'),([ProposedSS,Hazards,InundationMax,InundationTiming,SurgeHtPlusTideMSL,SurgeHtPlusTideMLLW,SurgeHtPlusTideMHHW,SurgeHtPlusTideNAVD], 'wfo')]

#---------------------------------------------------------------------------
#
#  Misc. Configurations
#
#---------------------------------------------------------------------------
# defines the number of days to keep log files
LOG_FILE_PURGE_AFTER = 28

# auto configure NotifyTextProd -- set after OB6
AUTO_CONFIGURE_NOTIFYTEXTPROD = 1   #0=off,1=on


#-----------------------------------
# DO NOT CHANGE THE FOLLOWING SECTION
#------------------------------------
# import the local config file

myOfficeType = SITES[GFESUITE_SITEID][5]

AdditionalISCRouting = [
   # Configure by adding entries to this list in the form of:
   # ([WeatherElements],  ModelName, EditAreaPrefix)
   # Example:
   # ([Hazards, LAL, CWR], "ISCFire", "FireWxAOR_"),
]

#---------------------------------------------------------------------------
# Parm groups.  Combine parms with time constraints
# list of ([parms], timeConstraints)
#---------------------------------------------------------------------------

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# There is nothing special about these variables. They are just used as a
# convienence to set up multiple models in modelDict with the same parameter
# set.  However, model parms are no longer as generic as they once were and
# its just as easy to set the parms explicitly in modelDict.

STD6_MODEL = [([Temp, Td, RH, Wind, Wind20ft, Sky, FzLevel, SnowLevel], TC6),
             ([Haines, MixHgt, FreeWind, TransWind, VentRate], TC6),
             ([DSI, Stability, Ttrend, RHtrend], TC6),
             ([SnowAmt, PoP, CWR], TC6NG), ([QPF, Weather, IceAcc, LAL], TC6NG),
             ([MarineLayer, HrsOfSun, InvBurnOffTemp], LT24),
             ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
             ([MaxT], MaxTTC), ([MinT], MinTTC),
             ([Wetflag], FireWx1300TC)]

# hourly
STD1_MODEL = [([Temp, Td, RH, Wind, Wind20ft, Sky, FzLevel, SnowLevel], TC1),
             ([Haines, MixHgt, FreeWind, TransWind], TC1),
             ([DSI, Stability, VentRate, Ttrend, RHtrend], TC1),
             ([SnowAmt, PoP, CWR], TC1), ([QPF, Weather, IceAcc, LAL], TC1),
             ([MarineLayer, HrsOfSun, InvBurnOffTemp], LT24),
             ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
             ([MaxT], MaxTTC), ([MinT], MinTTC),
             ([Wetflag], FireWx1300TC)]

# 3 hourly
STD3_MODEL = [([Temp, Td, RH, Wind, Wind20ft, Sky, FzLevel, SnowLevel], TC3),
             ([Haines, MixHgt, FreeWind, TransWind], TC3),
             ([DSI, Stability, VentRate, Ttrend, RHtrend], TC3),
             ([SnowAmt, PoP, CWR], TC3NG), ([QPF, IceAcc, Weather, LAL], TC3NG),
             ([MarineLayer, HrsOfSun, InvBurnOffTemp], LT24),
             ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
             ([MaxT], MaxTTC), ([MinT], MinTTC),
             ([Wetflag], FireWx1300TC)]

# Fcst and official database parameter groupings
OFFICIALDBS = [([Temp, Td, Wind, Weather, Sky, FzLevel, SnowLevel], TC1),
    ([HeatIndex, WindChill, RH, SnowAmt, CWR, QPF], TC1),
    ([PoP, Ttrend, RHtrend, Wind20ft, WindGust], TC1),
    ([MinT], MinTTC), ([MaxT], MaxTTC),
    ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
    ([VentRate, LAL, Haines, MixHgt, FreeWind, TransWind], TC1),
    ([DSI, Stability, MarineLayer], TC1),
    ([HrsOfSun, InvBurnOffTemp], LT24),
    ([IceAcc, IceCoverage, Hazards], TC1),
    ([Wetflag], FireWx1300TC),
    ([StormTotalSnow], TC1),
        # Tropical parms
    ([prob34, prob50, prob64,pws34,pws50,pws64,], TC1),
    ([InundationMax,SurgeHtPlusTideMSL,SurgeHtPlusTideMLLW,SurgeHtPlusTideMHHW,SurgeHtPlusTideNAVD], TC1),
    ([ProposedSS,DiffSS,tempProposedSS,InitialSS], TC1),
    ([WindThreat,StormSurgeThreat,FloodingRainThreat,TornadoThreat], TC1),
    ([pwsD34,pwsD64], PWSDTC),
    ([pwsN34,pwsN64], PWSNTC),
    ([pws34int,pws64int,InundationTiming,QPFtoFFGRatio], TC6NG),
    # DR20541 and 20482
    ([PoP12hr], TC12NG),
    ([QPF6hr, SnowAmt6hr], TC6NG),
    ([cape], LT6NG),
    ([ApparentT, HeatIndex, WindChill, LkSfcT, SnowMap, SnowRatio, StormTotalQPF], TC1),
    ]

## JCM Change wave and period (and swanswell) to TC1 for all marine sites
if SID in groups['marineSites'] or SID in groups['GreatLake_SITES']:
    OFFICIALDBS.append(([WaveHeight, PeakWaveDir, WindWaveHeight, SurfHeight, Swell, Swell2, Period, Period2], TC1))
    OFFICIALDBS.append(([SwanSwell, Wave1, Wave2, Wave3, Wave4, Wave5, Wave6, Wave7, Wave8, Wave9,
                         Period1, Period3, Period4, Period5, Period6, Period7, Period8, Period9], TC1))
    OFFICIALDBS.append(([NWPSwind, UWaveDir, VWaveDir, WaveDir, RipProb, ErosionProb, OverwashProb],TC1))

# NWPS
nwpsCG1_MODEL = [([SwanSwell, Period, WaveHeight, PeakWaveDir, WindWaveHeight, Wind, RipProb, ErosionProb, OverwashProb], TC1)]
nwpsTrkngCG0_MODEL = [([Wave1, Wave2, Wave3, Wave4, Wave5, Wave6, Wave7, Wave8, Wave9, Period1, Period2, Period3, Period4, Period5, Period6,Period7, Period8, Period9], TC1)]

# OPC TAF parameters (for NW, SW, and E)
OPCTAFBPARMS = [([WindWaveHeight, WaveHeight], TC1)]

# SAT database parameter groupings
SATPARMS = [([SatVisE, SatIR11E, SatIR13E, SatIR39E, SatWVE, SatFogE], TC_1M),
            ([SatVisW, SatIR11W, SatIR13W, SatIR39W, SatWVW, SatFogW], TC_1M)]

# RTMA database parameter groupings
# DCS17288/DR17144
if SID in groups['OCONUS_SITES']:
    RTMAPARMS = [([Temp,Td,RH,Wind,Vis,Pressure,WindGust],TC1),
             ([MinT],MinTTC), ([MaxT],MaxTTC),
             ([MinRH],MinRHTC), ([MaxRH],MaxRHTC),
             ([TUnc,TdUnc,WSpdUnc,WDirUnc,VisUnc,PressUnc,WGustUnc],TC1)]
else:
    RTMAPARMS = [([Temp,Td,RH,Wind,QPE,Sky,Vis,Pressure,WindGust],TC1),
             ([MinT],MinTTC), ([MaxT],MaxTTC),
             ([MinRH],MinRHTC), ([MaxRH],MaxRHTC),
             ([TUnc,TdUnc,WSpdUnc,WDirUnc,VisUnc,PressUnc,WGustUnc,SkyUnc],TC1)]

#---------------------------------------------------------------------------
# Databases for a site.
# list of (Database, [parms])
# Official, Practice, TestFcst, Test are all set after Fcst is defined.
#---------------------------------------------------------------------------

# Intersite coordination database parameter groupings, based on
# OFFICIALDBS, but time constraint is always TC1
ISCPARMS = []
if type(officeType) != str:
    raise TypeError, "Office type not a str: " + `officeType`
else:
    if officeType not in VALID_OFFICE_TYPES:
        raise ValueError, "Office type: " + str(officeType) + " does not match any of the following: [" + (', '.join(VALID_OFFICE_TYPES)) + "]"


#
# new parameters for NewTerrain
#
NewTopo     = ("NewTopo",     SCALAR, "ft", "New Topo",      50000.0, -32000.0, 1, NO)
PrevTopo    = ("PrevTopo",    SCALAR, "ft", "Previous Topo", 50000.0, -32000.0, 1, NO)
StdTopo     = ("StdTopo",     SCALAR, "ft", "Standard Topo", 50000.0, -32000.0, 1, NO)
GTOPO       = ("GTOPO",       SCALAR, "ft", "GTOPO30",       50000.0, -32000.0, 1, NO)
Topo        = ("Topo",        SCALAR, "ft", "Topography",    50000.0, -32000.0, 1, NO)

# Add Topo to ISC parms for NewTerrain
if type(REQUESTED_ISC_PARMS) is list and not "NewTopo" in REQUESTED_ISC_PARMS:
    REQUESTED_ISC_PARMS.append("NewTopo")
ISCPARMS.append(([NewTopo], Persistent))


#---------------------------------------------------------------------------
#
#  General server configuration section
#
#---------------------------------------------------------------------------

#----------------------------------------------------------------------------
# Server settings     DO NOT CHANGE THESE DEFINITIONS
#----------------------------------------------------------------------------
from com.raytheon.edex.plugin.gfe.config import SimpleServerConfig
IFPConfigServer = SimpleServerConfig()
#IFPConfigServer.allowedNodes             = []
IFPConfigServer.allowTopoBelowZero       = 1

#------------------------------------------------------------------------------
# serverConfig model configuration is now done in the modelDict dictionary.
# variables D2DMODELS, D2DDBVERSIONS,D2DAccumulativeElements,INITMODULES,
# INITSKIPS, DATABASES are no longer explicitly set and are not valid
# to be referenced in localConfig.py.

# WARNING: There can only be one version of a model in modelDict. Fcst,
# practice and test databases have to be handled separately because there
# are databases with the same name but different types.  This is ok
# because these databases are defined after any localConfig customizations
# of the normal Fcst database.

# modelDict contains the following keys. Only define what is needed, i.e.,
# it is not required to have every key defined
#   "DB": Definition of the database, i.e., the first value in a dbs entry:
#         ("wrfems", GRID, "", NO,  NO,  3, 0). This must be a tuple. The name
#         in the DB entry must be the same as the model name used as the key
#         into the modelDict variable.
#
#   "Parms" : Definition of the weather element parameters in the database,
#         i.e., the second part of the dbs entry. This is a list of tuples.
#
#   "D2DMODELS" : D2D metadata database name for the source model.
#
#   "INITMODULES': Name of the SmartInit module. It is usually just the
#         name as a string. If the init requires multiple models, use a tuple
#         of ('smartInit name',[list of model names])
#         'INITMODULES': ('Local_WPCGuide', ["HPCGuide","HPCERP","HPCWWD"]),
#
#   "D2DAccumulativeElements" : List of parm names that are accumulative
#
#   "D2DDBVERSIONS" : Number of versions of a D2D model to show in the Weather
#         Element Browser. Defaults to 2 if not supplied.
#
#   "INITSKIPS" : Used to skip specific model cycles.
#
# Example for a model:
#
#   modelDict["CMCreg"]={
#        "DB": ("CMCreg", "GRID", "", NO, NO, 2, 0),
#        "Parms": [([Temp, Td, RH, Wind, WindGust, Sky, MixHgt, TransWind, QPF,
#                    PoP, SnowAmt, SnowRatio], TC3),
#                  ([PoP6, QPF6, QPF6hr, CQPF1],TC6NG),
#                  ([QPF12, PoP12],TC12NG),
#                  ([MinRH], MinRHTC), ([MaxRH], MaxRHTC),
#                  ([MaxT], MaxTTC), ([MinT], MinTTC),
#                 ],
#        "D2DMODELS": "Canadian-Reg",
#        "INITMODULES": "Local_CMCreg",
#        "D2DAccumulativeElements": ["tpgemreg","tprun","tp3hr","tp6hr"],
#        "D2DDBVERSIONS": 3,
#   }
#

# Official, Practice, TestFcst, Test, Restore are all derivations of Fcst and
# are setup after localConfig is processed.
modelDict['Fcst'] = {'DB': Fcst, 'Parms': OFFICIALDBS}

# Model Databases
waveParms=[Period, Period2, SurfHeight, Swell, Swell2, WaveHeight,
           Wind, WindWaveHeight, ]

modelDict['BaseTerrain'] = {
            'DB': ('BaseTerrain', 'GRID', 'EditTopo', YES, NO, 1, 0),
            'Parms': [([StdTopo, GTOPO, PrevTopo], Persistent),
                     ],
            }

modelDict['CRMTopo'] = {
            'D2DDBVERSIONS': 1}

modelDict['ECMWFHiRes'] = {
            'D2DMODELS': 'ECMWF-HiRes',}

modelDict['ENPwave'] = {
            'D2DMODELS': 'ENPWAVE253',
            'DB': ('ENPwave', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [(waveParms, TC6),
                     ],
            }

modelDict['ESTOFS'] = {
            'D2DMODELS': 'estofsEP',
            'DB': ('ESTOFS', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'ESTOFS',
            'Parms': [([AstroTide, StormSurge], TC1),
                     ],
            }

modelDict['ETSS'] = {
            'D2DMODELS': 'ETSS',
            'DB': ('ETSS', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'ETSS',
            'Parms': [([StormSurge, SurgeTide], TC1),
                     ],
            }

modelDict['ETSSHiRes'] = {
            'D2DMODELS': 'ETSS-HiRes',
            'DB': ('ETSSHiRes', 'GRID', '', NO, NO, 2, 0),
            'INITMODULES': 'ETSSHiRes',
            'Parms': [([AstroTide, SurgeTide], TC1),
                     ],                        
             }

for s in ['ALR', 'FWR', 'KRF', 'MSR', 'ORN', 'PTR', 'RHA', 'RSA', 'STR', 'TAR',
          'TIR', 'TUA',]:
    modelDict['FFG'+s] = {'D2DMODELS': 'FFG-'+s}

modelDict['GFS20'] = {
            'D2DMODELS': 'GFS215',
            'D2DAccumulativeElements': ['tp3hr','tp6hr', 'tp', 'cp', 'crain', 'csnow', 'cfrzr', 'cicep'],
            'DB': ('GFS20', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [([Wetflag], FireWx1300TC),
                     ([MaxRH], MaxRHTC),
                     ([MaxT], MaxTTC),
                     ([MinRH], MinRHTC),
                     ([MinT], MinTTC),
                     ([HrsOfSun, InvBurnOffTemp, MarineLayer], LT24),
                     ([DSI, FreeWind, FzLevel, Haines, MixHgt, RH, RHtrend, Sky,
                       SnowLevel, Stability, Td, Temp, TransWind, Ttrend, VentRate,
                       Wind, Wind20ft], TC6),
                     ([CWR, IceAcc, LAL, PoP, QPF, SnowAmt, Weather], TC6NG),
                     ],
            }

modelDict['GFS80'] = {
            'D2DAccumulativeElements': ['tp', 'cp'],
            'D2DMODELS': 'AVN211',
            'DB': ('GFS80', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'GFS80',
            'Parms': STD6_MODEL,
            }

modelDict['GFSLAMPGrid'] = {
            'D2DMODELS': 'GFSLAMPGrid',
            'DB': ('GFSLAMPGrid', 'GRID', '', NO,  NO, 3, 0),
            'INITMODULES': 'GFSLAMPGrid',
            'Parms': [([CigHgt, Sky, Td, Temp, Vis, Wind], TC1),
                     ],
            }

modelDict['GWW'] = {
            'DB': ('GWW', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [(waveParms, TC6),
                     ],
            }

modelDict['GWW233'] = {
            'D2DMODELS': 'GWW233',}

modelDict['GlobalWave'] = {
            'D2DMODELS': 'GlobalWave',
            'DB': ('GlobalWave', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [(waveParms, TC3),
                     ],
            }

modelDict['HIRESWarw'] = {
            'D2DAccumulativeElements': ['tp'],
            'D2DMODELS': 'HiResW-ARW-West',
            'DB': ('HIRESWarw', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'HIRESWarw',
            'Parms': STD3_MODEL,
            }

modelDict['HIRESWnmm'] = {
            'D2DAccumulativeElements': ['tp'],
            'D2DMODELS': 'HiResW-NMM-West',
            'DB': ('HIRESWnmm', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'HIRESWnmm',
            'Parms': STD3_MODEL,
            }

modelDict['HPCERP'] = {
            'D2DAccumulativeElements': ['tpHPCndfd'],
            'D2DDBVERSIONS': 24,
            'D2DMODELS': 'HPCqpfNDFD',}

modelDict['HPCGRID'] = {
            'DB': ('HPCGRID', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [([PoP, SnowAmt], LTMOS),
                     ([MaxT], MaxTTCMOS),
                     ([MinT], MinTTCMOS),
                     ([Sky, Td, Temp, Weather, Wind], TC1),
                     ([QPF], TC6NG),
                     ],
            }

modelDict['HPCGuide'] = {
            'D2DAccumulativeElements': ['pop'],
            'D2DMODELS': 'HPCGuide',
            'DB': ('HPCGuide', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'HPCGuide',
            'Parms': [([MaxT], MaxTTC),
                     ([MinT], MinTTC),
                     ([PoP], TC12NG),
                     ([Sky, Td, Wind], TC6),
                     ],
            }

modelDict['HPCQPF'] = {
            'D2DAccumulativeElements': ['tpHPC'],
            'D2DMODELS': 'HPCqpf',
            'DB': ('HPCQPF', 'GRID', '', NO,  NO, 4, 0),
            'INITMODULES': 'HPCQPF',
            'Parms': [([QPF], TC6NG),
                     ],
            }

modelDict['HRRR'] = {
            'D2DAccumulativeElements': ['tp', 'crain', 'csnow', 'cfrzr', 'cicep'],
            'D2DMODELS': 'HRRR',
            'DB': ('HRRR', 'GRID', '', NO,  NO, 3, 0),
            'INITMODULES': 'HRRR',
            'Parms': [([QPF, RH, Sky, Td, Temp, Wind, WindGust], TC1),
                     ],
            }

modelDict['HWRF'] = {
            'D2DAccumulativeElements': ['tp', 'cp'],
            'D2DMODELS': 'HWRF',}

modelDict['LAPS'] = {
            'D2DAccumulativeElements': ['pc'],
            'D2DDBVERSIONS': 6,
            'D2DMODELS': 'LAPS',
            'DB': ('LAPS', 'GRID', '', YES, NO, 1, 30),
            'INITMODULES': 'LAPS',
            'Parms': [([QPF, Radar, Sky, SnowAmt, Td, Temp, Weather, Wind], TC1),
                     ],
            }

modelDict['MOSGuide'] = {
            'D2DAccumulativeElements': ['pop12hr', 'pop6hr', 'thp12hr', 'thp3hr',
                                       'thp6hr', 'tcc', 'tp6hr', 'tp12hr', 'wgs'],
            'D2DMODELS': 'MOSGuide',
            'DB': ('MOSGuide', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'MOSGuide',
            'Parms': [([MaxT], MaxTTC),
                     ([MinT], MinTTC),
                     ([RH, Td, Temp, Wind], TC1),
                     ([PoP, PoP12, QPF, QPF12, TstmPrb12], TC12NG),
                     ([TstmPrb3], TC3NG),
                     ([PoP6, QPF6, Sky, TstmPrb6, WindGust], TC6NG),
                     ],
            }

modelDict['MSAS'] = {
            'D2DAccumulativeElements': ['tp', 'cp'],
            'D2DDBVERSIONS': 6,
            'D2DMODELS': 'MSAS',
            'DB': ('MSAS', 'GRID', '', YES, NO, 1, 36),
            'INITMODULES': 'MSAS',
            'Parms': [([Td, Temp, Wind], TC1),
                     ],
            }

modelDict['NAHwave4'] = {
            'D2DMODELS': 'NAHwave4',}

modelDict['NAM12'] = {
            'D2DAccumulativeElements': ['tp', 'cp', 'crain', 'csnow', 'cfrzr', 'cicep'],
            'D2DMODELS': 'ETA218',
            'DB': ('NAM12', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'NAM12',
            'Parms': STD3_MODEL,
            }

modelDict['NAM20'] = {
            'D2DAccumulativeElements': ['tp', 'cp'],
            'D2DMODELS': 'mesoEta215',}

modelDict['NAM40'] = {
            'D2DAccumulativeElements': ['tp', 'cp'],
            'D2DMODELS': 'mesoEta212',
            'DB': ('NAM40', 'GRID', '', NO,  NO, 2, 0),
            'Parms': STD3_MODEL,
            }

modelDict['NAM80'] = {
            'D2DAccumulativeElements': ['tp', 'cp'],
            'D2DMODELS': 'ETA',
            'DB': ('NAM80', 'GRID', '', NO,  NO, 2, 0),
            'Parms': STD6_MODEL,
            }

modelDict['NED'] = {
            'D2DDBVERSIONS': 1}

modelDict['NamDNG'] = {
            'D2DMODELS': 'namdng25',
            'DB': ('NamDNG', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'NamDNG',
            'Parms': [([MaxRH], MaxRHTC),
                     ([MaxT], MaxTTC),
                     ([MinRH], MinRHTC),
                     ([MinT], MinTTC),
                     ([PoP12, QPF12], TC12NG),
                     ([MixHgt, RH, Sky, SnowLevel, Td, Temp, TransWind, Vis,
                       Wind, WindGust], TC3),
                     ([MaxRH3, MaxT3, MinT3, PoP, QPF3, SnowAmt], TC3NG),
                     ([PoP6, QPF6, SnowAmt6], TC6NG),
                     ],
            }

modelDict['NationalBlend'] = {
            'D2DAccumulativeElements': ["pop12hr", "pop", "pop6hr", "tp", "ppi1hr", "ppi6hr", "tp1hr", "tp6hr", "thp3hr", "thp6hr"],
            'D2DMODELS': 'NationalBlend',
            'DB': ('NationalBlend', 'GRID', '', NO,  NO, 7, 0),
            'INITMODULES': 'NationalBlend',
            'Parms': [([Temp, Td, RH, Sky, Wind, WindGust, ApparentT], TC1),
                     ([QPF1,PPI01,CloudBasePrimary,Ceiling,Visibility],TC1),
                     ([PoTIP, PoTR, PoTRW, PoTS, PoTSW, PoTZR,],TC1),
                     ([SnowLevel,MaxTwAloft,ProbIcePresent, ProbRefreezeSleet,SnowRatio],TC1),
                     ([PositiveEnergyAloft,NegativeEnergyLowLevel],TC1),
                     ([TstmPrb3],TC3NG),
                     ([TstmPrb6,QPF,PoP6,PPI06],TC6NG),
                     ([MaxT], MaxTTC), ([MinT], MinTTC),
                     ([MaxRH], MaxRHTC), ([MinRH], MinRHTC),([PoP],TC12NG),
                     ],
            }

modelDict['NewTerrain'] = {
            'DB': ('NewTerrain', 'GRID', 'EditTopo', YES, NO, 1, 0),
            'Parms': [([NewTopo], Persistent),
                     ],
            }

modelDict['PWPF'] = {
            'D2DMODELS': 'PWPF',}

modelDict['RFCQPF'] = {
            'D2DAccumulativeElements': ['tpHPC'],
            'D2DMODELS': 'RFCqpf',
            'DB': ('RFCQPF', 'GRID', '', NO,  NO, 4, 0),
            'INITMODULES': 'RFCQPF',
            'Parms': [([QPF], TC6NG),
                     ],
            }

modelDict['RTMA'] = {
            'D2DAccumulativeElements': ['tp'],
            'D2DMODELS': 'RTMA25',
            'DB': ('RTMA', 'GRID', '', YES, NO, 1, 36),
            'INITMODULES': 'RTMA',
            'Parms': RTMAPARMS,
            }

modelDict['RUC13'] = {
            'D2DAccumulativeElements': ['tp', 'cp'],
            'D2DMODELS': 'RUC130',
            'DB': ('RUC13', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'RUC13',
            'INITSKIPS': [1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20, 22, 23],
            'Parms': STD1_MODEL,
            }

modelDict['SAT'] = {
            'DB': ('SAT', 'GRID', '', YES, NO, 1, 12),
            'Parms': [([SatFogE, SatFogW, SatIR11E, SatIR11W, SatIR13E, SatIR13W,
                        SatIR39E, SatIR39W, SatVisE, SatVisW, SatWVE, SatWVW],
                        TC_1M),
                     ],
            }

modelDict['SPC'] = {
            'D2DDBVERSIONS': 8, 'D2DMODELS': 'SPCGuide',}

modelDict['SREF'] = {
            'D2DMODELS': 'SREF212',
            'DB': ('SREF', 'GRID', '', NO,  NO, 3, 0),
            'INITMODULES': 'SREF',
            'Parms': [([Td, Temp, Wind], TC1),
                     ],
            }

modelDict['Satellite'] = {
            'D2DDBVERSIONS': 6,}
# Turn on satellite smartInit only if SATDATA has some entries.
if SATDATA:
    modelDict['Satellite']['INITMODULES'] = 'SAT'

modelDict['TPCProb'] = {
            'D2DDBVERSIONS': 30,
            'D2DMODELS': 'TPCWindProb',
            'DB': ('TPCProb', 'GRID', '', NO,  NO, 30, 0),
            'Parms': [([pwsD34, pwsD64], PWSDTC),
                     ([pwsN34, pwsN64], PWSNTC),
                     ([prob34, prob50, prob64, pws34, pws50, pws64], TC1),
                     ],
            }

modelDict['TPCProbPrelim'] = {
            'D2DDBVERSIONS': 30,
            'D2DMODELS': 'TPCWindProb_Prelim',
            'DB': ('TPCProbPrelim', 'GRID', '', NO,  NO, 30, 0),
            'Parms': [([pwsD34, pwsD64], PWSDTC),
                     ([pwsN34, pwsN64], PWSNTC),
                     ([prob34, prob50, prob64, pws34, pws50, pws64], TC1),
                     ],
            }

modelDict['TPCStormSurge'] = {
            'D2DDBVERSIONS': 1}

modelDict['TPCSurgeProb'] = {
            'D2DMODELS': 'TPCSurgeProb',
            'D2DAccumulativeElements': [
                'Surge10Pct',
                'Surge20Pct',
                'Surge30Pct',
                'Surge40Pct',
                'Surge50Pct',
                'Surge90Pct',
                'PSurge25Ft',
                'PSurge24Ft',
                'PSurge23Ft',
                'PSurge22Ft',
                'PSurge21Ft',
                'PSurge20Ft',
                'PSurge19Ft',
                'PSurge18Ft',
                'PSurge17Ft',
                'PSurge16Ft',
                'PSurge15Ft',
                'PSurge14Ft',
                'PSurge13Ft',
                'PSurge12Ft',
                'PSurge11Ft',
                'PSurge10Ft',
                'PSurge9Ft',
                'PSurge8Ft',
                'PSurge7Ft',
                'PSurge6Ft',
                'PSurge5Ft',
                'PSurge4Ft',
                'PSurge3Ft',
                'PSurge2Ft',
                'PSurge1Ft',
                'PSurge0Ft',
                'Surge10Pctincr',
                'Surge20Pctincr',
                'Surge30Pctincr',
                'Surge40Pctincr',
                'Surge50Pctincr',
                'Surge90Pctincr',
                'PSurge20Ftincr',
                'PSurge19Ftincr',
                'PSurge18Ftincr',
                'PSurge17Ftincr',
                'PSurge16Ftincr',
                'PSurge15Ftincr',
                'PSurge14Ftincr',
                'PSurge13Ftincr',
                'PSurge12Ftincr',
                'PSurge11Ftincr',
                'PSurge10Ftincr',
                'PSurge9Ftincr',
                'PSurge8Ftincr',
                'PSurge7Ftincr',
                'PSurge6Ftincr',
                'PSurge5Ftincr',
                'PSurge4Ftincr',
                'PSurge3Ftincr',
                'PSurge2Ftincr',
                'PSurge1Ftincr',
                'PSurge0Ftincr',
            ],
        }

modelDict['PETSS'] = {
            'D2DMODELS': 'P-ETSS',
            'D2DAccumulativeElements': [
                'Surge10Pct',
                'Surge20Pct',
                'Surge30Pct',
                'Surge40Pct',
                'Surge50Pct',
                'Surge90Pct',
                'Surge10Pctincr',
                'Surge20Pctincr',
                'Surge30Pctincr',
                'Surge40Pctincr',
                'Surge50Pctincr',
                'Surge90Pctincr',
                'PSurge0Ftincr',
                'PSurge1Ftincr',
                'PSurge2Ftincr',
                'PSurge3Ftincr',
                'PSurge4Ftincr',
                'PSurge5Ftincr',
                'PSurge6Ftincr',
                'PSurge7Ftincr',
                'PSurge8Ftincr',
                'PSurge9Ftincr',
                'PSurge10Ftincr',
                'PSurge13Ftincr',
                'PSurge16Ftincr',
                'PSurge0Ft',
                'PSurge1Ft',
                'PSurge2Ft',
                'PSurge3Ft',
                'PSurge4Ft',
                'PSurge5Ft',
                'PSurge6Ft',
                'PSurge7Ft',
                'PSurge8Ft',
                'PSurge9Ft',
                'PSurge10Ft',
                'PSurge13Ft',
                'PSurge16Ft',
                'PSurgeMaxincr',
                'PSurgeMeanincr',
                'PSurgeMinincr',
                'PSurgeMax',
                'PSurgeMean',
                'PSurgeMin',
            ],
        }

modelDict['TPCtcm'] = {
            'DB': ('TPCtcm', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [([HiWind], TC3),
                     ],
            }

modelDict['URMA25'] = {
            'D2DAccumulativeElements': ['tp'],
            'D2DMODELS': 'URMA25',
            'DB': ('URMA25', 'GRID', '', YES, NO, 1, 36),
            'INITMODULES': 'URMA25',
            'Parms': [([MaxRH], MaxRHTC),
                     ([MaxT], MaxTTC),
                     ([MinRH], MinRHTC),
                     ([MinT], MinTTC),
                     ([PressUnc, Pressure, QPE, RH, Sky, SkyUnc, TUnc, Td, TdUnc,
                        Temp, Vis, VisUnc, WDirUnc, WGustUnc, WSpdUnc, Wind,
                        WindGust], TC1),
                     ],
            }

modelDict['WCwave10'] = {
            'D2DMODELS': 'WCwave10',
            'DB': ('WCwave10', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [(waveParms, TC3),
                     ],
            }

modelDict['WCwave4'] = {
            'D2DMODELS': 'WCwave4',
            'DB': ('WCwave4', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [(waveParms, TC3),
                     ],
            }

modelDict['WNAWAVE'] = {
            'DB': ('WNAWAVE', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [(waveParms, TC6),
                     ],
            }

modelDict['WNAWAVE238'] = {
            'D2DMODELS': 'WNAWAVE238',}

modelDict['WNAwave10'] = {
            'D2DMODELS': 'WNAwave10',
            'DB': ('WNAwave10', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [(waveParms, TC3),
                     ],
            }

modelDict['WNAwave4'] = {
            'D2DMODELS': 'WNAwave4',
            'DB': ('WNAwave4', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [(waveParms, TC3),
                     ],
            }

# This list will be used to set up a default ignoreDatabases list. This is shorter than
# listing all models to ignore.
includeOnly=[]
if SID in groups['ALASKA_SITES']:
    modelDict['AKwave4'] = {
            'D2DMODELS': 'AKwave4',
            'D2DDBVERSIONS': 2,
            'DB': ('AKwave4', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [([Period, Period2, Swell, Swell2, WaveHeight, Wind, 
                       WindWaveHgt, WindWavePeriod], TC3),
                     ],
            }

    modelDict['AKwave10'] = {
            'D2DMODELS': 'AKwave10',
            'D2DDBVERSIONS': 2,
            'DB': ('AKwave10', 'GRID', '', NO,  NO, 2, 0),
            'Parms': [([Period, Period2, Swell, Swell2, WaveHeight, Wind, 
                        WindWaveHgt, WindWavePeriod], TC3),
                     ],
            }
    
    updateModelDict(modelDict,'ESTOFS','D2DMODELS', 'estofsAK')
    updateModelDict(modelDict,'ETSS','D2DMODELS', 'ETSS-AK')
    updateModelDict(modelDict,'GFS20','D2DMODELS', 'GFS217')
    updateModelDict(modelDict,'HIRESWarw','D2DMODELS', 'HiResW-ARW-AK')
    updateModelDict(modelDict,'HIRESWnmm','D2DMODELS', 'HiResW-NMM-AK')
    updateModelDict(modelDict,'MOSGuide','D2DMODELS', 'MOSGuide-AK')
    updateModelDict(modelDict,'NAM12','D2DMODELS', 'ETA242')
    updateModelDict(modelDict,'NamDNG','D2DMODELS', 'AK-NamDNG3')
    updateModelDict(modelDict,'NationalBlend','D2DMODELS', 'NationalBlendAK')
    updateModelDict(modelDict,'RTMA','D2DMODELS', 'AK-RTMA3')
    updateModelDict(modelDict,'SREF','D2DMODELS', 'SREF216')
    updateModelDict(modelDict,'URMA','D2DMODELS', 'AK-URMA')
    updateModelDict(modelDict,'RTOFS-Alaska','D2DMODELS', 'RTOFS-Alaska')
    updateModelDict(modelDict,'RTOFS-Alaska','D2DMODELS', 'RTOFS-Alaska')
    updateModelDict(modelDict,'RTOFS-Arctic','D2DMODELS', 'RTOFS-Arctic')
    updateModelDict(modelDict,'RTOFS-Bering','D2DMODELS', 'RTOFS-Bering')
    updateModelDict(modelDict,'RTOFS-GulfAlaska','D2DMODELS', 'RTOFS-GulfAlaska')
    updateModelDict(modelDict,'PETSS','D2DMODELS', 'P-ETSS-AK')
    # Model databases for Alaska
    includeOnly = ['AKwave4', 'AKwave10', 'BaseTerrain', 'CRMTopo', 'ECMWFHiRes', 'ESTOFS', 
                   'ETSS',  'GFS20',  'GWW', 'HIRESWarw', 'HIRESWnmm', 'MOSGuide', 'NAM12', 
                   'NamDNG', 'NationalBlend', 'NED', 'NewTerrain', 'RTMA', 'RTOFS-Alaska', 
                   'RTOFS-Arctic', 'RTOFS-Bering', 'RTOFS-GulfAlaska', 'SAT', 'SREF', 'URMA',
                   'nwpsCG1AER', 'nwpsCG1AFG', 'nwpsCG1AJK', 'nwpsCG1ALU', 'nwpsTrkngCG0AER', 
                   'nwpsTrkngCG0AFG', 'nwpsTrkngCG0AJK', 'nwpsTrkngCG0ALU', 'PETSS',
                  ]

# Hawaii OCONUS
elif SID == "HFO":
    modelDict['GFS75'] = {
            'D2DMODELS': 'AVN225',
            'D2DAccumulativeElements': ['tp', 'cp'],
            'DB': ('GFS75', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'GFS75',
            'Parms': STD6_MODEL,
            }

    updateModelDict(modelDict,'GWW233','D2DMODELS', 'GWW233')
    updateModelDict(modelDict,'GlobalWave','D2DMODELS', 'GlobalWave')
    updateModelDict(modelDict,'RTMA','D2DMODELS', 'HI-RTMA')
    updateModelDict(modelDict,'NamDNG','D2DMODELS', 'HI-NamDNG5')
    updateModelDict(modelDict,'HIRESWarw','D2DMODELS', 'HiResW-ARW-HI')
    updateModelDict(modelDict,'HIRESWnmm','D2DMODELS', 'HiResW-NMM-HI')
    updateModelDict(modelDict,'SPC','D2DMODELS', 'SPCGuide')
    updateModelDict(modelDict,'TPCProb','D2DMODELS', 'TPCWindProb')
    updateModelDict(modelDict,'TPCProbPrelim','D2DMODELS', 'TPCWindProb_Prelim')
    updateModelDict(modelDict,'ECMWFHiRes','D2DMODELS', 'ECMWF-HiRes')
    updateModelDict(modelDict,'RTOFS-Honolulu','D2DMODELS', 'RTOFS-Honolulu')
    updateModelDict(modelDict,'ESTOFS','D2DMODELS', 'estofsHI')
    updateModelDict(modelDict,'MOSGuide','D2DMODELS', 'MOSGuide-HI')
    updateModelDict(modelDict,'NationalBlend','D2DMODELS', 'NationalBlendHI')
    # Model databases for HFO
    includeOnly = ['ECMWFHiRes', 'ESTOFS', 'GFS75', 'GWW233', 'GlobalWave',
                   'HIRESWarw', 'HIRESWnmm', 'MOSGuide', 'NamDNG', 'NationalBlend',
                   'RTMA', 'RTOFS-Honolulu', 'SPC', 'TPCProb', 'TPCProbPrelim', 'nwpsCG1GUM',
                   'nwpsCG1HFO', 'nwpsTrkngCG0GUM', 'nwpsTrkngCG0HFO',
                  ]

# Guam OCONUS
elif SID == "GUM":
    modelDict['GFS75'] = {
            'D2DMODELS': 'AVN225',
            'D2DAccumulativeElements': ['tp', 'cp'],
            'DB': ('GFS75', 'GRID', '', NO,  NO, 2, 0),
            'INITMODULES': 'GFS75',
            'Parms': STD6_MODEL,
            }

    updateModelDict(modelDict,'GlobalWave','D2DMODELS', 'GlobalWave')
    updateModelDict(modelDict,'TPCProb','D2DMODELS', 'TPCWindProb')
    updateModelDict(modelDict,'TPCProbPrelim','D2DMODELS', 'TPCWindProb_Prelim')
    updateModelDict(modelDict,'RTOFS-Guam','D2DMODELS', 'RTOFS-Guam')
    updateModelDict(modelDict,'RTMA','D2DMODELS', 'Guam-RTMA')
    # Model databases for GUM
    includeOnly = ['GFS75', 'GlobalWave', 'RTMA', 'RTOFS-Guam', 'TPCProb',
                   'TPCProbPrelim', 'nwpsCG1GUM', 'nwpsCG1HFO',
                   'nwpsTrkngCG0GUM', 'nwpsTrkngCG0HFO',
                  ]

# San Juan OCONUS
elif SID == "SJU":
    updateModelDict(modelDict,'GFS80','D2DMODELS', 'AVN211')
    updateModelDict(modelDict,'NAM80','D2DMODELS', 'ETA')
    updateModelDict(modelDict,'GWW233','D2DMODELS', 'GWW233')
    updateModelDict(modelDict,'GlobalWave','D2DMODELS', 'GlobalWave')
    updateModelDict(modelDict,'WNAwave10','D2DMODELS', 'WNAwave10')
    updateModelDict(modelDict,'WNAwave4','D2DMODELS', 'WNAwave4')
    updateModelDict(modelDict,'RTMA','D2DMODELS', 'PR-RTMA')
    updateModelDict(modelDict,'HIRESWarw','D2DMODELS', 'HiResW-ARW-SJU')
    updateModelDict(modelDict,'HIRESWnmm','D2DMODELS', 'HiResW-NMM-SJU')
    updateModelDict(modelDict,'SPC','D2DMODELS', 'SPCGuide')
    updateModelDict(modelDict,'TPCProb','D2DMODELS', 'TPCWindProb')
    updateModelDict(modelDict,'TPCProbPrelim','D2DMODELS', 'TPCWindProb_Prelim')
    updateModelDict(modelDict,'ECMWFHiRes','D2DMODELS', 'ECMWF-HiRes')
    updateModelDict(modelDict,'RTOFS-Atlantic','D2DMODELS', 'RTOFS-Atlantic')
    updateModelDict(modelDict,'ESTOFS','D2DMODELS', 'estofsPR')
    updateModelDict(modelDict,'NAHwave4','D2DMODELS', 'NAHwave4')
    updateModelDict(modelDict,'GFS20','D2DMODELS', 'GFS20-PRICO')
    updateModelDict(modelDict,'NationalBlend','D2DMODELS', 'NationalBlendPR')
    # Model databases for SJU
    includeOnly = ['ECMWFHiRes', 'ESTOFS', 'GFS20', 'GFS80', 'GWW233',
                   'GlobalWave', 'HIRESWarw', 'HIRESWnmm', 'NAHwave4', 'NAM80',
                   'NationalBlend', 'RTMA', 'RTOFS-Atlantic', 'SPC', 'TPCProb',
                   'TPCProbPrelim', 'WNAwave10', 'WNAwave4',
                   'nwpsCG1JAX', 'nwpsCG1KEY', 'nwpsCG1MFL', 'nwpsCG1MLB', 'nwpsCG1SJU',
                   'nwpsTrkngCG0JAX', 'nwpsTrkngCG0KEY', 'nwpsTrkngCG0MFL',
                   'nwpsTrkngCG0MLB', 'nwpsTrkngCG0SJU',
                  ]

# East CONUS changes from default modelDict
elif SID in groups['CONUS_EAST_SITES']:
    updateModelDict(modelDict,'ESTOFS','D2DMODELS', 'estofsUS')
    updateModelDict(modelDict,'HIRESWarw','D2DMODELS', 'HiResW-ARW-East')
    updateModelDict(modelDict,'HIRESWnmm','D2DMODELS', 'HiResW-NMM-East')

if SID in groups['GreatLake_SITES']:
    modelDict['GLERL'] = {
            'D2DMODELS': 'GLERL',
            'DB': ('GLERL', 'GRID', '', 0, 0, 2, 0),
            'Parms': [([Period, Swell, WaveHeight], TC1),
                     ]
            }

    modelDict['GLWN'] = {'D2DMODELS': 'GLWN'}

# NWPS configuration.
if SID in ['AFC', 'AER', 'AFG', 'AJK', 'ALU', 'AVAK']:
    nwpsSites = ['AER', 'AFG', 'AJK', 'ALU',]
elif SID in ['GUM', 'HFO',]:
    nwpsSites = ['GUM', 'HFO',]
elif SID == "SJU":
    nwpsSites = ['SJU', 'MFL', 'KEY', 'MLB', 'JAX']
elif SID in ['CAR', 'GYX', 'BOX', 'OKX', 'PHI', 'LWX', 'AKQ', 'MHX', 'ILM', 'CHS',
             'BRO', 'CRP', 'HGX', 'LCH', 'LIX', 'MOB', 'TAE', 'TBW', 'KEY', 'MFL',
             'MLB', 'JAX',]:
     nwpsSites = ['CAR', 'GYX', 'BOX', 'OKX', 'PHI', 'LWX', 'AKQ', 'MHX', 'ILM', 'CHS',
                  'BRO', 'CRP', 'HGX', 'LCH', 'LIX', 'MOB', 'TAE', 'TBW', 'KEY', 'MFL',
                  'MLB', 'JAX', 'SJU',]
elif SID in ['SEW', 'PQR', 'MFR', 'EKA', 'MTR', 'LOX', 'SGX',]:
    nwpsSites = ['SEW', 'PQR', 'MFR', 'EKA', 'MTR', 'LOX', 'SGX',]
else:
    nwpsSites = []

for s in nwpsSites:
    name='nwpsCG1%s' % s
    modelDict[name] = {
            'DB': (name, 'GRID', '', NO,  NO, 2, 0),
            'D2DMODELS': name,
            'INITMODULES': name,
            'Parms': nwpsCG1_MODEL,
            }
    name='nwpsTrkngCG0%s' % s
    modelDict[name] = {
            'DB': (name, 'GRID', '', NO,  NO, 2, 0),
            'D2DMODELS': name,
            'INITMODULES': name,
            'Parms': nwpsTrkngCG0_MODEL,
            }
# This list will be used to set up a default ignoreDatabases list. This is shorter than
# listing all models to ignore. Usually only set up for sites that aren't CONUS WFOs
# includeOnly is not designed to be changed by localConfig.
if includeOnly:
    for m in sorted(modelDict.keys()):
        if m not in includeOnly and 'D2DMODELS' in modelDict[m]:
            ignoreDatabases.append(m)

# END modelDict initial set up
#------------------------------------------------------------------------------
# Add in optional parms to Fcst parm def
if SID in groups['powt']:
    addPowt(modelDict)

if SID in groups['winterProbs']:
    addWinterWeatherProbs(modelDict)

if SID in groups['rainfallProbs']:
    addRainfallProbs(modelDict)

D2DMODELS=[]
D2DDBVERSIONS={}
D2DAccumulativeElements={}
INITMODULES={}
INITSKIPS={}

localParms = []
localISCParms = []
localISCExtraParms = []
localLogFile = ''

if not BASELINE and siteImport('localConfig'):
    localParms = getattr(localConfig, 'parms', [])
    localISCParms = getattr(localConfig, 'parmsISC', [])
    localISCExtraParms = getattr(localConfig, 'extraISCparms', [])
    localLogFile = getattr(localConfig, 'logFile', '')
    modelDict['Fcst']['Parms'] += localParms
    #ensure office type is set properly in localConfig SITES[]
    if len(SITES[GFESUITE_SITEID]) == 5:
        a = list(SITES[GFESUITE_SITEID])
        a.append(myOfficeType)
        SITES[GFESUITE_SITEID] = tuple(a)
    else:
        myOfficeType = SITES[GFESUITE_SITEID][5]  #probably from localConfig

# Instantiate settings from modelDict
db=dbConfig(modelDict)
db.addConfiguredModels(ignoreDatabases)
DATABASES = db.dbs
D2DMODELS = db.D2DMODELS
D2DDBVERSIONS = db.D2DDBVERSIONS
D2DAccumulativeElements = db.D2DAccumulativeElements
INITMODULES = db.INITMODULES
INITSKIPS = db.INITSKIPS
OFFICIALDBS=list(modelDict['Fcst']['Parms'])

# Create Practice and test databases from Fcst
DATABASES.append((Official, modelDict['Fcst']['Parms'])),
DATABASES.append((Practice, modelDict['Fcst']['Parms'])),
DATABASES.append((TestFcst, modelDict['Fcst']['Parms'])),
DATABASES.append((Test, modelDict['Fcst']['Parms'])),

for entry in AdditionalISCRouting:
    (parmList, dbName, editAreaPrefix) = entry
    parmList = list(parmList)
    addedIscDbDefinition = (dbName, ) + ISC[1:]
    addedIscParms = [(parmList, TC1)]
    DATABASES.append((addedIscDbDefinition, addedIscParms))

# Intersite coordination database parameter groupings, based on
# OFFICIALDBS, but time constraint is always TC1
for wes, tc in (OFFICIALDBS + localISCParms):
    ISCPARMS.append((wes, TC1))

# We also add in any extraISCparms as needed, but only for office
# types other than our own.
for wes, officeType in (EXTRA_ISC_PARMS + localISCExtraParms):
    if myOfficeType == officeType:
        continue
    if type(officeType) != str:
        raise TypeError, "Office type not a str: " + `officeType`
    else:
        if officeType not in VALID_OFFICE_TYPES:
            raise ValueError, "Office type: " + str(officeType) + " does not match any of the following: [" + (', '.join(VALID_OFFICE_TYPES)) + "]"
    for we in wes:
        wecopy = list(we)
        wecopy[0] = wecopy[0] + officeType  #rename the weather element
        wecopy = tuple(wecopy)
        ISCPARMS.append(([wecopy], TC1))

# Restore database parameter groupings (based on OFFICIALDBS, but TC1)
RESTOREPARMS = []
for wes, tc in modelDict['Fcst']['Parms']:
    RESTOREPARMS.append((wes, TC1))

# Now add the ISC and Restore databases to the DATABASES groupings
DATABASES.append((Restore, RESTOREPARMS))
DATABASES.append((ISC, ISCPARMS))


#D logfp=open('/localapps/logs/serverConfig2.log','w')
#D logfp.write('DATABASE names:\n')
#D for m in sorted(DATABASES):
#D     logfp.write('%s\n' % m[0][0])
#D logfp.write('\n\nDATABASES\n')
#D pprint.pprint(sorted(DATABASES),logfp,width=130)
#D logfp.write('\n\nINITMODULES\n')
#D pprint.pprint(INITMODULES,logfp,width=130)
#D logfp.write('\n\nD2DMODELS\n')
#D pprint.pprint(D2DMODELS,logfp,width=130)
#D logfp.close()

doIt()

#D logfp=open('/localapps/logs/SC_MD2.py','w')
#D modelDict=createModelDict(locals(),DATABASES,D2DMODELS,D2DDBVERSIONS,D2DAccumulativeElements,
#D                   INITMODULES,INITSKIPS,logfp)
#D logfp.close()
if localLogFile:
    printServerConfig(sys.modules[__name__],vars(localConfig),localLogFile)
#D scfp.close()
