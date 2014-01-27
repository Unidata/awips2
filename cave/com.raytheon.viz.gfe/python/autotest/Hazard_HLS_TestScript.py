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
# Hazard_HLS_TestScript
#
# Author:
# ----------------------------------------------------------------------------


useTest = [
"""def _useTestTCP(self):
        return True
        ##return False
""",
"""def _inlandAreas(self):
        return [
            "FLZ052", "FLZ056", "FLZ057", "FLZ061", "FLZ043",
            ]
""",
"""def _coastalAreas(self):
        return [
            "FLZ039", "FLZ042", "FLZ048", "FLZ049", "FLZ050", "FLZ051", "FLZ055", "FLZ060",
            "FLZ062", "FLZ065",
            ]
""",
"""def _marineAreas(self):
        return [
            "GMZ830", "GMZ850", "GMZ853", "GMZ856", "GMZ856", "GMZ870","GMZ873","GMZ876"
            ]
""",
"""def _cwa(self):
        return "TBW"
""",
"""def _cwa_descriptor(self):
        return "CENTRAL WEST FLORIDA"
""",
"""def _maor_descriptor(self):
        return "WEST CENTRAL FLORIDA AND THE GULF OF MEXICO"
""",
"""def _cwa_maor_descriptor(self):
        return "WEST FLORIDA AND THE GULF OF MEXICO"
""",
"""def _localReferencePoints(self):
        # Give the name and lat/lon for each local reference point
        return [
                ("Tampa Bay, FL", (28.01, -82.48)),
                ("Cape Coral, FL", (26.63, -82.00)),
                ("Lakeland, FL", (28.04, -81.95)),
                ("Sarasota, FL", (27.37, -82.55)),
                ]
""",
"""def _localReferencePoints_defaults(self):
        # Give a list of the local reference point names to be
        #  turned on by default
        return ["Tampa Bay, FL", "Sarasota, FL"]
"""
]

## We are setting up the segments which would result from combinations.
##  Note that GMZ876 is not include in the combos, so ends up in a segment
##  by itself.

##     Segments [['FLZ051', 'FLZ052', 'GMZ830'],
##               ['GMZ850', 'FLZ042', 'FLZ039', 'FLZ043'],
##               ['FLZ060', 'FLZ061', 'GMZ853'],
##               ['FLZ048', 'FLZ049', 'FLZ050'],
##               ['FLZ062', 'GMZ856', 'FLZ057'],
##               ['FLZ065', 'GMZ876']]

##     Combos [
##         (['FLZ039', 'FLZ042', 'FLZ043'], 'Region01'),
##         (['GMZ850'], 'Region02'),
##         (['FLZ048', 'FLZ049', 'FLZ050'], 'Region03'),
##         (['FLZ052'], 'Region04'), (['FLZ051', 'GMZ830'], 'Region05'),
##         (['FLZ060', 'FLZ061', 'GMZ853'], 'Region06'),
##         (['FLZ062', 'GMZ856', 'FLZ055', 'FLZ057', 'FLZ056'], 'Region07'),
##         (['FLZ065'], 'Region08')]

##     New segments [['FLZ039', 'FLZ042', 'FLZ043'],
##                   ['GMZ850'],
##                   ['FLZ048', 'FLZ049', 'FLZ050'],
##                   ['FLZ052'], ['FLZ051', 'GMZ830'],
##                   ['FLZ060', 'FLZ061', 'GMZ853'],
##                   ['FLZ062', 'GMZ856', 'FLZ057'],
##                   ['FLZ065'],
##                   ['GMZ876']]

segmentSetUp = [
        (1, "HU_W", ['FLZ039', 'FLZ042', 'FLZ043']), 
        (2, "HU_W", ['GMZ850']),
        (3, "HU_A", ['FLZ048', 'FLZ049', 'FLZ050']),
        (4, "HU_A_TR_W", ['FLZ052']),
        (5, "HU_A_TR_W", ['FLZ051', 'GMZ830']),
        (6, "TR_W", ['FLZ060', 'FLZ061', 'GMZ853']),
        (7, "TR_A", ['FLZ062', 'GMZ856', 'FLZ057']),
        (8, "HU_S", ['FLZ065']),
        (9, "HU_S", ['GMZ876']),
        ]
        
def makeTestCases():
    # 9 segments with these hazards and areas
    segments = [hazard for segNum, hazard, areas in segmentSetUp]
    contexts = ["NonEvent","PreEvent","Abbreviated","Watch","Warning","Conditions",
                "PostEvent","ExtraTropical"]
    uncertainty = ['Low', 'Average', 'High']
    sitDict = {
        "HU_W": ['Abbreviated', 'Warning', 'Conditions', 'ExtraTropical'],
        "TR_W": ['Abbreviated', 'Warning', 'Conditions', 'ExtraTropical'],
        "HU_A": ['Abbreviated', 'Watch'],
        "TR_A": ['Abbreviated', 'Watch'],
        "HU_A_TR_W": ['Abbreviated', 'Watch', 'Warning', 'Conditions', 'ExtraTropical'],
        "HU_S": ['NonEvent', 'PreEvent', 'PostEvent'],
        }
    scenDict = {
                "NonEvent": ["NonEvent"],
                "PreEvent": ["Advancing", "Peripheral", "InSitu"],
                "Abbreviated": ["FirstIssuance"],
                "Watch": ["Advancing", "Peripheral", "InSitu"],
                "Warning": ["Advancing", "Peripheral", "InSitu"],
                "Conditions": ["Imminent", "Ongoing", "Diminishing"],
                "PostEvent": ["Immediate", "LongTerm"],
                "ExtraTropical": ["InSitu", "Completed"],
                }
    segCaseDict = {}
    maxCases = 0
    for i in range(len(segments)):
        hazard = segments[i]
        segCases = []
        for sit in sitDict[hazard]:
            scenarios = scenDict[sit]
            for scen in scenDict[sit]:
                segCases.append((sit, scen))
        segCaseDict[i] = segCases
        if len(segCases) > maxCases:
            maxCases = len(segCases)
    #print "segCases\n", segCaseDict
    #print "maxCases", maxCases
            
    ##    # A test case is :
    ##     #   EventContext, Uncertainty, segmentInfo, checkStrs
    ##     #   segmentInfo is (segNum, situation, scenario) --
    ##            we will assume all sections included
    ##     # Generate a list of tests that cycles through all the situations and scenarios
    ##     #   for each segment
    testCases = []
    caseIndex = 0
    for i in range(maxCases):
        # Make a test case
        EventContext = getValue(contexts, i)
        Uncertainty = getValue(uncertainty, i)
        segs = []
        for segNum in range(len(segments)):
            segCaseList = segCaseDict[segNum]
            segCase = getValue(segCaseList, i)
            sit, scen = segCase
            segs.append((segNum+1, sit, scen))
        #checkStrs = checkStrings[i+1]  # Need to fix this later
        checkStrs = ["NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL"]
        testCase = (EventContext, Uncertainty, segs, checkStrs)
        testCases.append(testCase)
    #printTestCases(testCases)
    return testCases

def printTestCases(testCases):
    print "Test cases: Event Context, Uncertainty, Segments"
    ind = 1
    for testCase in testCases:
        ec, un, segs, checkStrs= testCase
        print `ind`, ec, un, "Segments:"
        for seg in segs:
            print '   ', seg, segmentSetUp[segs.index(seg)]
        print '\n'
        ind+= 1
    return testCases
                
def getValue(list, index):
    # If index too big, cycle around
    index = index%len(list)
    return list[index]

def makeScript(testCase, testNum, createData=None):
    # Use template script to make a script for this test case
    #  Test case is (EventContext, Uncertainty, segments)
    name = "HLS_"+ `testNum`
    ec, un, segs, checkStrs = testCase
    if testNum > 2: selectedSections=True
    else: selectedSections = False
    varDict, commentary = cmdLineVars(ec, un, segs, includeCommentary=True,
                                      selectedSections=selectedSections)
    varDict = str(varDict)
    if createData is None:
        createData = createDataGrids()
    script = {
     "name":name,
     "commentary": commentary,
     "productType" : "Hazard_HLS", 
     "gridsStartTime": None,     
     "createGrids": createHazardGrids() + createData,
     "cmdLineVars" : varDict,     
     "fileChanges": [("Hazard_HLS_<site>_Overrides", "TextUtility", "add", useTest, "undo")],
     "checkStrings" : checkStrs,
     }
    return script
     
def makeScripts(testNum=None, createData=None):
    testCases = makeTestCases()
    scripts = []
    if createData is None:
        createData = createDataGrids()
    if testNum is None:
        testNum = 0
    for testCase in testCases:
        testNum+=1
        scripts.append(makeScript(testCase, testNum, createData=createData))
    return scripts


def cmdLineVars(EventContext='NonEvent', Uncertainty='N/A', segmentList=None,
                StormInfo='TCPAT5', includeCommentary=False, selectedSections=False):
    sections = [
        ('PrecautionaryPreparednessActions', None, 0, None),
        ('Probability', None, 0, None),
        ('Wind', '', 0, None),
        ('StormSurgeTide', '', 0, None),
        ('InlandFlooding', '', 0, None),
        ('Tornadoes', '', 0, None)
        ]
    selectedSections = [
        ('PrecautionaryPreparednessActions', None, 0, None),
        ('Probability', None, 0, None),
        ('Wind', '', 0, None),
        #('StormSurgeTide', '', 0, None),
        #('InlandFlooding', '', 0, None),
        #('Tornadoes', '', 0, None)
        ]
    extraInfo = {'usePrev_HU_S_Headline': None, 'userHeadline_HU_S': None}    
    if segmentList is None:
        segmentList = [
            (1, 'Warning', 'Advancing'),
            (2, 'Conditions', 'Imminent'),
            (3, 'Watch', 'Peripheral'),
            (4, 'ExtraTropical', 'InSitu'),
            (5, 'Abbreviated', 'FirstIssuance'),
            (6, 'Abbreviated', 'FirstIssuance'),
            (7, 'Abbreviated', 'FirstIssuance'),
            (8, 'NonEvent', 'NonEvent'),
            (9, 'PreEvent', 'InSitu'),
            ]
    segTemplate = [(segNum, areas) for segNum, hazard, areas in segmentSetUp]
    commentary = EventContext +" " + Uncertainty + "\n"
    if selectedSections:
        sections = selectedSections
    segs = []
    for i in range(len(segTemplate)):
        segNum, areas = segTemplate[i]
        segNum, sit, scen = segmentList[i]
        sitStr = sit.ljust(15)
        scenStr = scen.ljust(15)
        commentary+= "   " +`segNum`+" "+sitStr+" "+scenStr+" "+`segmentSetUp[segNum-1]` + "\n"
        segs.append((segNum, areas, sit, scen, sections, extraInfo))
    varDict =  {
       ('StormInfo_entry:', 'StormInfo_entry'): '',
       ('OverviewEditMode:', 'OverviewEditMode'): 'CreateFromGUI',
       ('MainHeadline_entry:', 'MainHeadline_entry'): '',
       ('NextUpdate:', 'NextUpdate'): 'Shortly',
       ('EventContext:', 'EventContext'): EventContext,
       ('StormInfo:', 'StormInfo'): StormInfo,
       ('Uncertainty:', 'Uncertainty'): Uncertainty,
       ('Issued By', 'issuedBy'): None,
       ('MainHeadline:', 'MainHeadline'): 'Enter',
       ('LocalReferencePoints:', 'LocalReferencePoints'): [('Tampa Bay, FL', (28.01, -82.48)), ('Sarasota, FL', (27.37, -82.55))],    
       ('segments:', 'segments'): segs,
       ('NextUpdate_entry:', 'NextUpdate_entry'): ''
       }
    if includeCommentary:
        return varDict, commentary
    return varDict


def createHazardGrids():
    return [
          ("Fcst", "Hazards", "DISCRETE", 0,24,"<None>", "all"),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.W",["FLZ039","FLZ042","FLZ043","GMZ850"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.A",["FLZ048", "FLZ049", "FLZ050"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TR.W^HU.A",["FLZ051", "FLZ052", "GMZ830"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TR.W^FF.A",["FLZ061","FLZ060","GMZ853"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TR.A", ["FLZ062","FLZ057","GMZ856"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.S^CF.W", ["GMZ876","FLZ065"]),
          ]

def createDataGrids(maxWind=60, maxGust=75, prob34=20, prob64=10,
                    timeMax_pws34int=(30,36), timeMax_pws64int=(42,48)):
    return [
          ("Fcst", "prob34",  "SCALAR", 0, 120, prob34, "all"),
          ("Fcst", "prob64",  "SCALAR", 0, 120, prob64, "all"),

          ("Fcst", "Wind", "VECTOR", -6, 0, (10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 0, 6, (10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 6, 12, (20, "W"), "all"),
          # Trop begin
          ("Fcst", "Wind", "VECTOR", 12, 18, (35, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 18, 24, (40, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 24, 30, (50, "W"), "all"),
          # Hurricane begin
          ("Fcst", "Wind", "VECTOR", 30, 36, (maxWind-10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 36, 42, (maxWind, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 42, 48, (maxWind, "W"), "all"),
          # Hurricane end
          ("Fcst", "Wind", "VECTOR", 48, 54, (50, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 54, 60, (35, "W"), "all"),
          # Trop end
          ("Fcst", "Wind", "VECTOR", 60, 66, (10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 66, 120, (10, "W"), "all"),

          ("Fcst", "WindGust", "SCALAR", -6, 120, maxGust, "all"),
          
          ("Fcst", "pws34int", "SCALAR", 0, 6, 10, "all"),
          ("Fcst", "pws34int", "SCALAR", 6, 12, 10, "all"),
          ("Fcst", "pws34int", "SCALAR", 12, 18, 20, "all"),
          ("Fcst", "pws34int", "SCALAR", 18, 24, 30, "all"),
          ("Fcst", "pws34int", "SCALAR", 24, 30, 40, "all"),
          ("Fcst", "pws34int", "SCALAR", 30, 36, 80, "all"),
          ("Fcst", "pws34int", "SCALAR", 36, 42, 80, "all"),
          ("Fcst", "pws34int", "SCALAR", 42, 48, 60, "all"),
          ("Fcst", "pws34int", "SCALAR", 48, 54, 40, "all"),
          ("Fcst", "pws34int", "SCALAR", 54, 60, 10, "all"),
          ("Fcst", "pws34int", "SCALAR", 60, 66, 10, "all"),
          ("Fcst", "pws34int", "SCALAR", 66, 120, 10, "all"),

          ("Fcst", "pws64int", "SCALAR", 0, 6, 10, "all"),
          ("Fcst", "pws64int", "SCALAR", 6, 12, 20, "all"),
          ("Fcst", "pws64int", "SCALAR", 12, 18, 20, "all"),
          ("Fcst", "pws64int", "SCALAR", 18, 24, 30, "all"),
          ("Fcst", "pws64int", "SCALAR", 24, 30, 40, "all"),
          ("Fcst", "pws64int", "SCALAR", 30, 36, 60, "all"),
          ("Fcst", "pws64int", "SCALAR", 36, 42, 60, "all"),
          ("Fcst", "pws64int", "SCALAR", 42, 48, 80, "all"),
          ("Fcst", "pws64int", "SCALAR", 48, 54, 40, "all"),
          ("Fcst", "pws64int", "SCALAR", 54, 60, 10, "all"),
          ("Fcst", "pws64int", "SCALAR", 60, 66, 10, "all"),
          ("Fcst", "pws64int", "SCALAR", 66, 120, 10, "all"),
          ]


##  Tests 1-12                         have data values:
##         maxWind==60, maxGust=75, prob34=50, prob64=20,

##  Tests 12-24 (repeat 1-12 segments) have data values:
##         maxWind=75, maxGust=90, prob34=20, prob64=10,

##     Test cases: Event Context, Uncertainty, Segments
##     1 NonEvent Low Segments:
##         (1, 'Abbreviated', 'FirstIssuance') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Abbreviated', 'FirstIssuance') (2, 'HU_W', ['GMZ850'])
##         (3, 'Abbreviated', 'FirstIssuance') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Abbreviated', 'FirstIssuance') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Abbreviated', 'FirstIssuance') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Abbreviated', 'FirstIssuance') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Abbreviated', 'FirstIssuance') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'NonEvent', 'NonEvent') (8, 'HU_S', ['FLZ065'])
##         (9, 'NonEvent', 'NonEvent') (9, 'HU_S', ['GMZ876'])


##     2 PreEvent Average Segments:
##         (1, 'Warning', 'Advancing') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Warning', 'Advancing') (2, 'HU_W', ['GMZ850'])
##         (3, 'Watch', 'Advancing') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Watch', 'Advancing') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Watch', 'Advancing') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Warning', 'Advancing') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Watch', 'Advancing') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PreEvent', 'Advancing') (8, 'HU_S', ['FLZ065'])
##         (9, 'PreEvent', 'Advancing') (9, 'HU_S', ['GMZ876'])


##     3 Abbreviated High Segments:
##         (1, 'Warning', 'Peripheral') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Warning', 'Peripheral') (2, 'HU_W', ['GMZ850'])
##         (3, 'Watch', 'Peripheral') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Watch', 'Peripheral') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Watch', 'Peripheral') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Warning', 'Peripheral') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Watch', 'Peripheral') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PreEvent', 'Peripheral') (8, 'HU_S', ['FLZ065'])
##         (9, 'PreEvent', 'Peripheral') (9, 'HU_S', ['GMZ876'])


##     4 Watch Low Segments:
##         (1, 'Warning', 'InSitu') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Warning', 'InSitu') (2, 'HU_W', ['GMZ850'])
##         (3, 'Watch', 'InSitu') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Watch', 'InSitu') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Watch', 'InSitu') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Warning', 'InSitu') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Watch', 'InSitu') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PreEvent', 'InSitu') (8, 'HU_S', ['FLZ065'])
##         (9, 'PreEvent', 'InSitu') (9, 'HU_S', ['GMZ876'])


##     5 Warning Average Segments:
##         (1, 'Conditions', 'Imminent') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Conditions', 'Imminent') (2, 'HU_W', ['GMZ850'])
##         (3, 'Abbreviated', 'FirstIssuance') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Warning', 'Advancing') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Warning', 'Advancing') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Conditions', 'Imminent') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Abbreviated', 'FirstIssuance') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PostEvent', 'Immediate') (8, 'HU_S', ['FLZ065'])
##         (9, 'PostEvent', 'Immediate') (9, 'HU_S', ['GMZ876'])


##     6 Conditions High Segments:
##         (1, 'Conditions', 'Ongoing') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Conditions', 'Ongoing') (2, 'HU_W', ['GMZ850'])
##         (3, 'Watch', 'Advancing') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Warning', 'Peripheral') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Warning', 'Peripheral') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Conditions', 'Ongoing') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Watch', 'Advancing') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PostEvent', 'LongTerm') (8, 'HU_S', ['FLZ065'])
##         (9, 'PostEvent', 'LongTerm') (9, 'HU_S', ['GMZ876'])


##     7 PostEvent Low Segments:
##         (1, 'Conditions', 'Diminishing') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Conditions', 'Diminishing') (2, 'HU_W', ['GMZ850'])
##         (3, 'Watch', 'Peripheral') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Warning', 'InSitu') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Warning', 'InSitu') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Conditions', 'Diminishing') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Watch', 'Peripheral') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'NonEvent', 'NonEvent') (8, 'HU_S', ['FLZ065'])
##         (9, 'NonEvent', 'NonEvent') (9, 'HU_S', ['GMZ876'])


##     8 ExtraTropical Average Segments:
##         (1, 'ExtraTropical', 'InSitu') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'ExtraTropical', 'InSitu') (2, 'HU_W', ['GMZ850'])
##         (3, 'Watch', 'InSitu') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Conditions', 'Imminent') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Conditions', 'Imminent') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'ExtraTropical', 'InSitu') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Watch', 'InSitu') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PreEvent', 'Advancing') (8, 'HU_S', ['FLZ065'])
##         (9, 'PreEvent', 'Advancing') (9, 'HU_S', ['GMZ876'])


##     9 NonEvent High Segments:
##         (1, 'ExtraTropical', 'Completed') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'ExtraTropical', 'Completed') (2, 'HU_W', ['GMZ850'])
##         (3, 'Abbreviated', 'FirstIssuance') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Conditions', 'Ongoing') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Conditions', 'Ongoing') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'ExtraTropical', 'Completed') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Abbreviated', 'FirstIssuance') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PreEvent', 'Peripheral') (8, 'HU_S', ['FLZ065'])
##         (9, 'PreEvent', 'Peripheral') (9, 'HU_S', ['GMZ876'])


##     10 PreEvent Low Segments:
##         (1, 'Abbreviated', 'FirstIssuance') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Abbreviated', 'FirstIssuance') (2, 'HU_W', ['GMZ850'])
##         (3, 'Watch', 'Advancing') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'Conditions', 'Diminishing') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'Conditions', 'Diminishing') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Abbreviated', 'FirstIssuance') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Watch', 'Advancing') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PreEvent', 'InSitu') (8, 'HU_S', ['FLZ065'])
##         (9, 'PreEvent', 'InSitu') (9, 'HU_S', ['GMZ876'])


##     11 Abbreviated Average Segments:
##         (1, 'Warning', 'Advancing') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Warning', 'Advancing') (2, 'HU_W', ['GMZ850'])
##         (3, 'Watch', 'Peripheral') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'ExtraTropical', 'InSitu') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'ExtraTropical', 'InSitu') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Warning', 'Advancing') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Watch', 'Peripheral') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PostEvent', 'Immediate') (8, 'HU_S', ['FLZ065'])
##         (9, 'PostEvent', 'Immediate') (9, 'HU_S', ['GMZ876'])


##     12 Watch High Segments:
##         (1, 'Warning', 'Peripheral') (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
##         (2, 'Warning', 'Peripheral') (2, 'HU_W', ['GMZ850'])
##         (3, 'Watch', 'InSitu') (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
##         (4, 'ExtraTropical', 'Completed') (4, 'HU_A_TR_W', ['FLZ052'])
##         (5, 'ExtraTropical', 'Completed') (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
##         (6, 'Warning', 'Peripheral') (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
##         (7, 'Watch', 'InSitu') (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
##         (8, 'PostEvent', 'LongTerm') (8, 'HU_S', ['FLZ065'])
##         (9, 'PostEvent', 'LongTerm') (9, 'HU_S', ['GMZ876'])





scripts = [   
     {
     "name":"HLS_segmentation_1", 
     "productType" : "Hazard_HLS", 
     "commentary": """Segmentation testing of HLS product.
NonEvent N/A
   1 'Warning',       'Advancing'       (1, 'HU_W', ['FLZ039', 'FLZ042', 'FLZ043'])
   2 'Conditions',    'Imminent'        (2, 'HU_W', ['GMZ850'])
   3 'Watch',         'Peripheral'      (3, 'HU_A', ['FLZ048', 'FLZ049', 'FLZ050'])
   4 'ExtraTropical', 'InSitu'          (4, 'HU_A_TR_W', ['FLZ052'])
   5 'Abbreviated',   'FirstIssuance'   (5, 'HU_A_TR_W', ['FLZ051', 'GMZ830'])
   6 'Abbreviated',   'FirstIssuance'   (6, 'TR_W', ['FLZ060', 'FLZ061', 'GMZ853'])
   7 'Abbreviated',   'FirstIssuance'   (7, 'TR_A', ['FLZ062', 'GMZ856', 'FLZ057'])
   8 'NonEvent',      'NonEvent'        (8, 'HU_S', ['FLZ065'])
   9 'PreEvent',      'InSitu'          (9, 'HU_S', ['GMZ876'])
             """,
     "gridsStartTime": None,     
     "createGrids": createHazardGrids() + createDataGrids(),
     "cmdLineVars" :str(cmdLineVars()),     
     "fileChanges": [("Hazard_HLS_<site>_Overrides", "TextUtility", "add", useTest, "undo")],
     "checkStrings" : [
        #"HURRICANE LOCAL STATEMENT",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       ],
     },

     {
     "name":"HLS_segmentation_2", 
     "productType" : "Hazard_HLS", 
     "commentary": """Segmentation testing of HLS product. -- ETN codes""",
     "gridsStartTime": None,     
     "createGrids": createDataGrids() + [
          ("Fcst", "Hazards", "DISCRETE", 0,24,"<None>", "all"),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.W:1001",["FLZ039"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HI.W",["FLZ043"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.W",["GMZ830"]),            
          ],
     "cmdLineVars" :str(cmdLineVars()),     
     "fileChanges": [("Hazard_HLS_<site>_Overrides", "TextUtility", "add", useTest, "undo")],
     "checkStrings" : [
        #"HURRICANE LOCAL STATEMENT",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       ],
     },

     {
     "name":"HLS_generalTests_1", 
     "productType" : "Hazard_HLS", 
     "commentary": """Looks at Overview Watches/Warnings and Wind Timing reporting""",
     "gridsStartTime": None,     
     "createGrids": createDataGrids() + [
          ("Fcst", "Hazards", "DISCRETE", 0,24,"<None>", "all"),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.W",["FLZ039"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HI.W",["FLZ043"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TR.W",["GMZ830"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TI.W",["FLZ052"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TR.W^HU.A",["FLZ048"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.A",["FLZ050"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TI.W^HI.A",["FLZ056"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HI.A",["FLZ061"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TR.A^FF.A",["GMZ870"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TI.A^CF.W",["FLZ057"]),

          ("Fcst", "Wind", "VECTOR", -6, 0, (10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 0, 6, (10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 6, 12, (20, "W"), "all"),
          # Trop begin
          ("Fcst", "Wind", "VECTOR", 12, 18, (35, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 18, 24, (40, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 24, 30, (50, "W"), "all"),
          # Hurricane begin
          ("Fcst", "Wind", "VECTOR", 30, 36, (70, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 36, 42, (75, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 42, 48, (70, "W"), "all"),
          # Hurricane end
          ("Fcst", "Wind", "VECTOR", 48, 54, (50, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 54, 60, (35, "W"), "all"),
          # Trop end
          ("Fcst", "Wind", "VECTOR", 60, 66, (10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 66, 120, (10, "W"), "all"),
          
          ("Fcst", "WindGust", "SCALAR", -6, 120, 90, "all"),
          ],
     "cmdLineVars" :str(cmdLineVars()),     
     "fileChanges": [("Hazard_HLS_<site>_Overrides", "TextUtility", "add", useTest, "undo")],
     "checkStrings" : [
        #"HURRICANE LOCAL STATEMENT",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       ],
     },

     {
     "name":"HLS_generalTests_2", 
     "productType" : "Hazard_HLS", 
     "commentary": """Looks at Overview Watches/Warnings and Wind Timing reporting""",
     "gridsStartTime": None,     
     "createGrids": createDataGrids() + [
          ("Fcst", "Hazards", "DISCRETE", 0,24,"<None>", "all"),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.W",["GMZ873"]),# New
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.W",["FLZ039"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HI.W",["FLZ057"]), # New -- TI.A UPG
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HI.W",["FLZ043"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TR.W",["GMZ830"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TI.W",["FLZ052"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TR.W^HU.A",["FLZ048"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HU.A",["FLZ050"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TI.W^HI.A",["FLZ056"]),            
          ("Fcst", "Hazards", "DISCRETE", 0,24,"HI.A",["FLZ061"]),
          ("Fcst", "Hazards", "DISCRETE", 0,24,"TR.A",["GMZ870"]),

          ("Fcst", "Wind", "VECTOR", -6, 0, (10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 0, 6, (10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 6, 12, (20, "W"), "all"),
          # Trop begin
          ("Fcst", "Wind", "VECTOR", 12, 18, (35, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 18, 24, (40, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 24, 30, (50, "W"), "all"),
          # Hurricane begin
          ("Fcst", "Wind", "VECTOR", 30, 36, (70, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 36, 42, (75, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 42, 48, (70, "W"), "all"),
          # Hurricane end
          ("Fcst", "Wind", "VECTOR", 48, 54, (50, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 54, 60, (35, "W"), "all"),
          # Trop end
          ("Fcst", "Wind", "VECTOR", 60, 66, (10, "W"), "all"),
          ("Fcst", "Wind", "VECTOR", 66, 120, (10, "W"), "all"),
          ],
     "cmdLineVars" :str(cmdLineVars()),     
     "fileChanges": [("Hazard_HLS_<site>_Overrides", "TextUtility", "add", useTest, "undo")],
     "checkStrings" : [
        #"HURRICANE LOCAL STATEMENT",
        "NATIONAL WEATHER SERVICE TAMPA BAY RUSKIN FL",
       ],
     },  
    ]

# Now add the generated scripts
firstScripts = makeScripts()
secondScripts = makeScripts(
    len(firstScripts),
    createDataGrids(maxWind=75, maxGust=90, prob34=50, prob64=20))

scripts = scripts + firstScripts + secondScripts




import TestScript
def testScript(self, dataMgr):
    defaults = {
        "cmdLineVars" :"{('Source', 'source'): 'COLORADO EMERGENCY MANAGEMENT AGENCY DENVER COLORADO', ('Issued By', 'issuedBy'): None, ('EAS Level', 'eas'): 'NONE'}",
        "publishGrids" : 1,
        "vtecMode" : "O",
        "clearHazardsTable": 1,
        "gridsStartTime": "202001201_0000",
        "orderStrings": 1,
        "deleteGrids" : [
            ("Fcst", "Hazards", "SFC", -48, 240),
            ("Fcst", "pws34int", "SFC", -48, 240),
            ("Fcst", "pws64int", "SFC", -48, 240),
            ("Fcst", "prob34", "SFC", -48, 240),
            ("Fcst", "prob64", "SFC", -48, 240),
            ],
        }
    return TestScript.generalTestScript(self, dataMgr, scripts, defaults)

