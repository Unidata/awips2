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

class QCReport:
    VALIDITY_BIT = 2
    INTERNAL_BIT = 8
    TEMPORAL_BIT = 16
    STATISTICAL_BIT = 32
    SPATIAL_BIT = 64
    KALMAN_BIT = 1024
    
    FILL='.'
    
    ROW_HEADERS = ['validity','internal','temporal','statistical','spatial','subjective']
    CHECK_MASKS = [VALIDITY_BIT, INTERNAL_BIT, TEMPORAL_BIT, STATISTICAL_BIT, SPATIAL_BIT, 'X']
    
    def __init__(self, baseParamNames, headers, checks, tlCornerParameter = None):
        self.baseParamNames = baseParamNames
        self.paramNames = None
        self.headers = headers
        self.checks = checks
        self.tlCornerParameter = tlCornerParameter
        self.pdv = None
        self.tableText = None
        self.subjectiveMap = { 'B': 'B', 'G': 'G' }
        self.title = 'checks'

    def setTitle(self, title):
        self.title = title
        
    def setPointDataView(self, pdv):
        self.pdv = pdv 
        self.tableText = None
        self.failed = None
        
    def getPointDataView(self):
        return self.pdv
        
    def getTableText(self):
        if self.tableText is None:
            fill = QCReport.FILL
            # TODO: also tlCornerParameter
            hdr_col_width = max([len(h) for h in QCReport.ROW_HEADERS])
            prs = []
            failed = False
            for pName in self.baseParamNames:
                qA = self.pdv.getInt(pName + "QCA")
                qR = self.pdv.getInt(pName + "QCR")
                qD = self.pdv.getString(pName + "DD")
                
                pr = [ ]
                for chk in QCReport.CHECK_MASKS:
                    if chk != 'X':
                        if qA & chk:
                            ch = (qR & chk) and 'F' or 'P'
                        else:
                            ch = fill
                        failed = failed or ch == 'F'
                    else:
                        ch = self.subjectiveMap.get(qD, fill)
                        failed = failed or ch == 'B'
                    pr.append(ch)

                prs.append(pr)
                
            txt = ''
            hdr = fill * 2 + self.title
            txt += hdr + fill * (hdr_col_width - len(hdr))
            for hdr in self.headers:
                txt += '!' + hdr + fill * (3 - len(hdr))

            addTrailingExclamation = len(hdr) < 3
            if addTrailingExclamation:
                # add trailing exclamation
                txt += '!'
                
            ci = 0
            for chk in QCReport.CHECK_MASKS:
                hdr = QCReport.ROW_HEADERS[ci]
                txt += '\n' + hdr + fill * (hdr_col_width - len(hdr))
                for pi in range(0, len(self.baseParamNames)):
                    txt += '!' + fill + prs[pi][ci] + fill
                if addTrailingExclamation:
                    # add trailing exclamation
                    txt += '!'
                ci += 1 
                
            self.tableText = txt
            self.failed = failed
            
        return self.tableText
    
    def isOk(self):
        self.getTableText()
        return self.failed == False
    
    def isFailed(self):
        self.getTableText()
        return self.failed == True

