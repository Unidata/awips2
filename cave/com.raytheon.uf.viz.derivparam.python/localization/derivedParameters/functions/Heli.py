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

def execute(uStk, vStk, RM5):

    umot = RM5[2]
    vmot = RM5[3]
    u1 = uStk[0]
    v1 = vStk[0]
    u2 = uStk[-1]
    v2 = vStk[-1]
    # First do our motion, lower bulk shear computation.
    hptr = (v2-v1)*umot+(u1-u2)*vmot
    for i in range(1, len(uStk)):
        u1 = uStk[i-1]
        v1 = vStk[i-1]
        u2 = uStk[i]
        v2 = vStk[i]
        hptr += u2*v1-u1*v2
    return hptr
