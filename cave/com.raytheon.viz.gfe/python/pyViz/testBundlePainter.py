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
# Test of BundlePainter
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    02/23/10                      njensen        Initial Creation.
#    
# 
#

import BundlePainter

def main():
    print "Starting"
    from com.raytheon.uf.viz.core.datastructure import DataCubeManagerStarter
    dcms = DataCubeManagerStarter()
    dcms.earlyStartup()
    bundle = sys.argv[1]    
    bp = BundlePainter.BundlePainter(bundle, 800.0, 600.0)
    times = bp.getDescriptor().getDataTimes()
    for t in times:
        bp.paint(t)
        img = bp.getTarget().screenshot()
        outname = '/tmp/images/' + str(t) + '.png' #TODO fix
        print "Outputting ", outname
        bp.outputImage(img, outname)        
    print "Finished"
    


if __name__ == "__main__":
    main()
    
                    