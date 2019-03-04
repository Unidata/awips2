##
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
    
                    