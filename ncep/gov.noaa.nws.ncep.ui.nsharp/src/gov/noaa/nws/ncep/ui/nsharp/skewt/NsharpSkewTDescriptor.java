/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor
 * 
 * This java class performs the NSHARP NsharpSkewTDescriptor functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 03/24/2011   R1G2-9      Chin Chen   migration
 * 06/14/2011   11-5        Chin Chen   migration
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.skewt;

import java.util.List;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpBackgroundResource;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.viz.core.graphing.GraphDescriptor;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

public class NsharpSkewTDescriptor extends GraphDescriptor {
    public NsharpSkewTDescriptor() {
        this(new PixelExtent(NsharpConstants.NSHARP_SkewTRectangle));
    }
    
    public NsharpSkewTDescriptor(PixelExtent pe) {
        super(pe);
        //System.out.println("NsharpSkewTDescriptor  created " + this.toString());
        
    }
    
    @Override
    public void changeFrame(FrameChangeOperation operation, FrameChangeMode mode) {
        synchronized (this) {
        	//From stepping commands
        	NsharpSkewTResource skewRsc = getSkewtResource();
        	if( VizPerspectiveListener.getCurrentPerspectiveManager()!= null){
        		//System.out.println("current perspective ="+VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId());
    			if(!VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId().equals(NmapCommon.NatlCntrsPerspectiveID)){
    				if(mode != FrameChangeMode.TIME_AND_SPACE)
    					//Chin NOTE: for D2D perspective, it will call this function with mode of TIME_ONLY or SPACE_ONLY when Arrow Keys (up/down/left/right)
    					//hit by users. Since Nsharp implementation uses Arrow Keys for time line and station box operations,
    					//We have to disable this function when in D2D for Arrow keys. Otherwise, the following setSteppingTimeLine() function will
    					// be called 2 times when Arrow Keys hit in D2D.
    					//Therefore, only when TIME_AND_SPACE mode is allowed when in D2D.
    					return;
    			}
        	}
        	skewRsc.setSteppingTimeLine( operation,  mode);
        }
    }
    @SuppressWarnings("deprecation")
	@Override
    public void checkDrawTime(LoopProperties loopProperties) {
        super.checkDrawTime(loopProperties);
    //handleDataTimeIndex is no longer available since 11.5
    //called from DrawCoordinatedPane 
    //Chin: 11.11 note: it is now called from NsharpSkewTDisplay.paint() when animation is started.
    	//System.out.println("NsharpSkewTDescriptor checkDrawTime called ");
    	if (loopProperties == null || getFrames() == null) {
    		//System.out.println("NsharpSkewTDescriptor checkDrawTime called but jump ");
    		return;
    	}

    	if (loopProperties.isLooping() && loopProperties.isShouldDraw()) {
    		NsharpSkewTResource skewRsc = getSkewtResource();
    		
    		skewRsc.setLoopingDataTimeLine(loopProperties);
    		//System.out.println("NsharpSkewTDescriptor handleDataTimeIndex handled!!!!!! ");
    	}
    }
    
    public NsharpBackgroundResource getSkewTBkGResource() {
        List<NsharpBackgroundResource> list = resourceList
                .getResourcesByTypeAsType(NsharpBackgroundResource.class);
        if (list != null && !list.isEmpty()) {
            return list.get(0);
        }
        return null;
    }
    
    public NsharpSkewTResource getSkewtResource() {
        List<NsharpSkewTResource> list = resourceList
                .getResourcesByTypeAsType(NsharpSkewTResource.class);
        if (list != null && !list.isEmpty()) {
            return list.get(0);
        }
        return null;
    }

}
