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

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.viz.core.graphing.GraphDescriptor;

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
        	//System.out.println("NsharpSkewTDescriptor changeFrame mode" + mode.toString()+ " operation "+ operation );
        	NsharpSkewTResource skewRsc = getSkewtResource();
        	
        	skewRsc.setSteppingDataTimeLine( operation,  mode);
        }
    }
    @SuppressWarnings("deprecation")
	@Override
    public void checkDrawTime(LoopProperties loopProperties) {
        super.checkDrawTime(loopProperties);
    //handleDataTimeIndex is no longer available since 11.5
    //called from DrawCoordinatedPane 
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
