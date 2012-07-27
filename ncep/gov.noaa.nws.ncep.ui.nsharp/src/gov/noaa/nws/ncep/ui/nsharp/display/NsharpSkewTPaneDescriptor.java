/**
 * 
 * ggov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDescriptor
 * 
 * This java class performs the NSHARP NsharpSkewTPaneDescriptor functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/02/2012	229			Chin Chen	Initial coding for multiple display panes implementation
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpSkewTPaneResource;

import java.util.List;

import com.raytheon.uf.viz.core.PixelExtent;

public class NsharpSkewTPaneDescriptor extends NsharpAbstractPaneDescriptor {
    public NsharpSkewTPaneDescriptor(PixelExtent pe) {
        super(pe);
        //System.out.println("NsharpSkewTPaneDescriptor  created " + this.toString());  
    }
    public NsharpSkewTPaneDescriptor(PixelExtent pe, int paneNumber) {
        super(pe, paneNumber);
    }
    
    public NsharpSkewTPaneResource getSkewtResource() {
        List<NsharpSkewTPaneResource> list = resourceList
                .getResourcesByTypeAsType(NsharpSkewTPaneResource.class);
        if (list != null && !list.isEmpty()) {
            return list.get(0);
        }
        return null;
    }
    @Override
    public void changeFrame(FrameChangeOperation operation,  FrameChangeMode mode) {
        synchronized (this) {
        	//Chin Note: there are multiple (4) panes.
    		//However, we only need to step once. Therefore, only handle stepping by skewt pane.
        	//From stepping commands
        	//System.out.println("NsharpAbstractPaneDescriptor changeFrame called pane= "+paneNumber);
        	/*if( VizPerspectiveListener.getCurrentPerspectiveManager()!= null){
        		System.out.println("changeFrame: current perspective ="+VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId());
    			if(!VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId().equals(NmapCommon.NatlCntrsPerspectiveID)){
    				if(mode != FrameChangeMode.TIME_AND_SPACE)
    					//Chin NOTE: for D2D perspective, it will call this function with mode of TIME_ONLY or SPACE_ONLY when Arrow Keys (up/down/left/right)
    					//hit by users. Since Nsharp implementation uses Arrow Keys for time line and station box operations,
    					//We have to disable this function when in D2D for Arrow keys. Otherwise, the following setSteppingTimeLine() function will
    					// be called 2 times when Arrow Keys hit in D2D.
    					//Therefore, only when TIME_AND_SPACE mode is allowed when in D2D.
    					return;
    			}
        	}*/
        	NsharpEditor editor = NsharpEditor.getActiveNsharpEditor() ;
        	if(editor!= null && editor.getRscHandler()!=null){
        		editor.getRscHandler().setSteppingTimeLine(operation, mode);
        	}
        }
    }
}
