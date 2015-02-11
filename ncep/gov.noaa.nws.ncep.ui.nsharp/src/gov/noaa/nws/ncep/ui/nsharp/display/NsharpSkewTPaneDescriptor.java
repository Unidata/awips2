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
 * 01/13/2015   DR#17008,
 *              task#5930   Chin Chen   NSHARP Hodograph Does Not Loop in D2D Lite Configuration
 *                                      moved "setFrameCoordinator()" to NsharpAbstractPaneDescriptor,
 *                                      so it can be used by other descriptor
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpSkewTPaneResource;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "nsharpSkewTPaneDescriptor")
public class NsharpSkewTPaneDescriptor extends NsharpAbstractPaneDescriptor {
    public NsharpSkewTPaneDescriptor(PixelExtent pe) {
        super(pe);
        setTimeMatcher(new D2DTimeMatcher());
        setFrameCoordinator();
        //System.out.println("NsharpSkewTPaneDescriptor  created " + this.toString());  
    }
    public NsharpSkewTPaneDescriptor(PixelExtent pe, int paneNumber) {
        super(pe, paneNumber);
        setTimeMatcher(new D2DTimeMatcher());
        setFrameCoordinator();
    }
    
    public NsharpSkewTPaneDescriptor() {
		super();
		setTimeMatcher(new D2DTimeMatcher());
		setFrameCoordinator();
	}
    public NsharpSkewTPaneResource getSkewtResource() {
        List<NsharpSkewTPaneResource> list = resourceList
                .getResourcesByTypeAsType(NsharpSkewTPaneResource.class);
        if (list != null && !list.isEmpty()) {
            return list.get(0);
        }
        return null;
    }
	
    @SuppressWarnings("deprecation")
	@Override
	/*
	 * Chin Note: this function handles time line stepping from NC Perspective tool bar left/right/first/last arrow Buttons.
	 */
    public void changeFrame(FrameChangeOperation operation,  FrameChangeMode mode) {
        synchronized (this) {
        	//Chin Note: there are multiple (6) panes.
    		//However, we only need to step once. Therefore, only handle stepping by skewt pane.
        	//From stepping commands
        	//System.out.println("NsharpAbstractPaneDescriptor changeFrame called ");
        	if( VizPerspectiveListener.getCurrentPerspectiveManager()!= null){
        		//System.out.println("changeFrame: current perspective ="+VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId());
    			if(!VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId().equals(NmapCommon.NatlCntrsPerspectiveID)){
    				if(mode != FrameChangeMode.TIME_ONLY)
    					return;
    			}
        	}
        	//System.out.println("changeFrame");
        	if(rscHandler == null)
        		return;
        	// we will have to do conversion here
        	IFrameCoordinator.FrameChangeOperation dop = IFrameCoordinator.FrameChangeOperation.valueOf(operation.name());
        	IFrameCoordinator.FrameChangeMode dmode = IFrameCoordinator.FrameChangeMode.valueOf(mode.name());
        	rscHandler.setSteppingTimeLine(dop, dmode);

        }
    }
}
