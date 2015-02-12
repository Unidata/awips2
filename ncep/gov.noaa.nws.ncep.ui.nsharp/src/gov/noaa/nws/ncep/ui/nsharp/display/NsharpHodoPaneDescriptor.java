/**
 * 
 * ggov.noaa.nws.ncep.ui.nsharp.display.NsharpHodoPaneDescriptor
 * 
 * This java class performs the NSHARP NsharpHodoPaneDescriptor functions.
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
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpHodoPaneResource;
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
@XmlType(name = "nsharpHodoPaneDescriptor")
public class NsharpHodoPaneDescriptor extends NsharpAbstractPaneDescriptor {
   
    public NsharpHodoPaneDescriptor(PixelExtent pe) {
        super(pe);
        if((NsharpEditor.getActiveNsharpEditor() != null) && (NsharpEditor.getActiveNsharpEditor().getPaneConfigurationName().equals(NsharpConstants.PANE_LITE_D2D_CFG_STR)))
        {
        	setTimeMatcher(new D2DTimeMatcher());
            setFrameCoordinator();
        }
    }
    public NsharpHodoPaneDescriptor(PixelExtent pe, int paneNumber) {
        super(pe, paneNumber);
        if((NsharpEditor.getActiveNsharpEditor() != null) && (NsharpEditor.getActiveNsharpEditor().getPaneConfigurationName().equals(NsharpConstants.PANE_LITE_D2D_CFG_STR)))
        {
        	setTimeMatcher(new D2DTimeMatcher());
            setFrameCoordinator();
        }
    }
    public NsharpHodoPaneDescriptor() {
        super();
        if((NsharpEditor.getActiveNsharpEditor() != null) && (NsharpEditor.getActiveNsharpEditor().getPaneConfigurationName().equals(NsharpConstants.PANE_LITE_D2D_CFG_STR)))
        {
        	setTimeMatcher(new D2DTimeMatcher());
            setFrameCoordinator();
        }
    }
    public NsharpHodoPaneResource getHodoResource() {
        List<NsharpHodoPaneResource> list = resourceList
                .getResourcesByTypeAsType(NsharpHodoPaneResource.class);
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
    		//We only need to step one timeline. Therefore, we handle stepping by skewt pane.
        	//However, for D2D LITE display configuration, when switched to HODO pane, skewT pane is no longer
        	//in charge. therefore, we have to handle this special case here by HODO pane.
        	
        	if((rscHandler == null) ||(!rscHandler.getPaneConfigurationName().equals(NsharpConstants.PANE_LITE_D2D_CFG_STR)))
        		return;
        	
        	if( VizPerspectiveListener.getCurrentPerspectiveManager()!= null){
        		//System.out.println("changeFrame: current perspective ="+VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId());
    			if(!VizPerspectiveListener.getCurrentPerspectiveManager().getPerspectiveId().equals(NmapCommon.NatlCntrsPerspectiveID)){
    				if(mode != FrameChangeMode.TIME_ONLY)
    					return;
    			}
        	}
        	
        	// we will have to do conversion here
        	IFrameCoordinator.FrameChangeOperation dop = IFrameCoordinator.FrameChangeOperation.valueOf(operation.name());
        	IFrameCoordinator.FrameChangeMode dmode = IFrameCoordinator.FrameChangeMode.valueOf(mode.name());
        	rscHandler.setSteppingTimeLine(dop, dmode);

        }
    }

}
