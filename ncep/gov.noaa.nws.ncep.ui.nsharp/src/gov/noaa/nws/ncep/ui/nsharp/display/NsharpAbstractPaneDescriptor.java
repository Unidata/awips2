/**
 * 
 * ggov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor
 * 
 * This java class performs the NSHARP NsharpAbstractPaneDescriptor functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/02/2012	229			Chin Chen	Initial coding for multiple display panes implementation
 * 03/11/2013   972         Greg Hull   rm paneNumber
 * 03/11/2013   2491        bsteffen    extend IDescriptor derictly for better serialization detection.
 * 01/13/2015   DR#17008,
 *              task#5930   Chin Chen   NSHARP Hodograph Does Not Loop in D2D Lite Configuration
 *                                      moved "setFrameCoordinator()"  from NsharpSkewTPaneDescriptor to here,
 *                                      so it can be used by other descriptor
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpAbstractPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.viz.ui.display.NCLoopProperties;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import org.geotools.referencing.crs.DefaultEngineeringCRS;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.FrameCoordinator;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.GraphDescriptor;
@XmlAccessorType(XmlAccessType.NONE)
public class NsharpAbstractPaneDescriptor extends GraphDescriptor implements
        IDescriptor {
	protected NsharpResourceHandler rscHandler=null;
    
	
    public NsharpResourceHandler getRscHandler() {
		return rscHandler;
	}

	public void setRscHandler(NsharpResourceHandler rscHandler) {
		this.rscHandler = rscHandler;
	}
    
    public NsharpAbstractPaneDescriptor() {
		super();
		// TODO Auto-generated constructor stub
	}

	public NsharpAbstractPaneDescriptor(PixelExtent pe) {
        super(pe);
        //System.out.println("NsharpAbstractPaneDescriptor  created " + this.toString());  
    }
    public NsharpAbstractPaneDescriptor(PixelExtent pe, int paneNumber) {
        super(pe);
//        this.paneNumber = paneNumber;
        //System.out.println("NsharpAbstractPaneDescriptor  created " + this.toString());       
    }
    @SuppressWarnings("deprecation") 
    @Override
    public void changeFrame(FrameChangeOperation operation,  FrameChangeMode mode) {
        //Chin: 12.8.1: Each pane has its own pane descriptor. For multiple pane architecture, we only 
    	// let skewtPaneDescriptor to handle this function. Otherwise, stepping will cause X times frame change
    	// as each pane will change frame once.
    }
    //@Override
    public void checkDrawTime(LoopProperties loopProperties) {
    	//Chin: 12.8.1: Same reason as changeFrame(). 
    }
    
    public NsharpAbstractPaneResource getPaneResource() {
        List<NsharpAbstractPaneResource> list = resourceList
                .getResourcesByTypeAsType(NsharpAbstractPaneResource.class);
        if (list != null && !list.isEmpty()) {
            return list.get(0);
        }
        return null;
    }
    public void setNewPe(PixelExtent anExtent){
        try {
        	//System.out.println("PaneDescriptor " + this.toString() + " PE="+anExtent.getMinX()+","+anExtent.getMinY()+
        	//		","+anExtent.getMaxX()+","+anExtent.getMaxY());  
			setGridGeometry(createGridGeometry(anExtent, DefaultEngineeringCRS.CARTESIAN_2D));
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }
    //DR17008
    protected void setFrameCoordinator() { 
    	frameCoordinator = new FrameCoordinator(this) {
			@Override
			/*
			 * Chin Note: this function handles keyboard up/down/left/right arrow keys for station and time line stepping.
			 */
            public void changeFrame(
                    IFrameCoordinator.FrameChangeOperation operation,
                    IFrameCoordinator.FrameChangeMode mode) {
				if(rscHandler == null)
					return;
				//System.out.println("NsharpSkewTPaneDescriptor changeFrame(operation) called  op="+operation+" mode"+mode);
				if(mode == IFrameCoordinator.FrameChangeMode.SPACE_ONLY){
					//up/down arrow keys for stepping stations
					//editor.getRscHandler().setSteppingStnIdList(operation);
					rscHandler.setSteppingStnIdList(operation);
				} else if(mode == IFrameCoordinator.FrameChangeMode.TIME_ONLY || mode == IFrameCoordinator.FrameChangeMode.TIME_AND_SPACE){
					//left/right arrow keys for stepping time lines
					//editor.getRscHandler().setSteppingTimeLine(operation, mode);
					rscHandler.setSteppingTimeLine(operation, mode);
				}
            }
			/*
			 * (non-Javadoc)
			 * @see com.raytheon.uf.viz.core.drawables.FrameCoordinator#changeFrame(com.raytheon.uf.viz.core.datastructure.LoopProperties)
			 * This function handling nsharp looping. 
			 * Chin: 12.8.1: let skewtPaneDescriptor handle looping. All other pane descriptor will do nothing. Otherwise, we will looping X times faster when we
	    	 *  have X number of panes configured and each pane move frame once.
			 * 
			 */
			@Override
			public void changeFrame(LoopProperties loopProperties) {
				if(rscHandler == null)
					return;
				
				long waitTime = Long.MAX_VALUE;
				//System.out.println("NsharpSkewTPaneDescriptor changeFrame(loop) called, loopDirection= "+loopDirection + " fwd="+loopProperties.getFwdFrameTime()+
				//		" back="+loopProperties.getRevFrameTime() + " 1st dt="+loopProperties.getFirstFrameDwell()+ " lasDt="+loopProperties.getLastFrameDwell());
				if(loopProperties.getMode() == LoopProperties.LoopMode.Forward || loopProperties.getMode() == LoopProperties.LoopMode.Cycle)
					waitTime = loopProperties.getFwdFrameTime();
				else
					waitTime = loopProperties.getRevFrameTime();
				int frameSize= rscHandler.getTimeElementListSize();
				int curFrameIndex = rscHandler.getCurrentTimeElementListIndex();
				if(curFrameIndex == 0)
					waitTime = loopProperties.getFirstFrameDwell();
				else if(curFrameIndex == frameSize-1)
					waitTime = loopProperties.getLastFrameDwell();

				loopProperties.drawAfterWait(waitTime);
				//System.out.println("wait time="+waitTime);
				if (loopProperties.isShouldDraw()) {
					rscHandler.setLoopingDataTimeLine(loopProperties);
					//System.out.println("loopinp step");
				}
			}
			
		};
    }
}
