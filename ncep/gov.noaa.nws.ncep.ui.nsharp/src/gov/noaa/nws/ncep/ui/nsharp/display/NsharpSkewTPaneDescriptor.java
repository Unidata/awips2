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
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.FrameCoordinator;
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
    
    private void setFrameCoordinator() {
    	frameCoordinator = new FrameCoordinator(this) {
			@Override
			/*
			 * Chin Note: this function handles keyboard up/down/left/right arrow keys for station and time line stepping.
			 */
            public void changeFrame(
                    IFrameCoordinator.FrameChangeOperation operation,
                    IFrameCoordinator.FrameChangeMode mode) {
				//NsharpEditor editor = NsharpEditor.getActiveNsharpEditor() ;
				//if(editor== null || editor.getRscHandler()==null)
				//	return;
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
				int frameSize= rscHandler.getTimeLineStateListSize();
				int curFrameIndex = rscHandler.getCurrentTimeLineStateListIndex();
				if(curFrameIndex == 0)
					waitTime = loopProperties.getFirstFrameDwell();
				else if(curFrameIndex == frameSize-1)
					waitTime = loopProperties.getLastFrameDwell();

				loopProperties.drawAfterWait(waitTime);

				if (loopProperties.isShouldDraw()) {
					rscHandler.setLoopingDataTimeLine(loopProperties);

				}
			}
			
		};
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
        	//System.out.println("NsharpAbstractPaneDescriptor changeFrame called pane= "+paneNumber);
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
