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
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpAbstractPaneResource;

import java.util.List;

import org.geotools.referencing.crs.DefaultEngineeringCRS;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.GraphDescriptor;

public class NsharpAbstractPaneDescriptor extends GraphDescriptor {
	protected int paneNumber;
    
    public int getPaneNumber() {
		return paneNumber;
	}

	public void setPaneNumber(int paneNumber) {
		this.paneNumber = paneNumber;
	}
    
    public NsharpAbstractPaneDescriptor(PixelExtent pe) {
        super(pe);
        //System.out.println("NsharpAbstractPaneDescriptor  created " + this.toString());  
    }
    public NsharpAbstractPaneDescriptor(PixelExtent pe, int paneNumber) {
        super(pe);
        this.paneNumber = paneNumber;
        //System.out.println("NsharpAbstractPaneDescriptor  created " + this.toString());       
    }
    @SuppressWarnings("deprecation") 
    @Override
    public void changeFrame(FrameChangeOperation operation,  FrameChangeMode mode) {
        synchronized (this) {
        	//From stepping commands
        	//System.out.println("NsharpAbstractPaneDescriptor changeFrame called pane= "+paneNumber);
        }
    }
    @SuppressWarnings("deprecation")
	@Override
    public void checkDrawTime(LoopProperties loopProperties) {
        super.checkDrawTime(loopProperties);
        //System.out.println("NsharpAbstractPaneDescriptor checkDrawTime called pane= "+paneNumber);
    //handleDataTimeIndex is no longer available since 11.5
    //called from DrawCoordinatedPane 
    //Chin: 11.11 note: it is now called from NsharpSkewTDisplay.paint() when animation is started.
    	//System.out.println("NsharpAbstractPaneDescriptor checkDrawTime called ");
    	if (loopProperties == null || getFrames() == null) {
    		//System.out.println("NsharpAbstractPaneDescriptor checkDrawTime called but jump ");
    		return;
    	}
    	if (loopProperties.isLooping() && loopProperties.isShouldDraw()) {
    		//System.out.println("NsharpAbstractPaneDescriptor checkDrawTime  with looping called pane= "+paneNumber);
    		NsharpEditor editor = NsharpEditor.getActiveNsharpEditor() ;
        	if(editor!= null){
        		
        		editor.getRscHandler().setLoopingDataTimeLine(loopProperties);
        	}
    	}
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
			setGridGeometry(createGridGeometry(anExtent, DefaultEngineeringCRS.CARTESIAN_2D));
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }
}
