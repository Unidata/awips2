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
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.geotools.referencing.crs.DefaultEngineeringCRS;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.GraphDescriptor;
@XmlAccessorType(XmlAccessType.NONE)
public class NsharpAbstractPaneDescriptor extends GraphDescriptor {
	@XmlElement
	protected int paneNumber;
    
	protected NsharpResourceHandler rscHandler=null;
    
	
    public NsharpResourceHandler getRscHandler() {
		return rscHandler;
	}

	public void setRscHandler(NsharpResourceHandler rscHandler) {
		this.rscHandler = rscHandler;
	}

    public int getPaneNumber() {
		return paneNumber;
	}

	public void setPaneNumber(int paneNumber) {
		this.paneNumber = paneNumber;
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
        this.paneNumber = paneNumber;
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
}
