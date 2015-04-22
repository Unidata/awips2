/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDisplay
 * 
 * This java class performs the NSHARP NsharpAbstractPaneDisplay functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/30/2012	229			Chin Chen	Initial coding for multiple display panes implementation
 * 03/11/2013   972         Greg Hull   rm paneNum and editorNum
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpAbstractPaneResourceData;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.vividsolutions.jts.geom.GeometryFactory;

@XmlAccessorType(XmlAccessType.NONE)
public class NsharpAbstractPaneDisplay extends AbstractRenderableDisplay {
    public static GeometryFactory gf = new GeometryFactory();
 // TODO : I'm reasonably sure these are not needed anymore. Remove with EditorManager code
//  protected  int paneNum =0;
//  protected  int editorNum =0;
//  protected String paneName;
  
//  public int getEditorNum() {
//		return editorNum;
//	}
//	public void setEditorNum(int editorNum) {
//		this.editorNum = editorNum;
//		//System.out.println("NsharpAbstractPaneDisplay setEditorNnum " + editorNum );
//	   	 
//	}

    public NsharpAbstractPaneDisplay() {
		super();
		// TODO Auto-generated constructor stub
	}
    public NsharpAbstractPaneDisplay(PixelExtent pixelExtent,int paneNumber) {
        this (pixelExtent,paneNumber, "AbstractPane", new NsharpAbstractPaneDescriptor(pixelExtent, paneNumber));
    }
	public NsharpAbstractPaneDisplay(PixelExtent pixelExtent,int paneNumber, String name, NsharpAbstractPaneDescriptor desc) {
        super(pixelExtent, desc);
//        paneNum = paneNumber;
//        paneName = name;
    }
    
    @Override
    public void dispose() {
    	// if it turns out that we still need reference counts for the editor, move the
    	// EditorManager methods to NsharpEditor or replace with a simple reference count 
    	// 
//    	if( getContainer() instanceof NsharpEditor ) {
//    		int editorInstanceNum = ((NsharpEditor)getContainer()).getNumOfEditorInstance(editorNum);
//
////    		int editorInstanceNum = EditorManager.getNumOfEditorInstance(editorNum);
//    		//System.out.println("NsharpAbstractPaneDisplay disposed  "+ this.toString());
//    		if (this.descriptor != null && editorInstanceNum <= 1) {
    			super.dispose();
//    		}
//        }
    }
    @Override
    public NsharpAbstractPaneDescriptor getDescriptor() {
        return (NsharpAbstractPaneDescriptor) super.getDescriptor();
    }
    
    @SuppressWarnings("deprecation")
	@Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);

        //this.target = target;
        //this.paintProps = paintProps;

        //Chin:: 11.5 changes, DrawCoordinatedPane() will call checkDrawTime(),
        // so we dont have to call it from here.
        //Chin: 11.11 change: however, DrawCoordinatedPane() does not  call checkDrawTime() any more
        // since 11.11. So, to make looping work, we call it from here AGAIN. Any other good work from Raytheon.
        descriptor.checkDrawTime(paintProps.getLoopProperties());
        
        drawTheData(target, paintProps);
    }

    /**
     * Draws the data on the screen.
     * 
     * @throws VizException
     */
    protected void drawTheData(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        ArrayList<ResourcePair> resourceList = new ArrayList<ResourcePair>(
                descriptor.getResourceList());
        
        PaintProperties myProps = new PaintProperties(paintProps);
        for (ResourcePair pair : resourceList) {
            if (pair.getProperties().isVisible()) {
                AbstractVizResource<?, ?> rsc = pair.getResource();
                if ((rsc != null) && (rsc.getStatus()!= ResourceStatus.DISPOSED)) {
                    myProps = calcPaintDataTime(myProps, rsc);
                    rsc.paint(target, myProps);
                }
            }
        }
    }
    public GeometryFactory getGeometry() {
        return gf;
    }

	@Override
    protected void customizeResourceList(ResourceList resourceList) {
    	AbstractResourceData resourceData = new NsharpAbstractPaneResourceData();
    	// get a load properties
    	LoadProperties loadProperties = new LoadProperties();
    	ColorableCapability colorableCapability = new ColorableCapability();
    	colorableCapability.setColor(NsharpConstants.backgroundColor);
    	loadProperties.getCapabilities().addCapability(colorableCapability);
    	// get some resource properties
    	ResourceProperties resourceProperties = new ResourceProperties();
    	resourceProperties.setVisible(true);
    	resourceProperties.setMapLayer(true);
    	resourceProperties.setSystemResource(true);
    	// Make a resource pair
    	ResourcePair resourcePair = new ResourcePair();
    	resourcePair.setResourceData(resourceData);
    	resourcePair.setLoadProperties(loadProperties);
    	resourcePair.setProperties(resourceProperties);
    	// add it to the resource list.
    	resourceList.add(resourcePair);           
    }

}
