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
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;

import gov.noaa.nws.ncep.viz.common.EditorManager;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.vividsolutions.jts.geom.GeometryFactory;

@XmlAccessorType(XmlAccessType.NONE)
public class NsharpAbstractPaneDisplay extends AbstractRenderableDisplay {
    public static GeometryFactory gf = new GeometryFactory();
    protected  int paneNum =0;
    protected  int editorNum =0;
    protected String paneName;
    public int getEditorNum() {
		return editorNum;
	}

	public void setEditorNum(int editorNum) {
		this.editorNum = editorNum;
		//System.out.println("NsharpAbstractPaneDisplay setEditorNnum " + editorNum );
	   	 
	}

    public NsharpAbstractPaneDisplay() {
		super();
		// TODO Auto-generated constructor stub
	}
    public NsharpAbstractPaneDisplay(PixelExtent pixelExtent,int paneNumber) {
        this (pixelExtent,paneNumber, "AbstractPane", new NsharpAbstractPaneDescriptor(pixelExtent, paneNumber));
    }
	public NsharpAbstractPaneDisplay(PixelExtent pixelExtent,int paneNumber, String name, NsharpAbstractPaneDescriptor desc) {
        super(pixelExtent, desc);
        paneNum = paneNumber;
        paneName = name;
    }
    
    @Override
    public void dispose() {
    	
    	 int editorInstanceNum = EditorManager.getNumOfEditorInstance(editorNum);
    	 //System.out.println("NsharpAbstractPaneDisplay disposed  editor num " + editorNum +" instance "+ editorInstanceNum);
    	 if (this.descriptor != null && editorInstanceNum <= 1) {
            super.dispose();
        }
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

	public int getPaneNum() {
		return paneNum;
	}

	public String getPaneName() {
		return paneName;
	}

}
