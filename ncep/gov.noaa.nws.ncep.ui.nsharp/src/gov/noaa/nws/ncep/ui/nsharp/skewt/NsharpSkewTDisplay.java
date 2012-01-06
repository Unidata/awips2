/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDisplay
 * 
 * This java class performs the NSHARP NsharpSkewTDisplay functions.
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

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpBkgResourceData;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResourceData;
import gov.noaa.nws.ncep.viz.common.EditorManager;

import java.util.ArrayList;
import com.raytheon.uf.viz.core.IGraphicsTarget; 
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.vividsolutions.jts.geom.GeometryFactory;

public class NsharpSkewTDisplay extends AbstractRenderableDisplay {
    public static GeometryFactory gf = new GeometryFactory();
    
    //private PaintProperties paintProps = null;

    //private IGraphicsTarget target = null;

    //private NsharpBackgroundResource bkgRsc;
    //private NsharpSkewTResource skewRsc;
    private  int editorNum =0;
    
    public int getEditorNum() {
		return editorNum;
	}

	public void setEditorNum(int editorNum) {
		this.editorNum = editorNum;
		//System.out.println("NsharpSkewTDisplay setEditorNnum " + editorNum );
	   	 
	}

	/**
     * Constructor
     * Chin: here to set whole Nsharp display area by setting PixelExtent's rect area.
     * Adjust NsharpConstants.NSHARP_SkewTRectangle size will change nsharp display "visible" area.
     */
    public NsharpSkewTDisplay() {
        this(new PixelExtent(NsharpConstants.NSHARP_SkewTRectangle));
    }

    private NsharpSkewTDisplay(PixelExtent pixelExtent) {
        super(pixelExtent, new NsharpSkewTDescriptor(pixelExtent));
    }
    /* Chin MERGE
    public static NsharpSkewTDisplay getOrCreateSkewTDisplay (){
    	//System.out.println("NsharpSkewTDisplay getOrCreateSkewTDisplay () called");
		if(display == null){
			display = new NsharpSkewTDisplay();
		}
        return display;
	} */
    
    @Override
    public void dispose() {
    	
    	 int editorInstanceNum = EditorManager.getNumOfEditorInstance(editorNum);
    	 //System.out.println("NsharpSkewTDisplay disposed  editor num " + editorNum +" instance "+ editorInstanceNum);
    	 if (this.descriptor != null && editorInstanceNum <= 1) {
            super.dispose();
        }
    }
    /*Chin MERGE  moved this to customizeResourceList
    @Override
    public void setDescriptor(IDescriptor desc) {
    	super.setDescriptor(desc);
        bkgRsc = NsharpBackgroundResource.getOrCreateSkewTBkGResource();
        ResourceProperties props = new ResourceProperties();
        props.setVisible(true);
        props.setMapLayer(true);
        ResourcePair rp = new ResourcePair();
        rp.setResource(bkgRsc);
        rp.setProperties(props);
        rp.setLoadProperties(bkgRsc.getLoadProperties());
        desc.getResourceList().add(rp);

        skewRsc = NsharpSkewTResource.getOrCreateSkewtResource();
        ResourceProperties props1 = new ResourceProperties();
        props1.setVisible(true);
        props1.setMapLayer(true);
        ResourcePair rp1 = new ResourcePair();
        rp1.setResource(skewRsc);
        rp1.setProperties(props1);
        rp1.setLoadProperties(skewRsc.getLoadProperties());
        desc.getResourceList().add(rp1);
        skewRsc.setDesc((NsharpSkewTDescriptor)desc);

    }*/
    //Chin MERGE  added
    @Override
    public NsharpSkewTDescriptor getDescriptor() {
        return (NsharpSkewTDescriptor) super.getDescriptor();
    }
    
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);

        //this.target = target;
        //this.paintProps = paintProps;

        //Chin:: 11.5 changes, DrawCoordinatedPane() will call checkDrawTime(),
        // so we dont have to call it from here.
        //descriptor.checkDrawTime(loopProperties);
        
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

   // public IGraphicsTarget getTarget() {
    //    return target;
    //}

    //public PaintProperties getPaintProperties() {
    //    return paintProps;
    //}


    @Override
    protected void customizeResourceList(ResourceList resourceList) {
    	 // get a resource Data.
        AbstractResourceData resourceData = new NsharpSkewTResourceData();
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

        // Next add a background resource

        // get a resource Data.
        resourceData = new NsharpBkgResourceData(getDescriptor());
        // get a load properties
        loadProperties = new LoadProperties();
        colorableCapability = new ColorableCapability();
        colorableCapability.setColor(NsharpConstants.backgroundColor);
        loadProperties.getCapabilities().addCapability(colorableCapability);
        // get some resource properties
        resourceProperties = new ResourceProperties();
        resourceProperties.setVisible(true);
        resourceProperties.setMapLayer(true);
        resourceProperties.setSystemResource(true);
        // Make a resource pair
        resourcePair = new ResourcePair();
        resourcePair.setResourceData(resourceData);
        resourcePair.setLoadProperties(loadProperties);
        resourcePair.setProperties(resourceProperties);
        // add it to the resource list.
        resourceList.add(resourcePair);
    	/*
    	//System.out.println("NsharpSkewTDisplay customizeResourceList called...desc="+ this.descriptor.toString());
        if (resourceList.getResourcesByTypeAsType(NsharpSkewTResource.class).isEmpty()){
        	NsharpSkewTResource skewRsc = NsharpSkewTResource.createSkewtResource();
            ResourceProperties props1 = new ResourceProperties();
            props1.setVisible(true);
            props1.setMapLayer(true);
          //Chin MERGE
        	// bsteffen set this to system resource so it does not get serialized
            props1.setSystemResource(true);
           ResourcePair rp1 = new ResourcePair();
            rp1.setResource(skewRsc);
            rp1.setProperties(props1);
            rp1.setLoadProperties(skewRsc.getLoadProperties());
            resourceList.add(rp1);
            //skewRsc.setDesc(getDescriptor());
        }

        if (resourceList.getResourcesByTypeAsType(NsharpBackgroundResource.class).isEmpty()){
            LoadProperties loadProperties = new LoadProperties();
            ColorableCapability colorable1 = new ColorableCapability();
            colorable1.setColor(NsharpConstants.backgroundColor);
            loadProperties.getCapabilities().addCapability(colorable1);
            NsharpBackgroundResource bkgRsc = NsharpBackgroundResource.createSkewTBkGResource((NsharpSkewTDescriptor)this.descriptor);
            ResourceProperties props = new ResourceProperties();
            props.setVisible(true);
            props.setMapLayer(true);
            //Chin MERGE
        	//bsteffen set this to system resource so it does not get serialized
            props.setSystemResource(true);
            ResourcePair rp = new ResourcePair();
            rp.setResource(bkgRsc);
            rp.setProperties(props);
            rp.setLoadProperties(bkgRsc.getLoadProperties());
            resourceList.add(rp);
        }*/
    }

}
