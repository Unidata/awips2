/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.display.NsharpSpcGraphsPaneDisplay
 * 
 * This java class performs the NSHARP NsharpSpcGraphsPaneDisplay functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 07/2/2012	229			Chin Chen	Initial coding for multiple display panes implementation
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpSpcGraphsPaneResourceData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class NsharpSpcGraphsPaneDisplay extends NsharpAbstractPaneDisplay {
    public NsharpSpcGraphsPaneDisplay(PixelExtent pixelExtent,int paneNumber) {
        super(pixelExtent,paneNumber, "HodoPane",(NsharpAbstractPaneDescriptor)( new NsharpSpcGraphsPaneDescriptor(pixelExtent, paneNumber)));
    }
    
    public NsharpSpcGraphsPaneDisplay() {
		super();
		// TODO Auto-generated constructor stub
	}

	public NsharpSpcGraphsPaneDisplay(PixelExtent pixelExtent, int paneNumber,
			String name, NsharpAbstractPaneDescriptor desc) {
		super(pixelExtent, paneNumber, name, desc);
		// TODO Auto-generated constructor stub
	}

	//@Override
    //public NsharpSpcGraphsPaneDescriptor getDescriptor() {
    //    return (NsharpSpcGraphsPaneDescriptor) super.getDescriptor();
    //}
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	super.paint(target, paintProps);
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
    	AbstractResourceData resourceData = new NsharpSpcGraphsPaneResourceData();
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
