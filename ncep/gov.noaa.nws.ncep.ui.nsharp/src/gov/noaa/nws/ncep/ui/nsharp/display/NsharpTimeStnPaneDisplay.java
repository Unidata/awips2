/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.display.NsharpTimeStnPaneDisplay
 * 
 * This java class performs the NSHARP NsharpTimeStnPaneDisplay functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/22/2012	229			Chin Chen	Initial coding for multiple display panes implementation
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpTimeStnPaneResourceData;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class NsharpTimeStnPaneDisplay extends NsharpAbstractPaneDisplay {
    public NsharpTimeStnPaneDisplay(PixelExtent pixelExtent,int paneNumber) {
        super(pixelExtent,paneNumber, "TimeStnPane", new NsharpAbstractPaneDescriptor(pixelExtent, paneNumber));
    }
    
    public NsharpTimeStnPaneDisplay() {
		super();
		// TODO Auto-generated constructor stub
	}

	@Override
    public NsharpAbstractPaneDescriptor getDescriptor() {
        return (NsharpAbstractPaneDescriptor) super.getDescriptor();
    }
    

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
    	AbstractResourceData resourceData = new NsharpTimeStnPaneResourceData();
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
