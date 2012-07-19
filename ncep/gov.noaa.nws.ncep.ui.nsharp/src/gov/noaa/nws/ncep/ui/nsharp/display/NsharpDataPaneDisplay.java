/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.display.NsharpDataPaneDisplay
 * 
 * This java class performs the NSHARP NsharpDataPaneDisplay functions.
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

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpDataPaneResourceData;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;

public class NsharpDataPaneDisplay extends NsharpAbstractPaneDisplay {
    public NsharpDataPaneDisplay(PixelExtent pixelExtent,int paneNumber) {
        super(pixelExtent,paneNumber, "DataPane",(NsharpAbstractPaneDescriptor)( new NsharpAbstractPaneDescriptor(pixelExtent, paneNumber)));
    }
    
    @Override
    public NsharpAbstractPaneDescriptor getDescriptor() {
        return (NsharpAbstractPaneDescriptor) super.getDescriptor();
    }
    
    @Override
    protected void customizeResourceList(ResourceList resourceList) {
    	AbstractResourceData resourceData = new NsharpDataPaneResourceData();
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
