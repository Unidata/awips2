/**
 * 
 * ggov.noaa.nws.ncep.ui.nsharp.display.NsharpHodoPaneDescriptor
 * 
 * This java class performs the NSHARP NsharpHodoPaneDescriptor functions.
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

import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpHodoPaneResource;

import java.util.List;

import com.raytheon.uf.viz.core.PixelExtent;

public class NsharpHodoPaneDescriptor extends NsharpAbstractPaneDescriptor {
   
    public NsharpHodoPaneDescriptor(PixelExtent pe) {
        super(pe);
        //System.out.println("NsharpHodoPaneDescriptor  created " + this.toString());  
    }
    public NsharpHodoPaneDescriptor(PixelExtent pe, int paneNumber) {
        super(pe, paneNumber);
    }
    public NsharpHodoPaneResource getHodoResource() {
        List<NsharpHodoPaneResource> list = resourceList
                .getResourcesByTypeAsType(NsharpHodoPaneResource.class);
        if (list != null && !list.isEmpty()) {
            return list.get(0);
        }
        return null;
    }

}
