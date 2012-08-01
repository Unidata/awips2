/**
 * 
 * ggov.noaa.nws.ncep.ui.nsharp.display.NsharpSpcGraphsPaneDescriptor
 * 
 * This java class performs the NSHARP NsharpSpcGraphsPaneDescriptor functions.
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

import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpSpcGraphsPaneResource;

import java.util.List;

import com.raytheon.uf.viz.core.PixelExtent;

public class NsharpSpcGraphsPaneDescriptor extends NsharpAbstractPaneDescriptor {
   
    public NsharpSpcGraphsPaneDescriptor(PixelExtent pe) {
        super(pe);
        //System.out.println("NsharpSpcGraphsPaneDescriptor  created " + this.toString());  
    }
    public NsharpSpcGraphsPaneDescriptor(PixelExtent pe, int paneNumber) {
        super(pe, paneNumber);
    }
    public NsharpSpcGraphsPaneResource getSpcGraphsResource() {
        List<NsharpSpcGraphsPaneResource> list = resourceList
                .getResourcesByTypeAsType(NsharpSpcGraphsPaneResource.class);
        if (list != null && !list.isEmpty()) {
            return list.get(0);
        }
        return null;
    }

}
