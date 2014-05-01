package gov.noaa.nws.ncep.ui.nsharp.display.rsc;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

public class NsharpDataPaneResourceData extends AbstractResourceData {

	@Override
	public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		NsharpAbstractPaneDescriptor desc=(NsharpAbstractPaneDescriptor)descriptor;
		//System.out.println("NsharpDataPaneResourceData construct called Panes="+desc.getPaneNumber());
		return new NsharpDataPaneResource(this, loadProperties, desc);
		
	}

	@Override
	public void update(Object updateData) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean equals(Object obj) {
		// TODO Auto-generated method stub
		return false;
	}

}
