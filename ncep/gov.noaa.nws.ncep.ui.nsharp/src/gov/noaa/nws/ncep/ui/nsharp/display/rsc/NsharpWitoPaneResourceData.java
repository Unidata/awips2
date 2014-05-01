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

public class NsharpWitoPaneResourceData extends AbstractResourceData {
	private NsharpWitoPaneResource witoPaneRsc;
	@Override
	public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		NsharpAbstractPaneDescriptor desc=(NsharpAbstractPaneDescriptor)descriptor;
		//System.out.println("NsharpWitoPaneResourceData construct called Panes="+desc.getPaneNumber());
		witoPaneRsc =  new NsharpWitoPaneResource(this, loadProperties, desc);
		return witoPaneRsc;		
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
