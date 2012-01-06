/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTBkgResourceData
 * 
 * This java class performs the NSHARP NsharpSkewTBkgResourceData functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.skewt.rsc;

import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

public class NsharpBkgResourceData extends AbstractResourceData {
	private NsharpSkewTDescriptor desc;
	public NsharpBkgResourceData(NsharpSkewTDescriptor desc) {
		this.desc = desc;
	}

	@Override
	public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		return new NsharpBackgroundResource(this, loadProperties, desc);
	} 
	// Chin MERGE
	@Override
	public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        return true;
	}

	@Override
	public void update(Object updateData) {
		// TODO Auto-generated method stub

	}
	// Chin MERGE added simple hashCode
    @Override
    public int hashCode() {
        return NsharpBkgResourceData.class.hashCode();
    }

}
