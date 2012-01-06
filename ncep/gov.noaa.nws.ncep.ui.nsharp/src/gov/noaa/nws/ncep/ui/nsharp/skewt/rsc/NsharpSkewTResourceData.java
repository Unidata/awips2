/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResourceData
 * 
 * This java class performs the NSHARP NsharpSkewTResourceData functions.
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

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

public class NsharpSkewTResourceData extends AbstractResourceData {

	public NsharpSkewTResourceData() {
		super();
		// TODO Auto-generated constructor stub
	}

	@Override
	public NsharpSkewTResource construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		return new NsharpSkewTResource(this, loadProperties);
	}

	
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
	
    @Override
    public int hashCode() {
        return NsharpSkewTResourceData.class.hashCode();
    }

}
