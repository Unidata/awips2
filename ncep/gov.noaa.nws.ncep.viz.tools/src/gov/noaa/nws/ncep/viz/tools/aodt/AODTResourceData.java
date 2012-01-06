/**
 * 
 */
package gov.noaa.nws.ncep.viz.tools.aodt;

import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;

/**
 * @author mli
 *
 */
public class AODTResourceData extends AbstractResourceData {

	/**
	 * 
	 */
	public AODTResourceData() {
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon.uf.viz.core.comm.LoadProperties, com.raytheon.uf.viz.core.drawables.IDescriptor)
	 */
	@Override
	public AODTResource construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		// TODO Auto-generated method stub
		return new AODTResource(this, loadProperties);
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object)
	 */
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
