/**
 * 
 */
package gov.noaa.nws.ncep.viz.tools.logos;

import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;

/**
 * @author mli
 *
 */
public class LogosResourceData extends AbstractResourceData {

	/**
	 * 
	 */
	public LogosResourceData() {
		super();
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon.uf.viz.core.comm.LoadProperties, com.raytheon.uf.viz.core.drawables.IDescriptor)
	 */
	@Override
	public LogosResource construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		// TODO Auto-generated method stub
		return new LogosResource(this, loadProperties);
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object)
	 */
	@Override
	public void update(Object updateData) {
		// TODO Auto-generated method stub

	}

    @Override
    public String toString() {
    	return "Logos";  // what's this used for?
    }    

	@Override
	public boolean equals(Object obj) {

		if (this == obj) {
			return true;
		}
		if (obj == null || obj instanceof LogosResourceData == false) {
			return false;
		}
		LogosResourceData other = (LogosResourceData) obj;

		// TODO finish this....
		return true;
	}
}
