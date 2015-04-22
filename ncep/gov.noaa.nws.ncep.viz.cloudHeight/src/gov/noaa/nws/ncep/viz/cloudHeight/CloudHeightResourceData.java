/**
 * 
 */
package gov.noaa.nws.ncep.viz.cloudHeight;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;

/**
 * @author mli
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NcCloudHeightResourceData")
public class CloudHeightResourceData extends AbstractResourceData {

	public CloudHeightResourceData() {
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon.uf.viz.core.comm.LoadProperties, com.raytheon.uf.viz.core.drawables.IDescriptor)
	 */
	@Override
	public CloudHeightResource construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		return new CloudHeightResource(this, loadProperties);
	}

	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object)
	 */
	@Override
	public void update(Object updateData) {
		// add code to update the station data list
	}

	@Override
	public boolean equals(Object obj) {
		// TODO Implement this
		return false;
	}

}
