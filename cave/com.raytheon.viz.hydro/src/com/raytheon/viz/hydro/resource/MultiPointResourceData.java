package com.raytheon.viz.hydro.resource;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.resource.HydroPointResourceData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2011            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "multiPointResourceData")
public class MultiPointResourceData extends
		HydroPointResourceData<MultiPointResource> {

	public MultiPointResourceData() {
		super();
	}

	public MultiPointResourceData(String string, RGB color,
			Coordinate location, Style style) {
		super(string, color, location, style);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
	 * .uf.viz.core.rsc.LoadProperties,
	 * com.raytheon.uf.viz.core.drawables.IDescriptor)
	 */
	@Override
	public MultiPointResource construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		MultiPointResource multiPointResource = new MultiPointResource(this,
				loadProperties);
		return multiPointResource;
	}

}
