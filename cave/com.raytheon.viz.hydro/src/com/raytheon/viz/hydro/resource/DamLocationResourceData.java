package com.raytheon.viz.hydro.resource;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.data.DamMaster;
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
@XmlRootElement(name = "damLocationResourceData")
public class DamLocationResourceData extends
		HydroPointResourceData<DamLocationResource> {
	@XmlElement
	List<DamMaster> damList;

	public DamLocationResourceData() {
		super();
	}

	public DamLocationResourceData(String name, RGB color, Coordinate location,
			Style style) {
		super(name, color, location, style);
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
	public DamLocationResource construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		return new DamLocationResource(this, loadProperties);
	}

	public void setDamList(List<DamMaster> damList) {
		this.damList = damList;
	}

	public List<DamMaster> getDamList() {
		return this.damList;
	}

}
