package com.raytheon.viz.mpe.ui.rsc;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.ColorLookupParameters;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "pointResourceData")
public abstract class PointResourceData<T extends HydroPointResource<?>>
		extends
		HydroPointResourceData<T> {

	protected List<Colorvalue> colorSet;

	protected MPEDisplayManager mpeDisplayManager;

	@XmlElement
	protected ColorLookupParameters lookupParameters;

	public PointResourceData() {
		super();
		this.mpeDisplayManager = MPEDisplayManager.getCurrent();
	}

	public PointResourceData(MPEDisplayManager mpeDisplayManager, String name,
			ColorLookupParameters lookupParameters) {
		super();
		this.name = name;
		this.mpeDisplayManager = mpeDisplayManager;
		this.lookupParameters = lookupParameters;
	}

	public List<Colorvalue> getColorSet() {
		if(this.colorSet == null){
			String user_id = System.getProperty("user.name");
			this.colorSet = GetColorValues.get_colorvalues(user_id,
					lookupParameters.getApplicationName(),
					lookupParameters.getColoruseName(),
					lookupParameters.getDuration(),
					lookupParameters.getThresholdUnit(), null);
		}
		return this.colorSet;
	}

	public void setColorSet(List<Colorvalue> colorSet) {
		this.colorSet = colorSet;
	}

	public MPEDisplayManager getMPEDisplayManager() {
		return mpeDisplayManager;
	}

	public void setMPEDisplayManager(MPEDisplayManager mpeDisplayManager) {
		this.mpeDisplayManager = mpeDisplayManager;
	}
}
