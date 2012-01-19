package com.raytheon.viz.hydrocommon.whfslib.colorthreshold;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

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
@XmlRootElement(name = "colorLookupParameters")
public class ColorLookupParameters {
	@XmlElement
	private String applicationName;

	@XmlElement
	private String coloruseName;

	@XmlElement
	private int duration;

	@XmlElement
	private String thresholdUnit;

	public ColorLookupParameters(String applicationName, String coloruseName,
			int duration, String thresholdUnit) {
		this.applicationName = applicationName;
		this.coloruseName = coloruseName;
		this.duration = duration;
		this.thresholdUnit = thresholdUnit;
	}

	public ColorLookupParameters() {
	}

	public String getApplicationName() {
		return applicationName;
	}

	public void setApplicationName(String applicationName) {
		this.applicationName = applicationName;
	}

	public String getColoruseName() {
		return coloruseName;
	}

	public void setColoruseName(String coloruseName) {
		this.coloruseName = coloruseName;
	}

	public int getDuration() {
		return duration;
	}

	public void setDuration(int duration) {
		this.duration = duration;
	}

	public String getThresholdUnit() {
		return thresholdUnit;
	}

	public void setThresholdUnit(String thresholdUnit) {
		this.thresholdUnit = thresholdUnit;
	}
}
