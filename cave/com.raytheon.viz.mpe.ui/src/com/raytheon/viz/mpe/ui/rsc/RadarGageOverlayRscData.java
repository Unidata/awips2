package com.raytheon.viz.mpe.ui.rsc;

import javax.xml.bind.annotation.XmlElement;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

public class RadarGageOverlayRscData extends AbstractResourceData {

	@XmlElement
	protected String name;

	private RGB color = RGBColors.getRGBColor("SandyBrown");

	private int red;

	private int green;

	private int blue;

	public RadarGageOverlayRscData() {
		super();
	}

	public RadarGageOverlayRscData(String name, RGB color) {
		super();
		if (color != null) {
			this.color = color;
		}
		this.name = name;
	}

	@Override
	public RadarGageOverlayRsc construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		return new RadarGageOverlayRsc(this, loadProperties);
	}

	@XmlElement
	public int getRed() {
		return red;
	}

	public void setRed(int red) {
		this.red = red;
		this.color.red = red;
	}

	@XmlElement
	public int getGreen() {
		return green;
	}

	public void setGreen(int green) {
		this.green = green;
		this.color.green = green;
	}

	@XmlElement
	public int getBlue() {
		return blue;
	}

	public void setBlue(int blue) {
		this.blue = blue;
		this.color.blue = blue;
	}

	public RGB getColor() {
		return color;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public void update(Object updateData) {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		RadarGageOverlayRscData other = (RadarGageOverlayRscData) obj;
		if (color == null) {
			if (other.color != null)
				return false;
		} else if (!color.equals(other.color))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}
