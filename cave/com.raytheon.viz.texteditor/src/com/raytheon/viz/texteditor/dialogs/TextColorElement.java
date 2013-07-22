package com.raytheon.viz.texteditor.dialogs;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.ISerializableObject;

import org.eclipse.swt.graphics.RGB;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 24, 2013  DR 15733   Xiaochuan     Initial creation
 * 
 * </pre>
 * 
 * @author XHuang
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "TextColorElement")
public class TextColorElement implements ISerializableObject {
	
	@XmlAttribute
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	private RGB color;
		
	@XmlAttribute
    private String paramName;

    public RGB getColor() {
		return color;
	}

	public void setColor(RGB color) {
		this.color = color;
	}

	public String getParamName() {
		return paramName;
	}

	public void setParamName(String paramName) {
		this.paramName = paramName;
	}

}