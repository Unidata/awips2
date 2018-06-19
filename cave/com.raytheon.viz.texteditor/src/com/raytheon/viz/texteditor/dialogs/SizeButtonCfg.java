package com.raytheon.viz.texteditor.dialogs;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 07, 2013  DR 15733  Xiaochuan     Initial creation
 * July 25, 2013 DR 15733  G. Hull       now part of TextEditorCfg ; rm sizeEnabled
 * 
 * </pre>
 * 
 * @author XHuang
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class SizeButtonCfg implements ISerializableObject {
	
    private final int DEFAULT_FONT_SIZE = 11;

    @XmlElement(name = "LabelName")
    private String labelName="Medium";

    @XmlElement(name = "FontName")
    private String fontName = "Courier";
    
	@XmlElement(name = "FontSize")
    private int fontSize= DEFAULT_FONT_SIZE;

    @XmlElement(name = "Selected")
    private boolean selected=true;

    public String getLabelName() {
        return labelName;
    }

    public void setLabelName(String labelName) {
        this.labelName = labelName;
    }

    public int getFontSize() {
    	if( fontSize < 5 || fontSize > 40 ) { // sanity check
    		return DEFAULT_FONT_SIZE;
    	}
    	return fontSize;
    }

    public void setFontSize(int fontSize) {
    	this.fontSize = fontSize;
    }

    public boolean isSelected() {
        return selected;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }
    
    public String getFontName() {
		return fontName;
	}

	public void setFontName(String fontName) {
		this.fontName = fontName;
	}
}