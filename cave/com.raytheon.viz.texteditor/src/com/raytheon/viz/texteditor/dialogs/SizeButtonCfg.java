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
 * 
 * </pre>
 * 
 * @author XHuang
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class SizeButtonCfg implements ISerializableObject {
    @XmlElement(name = "LabelName")
    private String labelName;

    @XmlElement(name = "SizeEnabled")
    private boolean sizeEnabled;

    @XmlElement(name = "FontSize")
    private int fontSize;

    @XmlElement(name = "Selected")
    private boolean selected;

    public String getLabelName() {
        return labelName;
    }

    public void setLabelName(String labelName) {
        this.labelName = labelName;
    }

    public boolean isSizeEnabled() {
        return sizeEnabled;
    }

    public void setSizeEnabled(boolean sizeEnabled) {
        this.sizeEnabled = sizeEnabled;
    }

    public int getFontSize() {
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
}