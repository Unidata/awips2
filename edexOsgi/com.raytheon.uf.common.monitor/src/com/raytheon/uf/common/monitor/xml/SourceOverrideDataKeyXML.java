package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * XML for Source override datakeys
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14 Nov, 2011 11456         dhladky     Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SourceOverrideDataKeyXML {

    @XmlAttribute(name = "dataKey")
    protected String dataKey;

    @XmlElements({ @XmlElement(name = "override") })
    private ArrayList<SourceOverrideParamXML> sourceOverrideParams;

    public void setDataKey(String dataKey) {
        this.dataKey = dataKey;
    }

    public String getDataKey() {
        return dataKey;
    }

    public void setSourceOverrideParams(
            ArrayList<SourceOverrideParamXML> sourceOverrideParams) {
        this.sourceOverrideParams = sourceOverrideParams;
    }

    public ArrayList<SourceOverrideParamXML> getSourceOverrideParams() {
        return sourceOverrideParams;
    }

    /**
     * Gets the param object for a given param name
     * 
     * @param paramName
     * @return
     */
    public SourceOverrideParamXML getSourceOverrideByParam(String paramName) {
        if (sourceOverrideParams != null) {
            for (SourceOverrideParamXML param : sourceOverrideParams) {
                if (param.getParam().equals(paramName)) {
                    return param;
                }
            }
        }

        return null;
    }

}
