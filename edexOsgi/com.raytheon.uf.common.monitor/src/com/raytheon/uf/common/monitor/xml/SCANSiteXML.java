package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlAccessorType(XmlAccessType.NONE)
public class SCANSiteXML implements ISerializableObject {

    @XmlElement(name = "scanSite")
    private String scanSite;

    @XmlAttribute(name = "menuLocation")
    private String menuLocation;

    @XmlElements({ @XmlElement(name = "modelParameter") })
    private ArrayList<SCANModelParameterXML> modelParameters;

    public String getScanSite() {
        return scanSite;
    }

    public void setScanSite(String scanSite) {
        this.scanSite = scanSite;
    }

    public ArrayList<SCANModelParameterXML> getModelParameters() {
        return modelParameters;
    }

    public void setModelParameters(
            ArrayList<SCANModelParameterXML> modelParameters) {
        this.modelParameters = modelParameters;
    }

    public void addModelParameter(SCANModelParameterXML paramXML) {
        if (modelParameters == null) {
            modelParameters = new ArrayList<SCANModelParameterXML>();
        }

        modelParameters.add(paramXML);
    }

    public SCANModelParameterXML getModelParameter(String smodelParameter) {
        SCANModelParameterXML paramXML = null;

        if (modelParameters != null) {
            for (SCANModelParameterXML modelParamXML : modelParameters) {
                if (modelParamXML.getParameterName().equals(smodelParameter)) {
                    paramXML = modelParamXML;
                    break;
                }
            }
        }

        return paramXML;
    }

    public void setMenuLocation(String menuLocation) {
        this.menuLocation = menuLocation;
    }

    public String getMenuLocation() {
        return menuLocation;
    }

}
