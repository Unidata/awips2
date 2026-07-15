package com.raytheon.viz.pointdata.def;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;

/**
 * 
 * Implementation of ConditionalFilterElement class used in Conditional Filter
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2012      #615       S. Gurung   Initial creation
 * 12/10/2019   72280      K Sunil     Moved from NCP's gov.noaa.nws.ncep.viz.rsc.plotdata to D2D
 * 09/15/2021   95459      smanoj      Conditional Filter AND/OR update
 * 
 * </pre>
 *
 * @author sgurung
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "ConditionalFilterElement")
public class ConditionalFilterElement {
    @XmlAttribute(name = "paramName")
    private String paramName;

    @XmlAttribute(name = "constraintType")
    private String constraintType;

    @XmlAttribute(name = "value")
    private String value;

    @XmlAttribute(name = "connectVal")
    private String connectVal;

    public ConditionalFilterElement() {
        paramName = "";
        constraintType = "";
        value = "";
        connectVal = "";
    }

    public ConditionalFilterElement(String pName, String cType, String val,
            String conVal) {
        paramName = pName;
        constraintType = cType;
        value = val;
        connectVal = conVal;
    }

    public String getParamName() {
        return paramName;
    }

    public String getConstraintType() {
        return constraintType;
    }

    public String getValue() {
        return value;
    }

    public String getConnectValue() {
        return connectVal;
    }

    public void setParamName(String pName) {
        paramName = pName;
    }

    public void setConstraintType(String cType) {
        constraintType = cType;
    }

    public void setValue(String val) {
        value = val;
    }

    public void setConnectValue(String conVal) {
        connectVal = conVal;
    }

    @Override
    public String toString() {
        return paramName + " " + constraintType + " " + value + " "
                + connectVal;
    }
}
