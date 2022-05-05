package com.raytheon.viz.pointdata.def;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

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

    public ConditionalFilterElement() {
        paramName = "";
        constraintType = "";
        value = "";
    }

    public ConditionalFilterElement(String pName, String cType, String val) {
        paramName = pName;
        constraintType = cType;
        value = val;
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

    public void setParamName(String pName) {
        paramName = pName;
    }

    public void setConstraintType(String cType) {
        constraintType = cType;
    }

    public void setValue(String val) {
        value = val;
    }

    @Override
    public String toString() {
        return paramName + " " + constraintType + " " + value;
    }
}
