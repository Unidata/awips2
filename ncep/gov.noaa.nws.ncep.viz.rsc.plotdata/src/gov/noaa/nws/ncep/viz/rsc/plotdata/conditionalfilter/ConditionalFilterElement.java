package gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter;

import javax.xml.bind.annotation.XmlAccessOrder;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorOrder;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * 
 * Implementation of ConditionalFilterElement class used in Conditional Filter
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2012      #615       S. Gurung   Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
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
    	return paramName+" "+constraintType+" "+value;
    }
}
