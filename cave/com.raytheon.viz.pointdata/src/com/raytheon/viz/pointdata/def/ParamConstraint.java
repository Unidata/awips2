package com.raytheon.viz.pointdata.def;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;

/**
 * ParamConstraint object
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/10/2019   71272      Mark Peters Initial Creation
 *
 * </pre>
 *
 * @author mpeters
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ParamConstraint {

    @XmlAttribute
    private String param;

    @XmlElement
    private RequestConstraint constraint;

    /**
     * Empty constructor for serialization
     */
    public ParamConstraint() {
    }

    public ParamConstraint(String param, RequestConstraint constraint) {
        this.param = param;
        this.constraint = constraint;
    }

    public ParamConstraint(ParamConstraint other) {
        this.param = other.param;
        this.constraint = other.constraint.clone();
    }

    public String getParam() {
        return param;
    }

    public void setParam(String param) {
        this.param = param;
    }

    // Switch to separate getter/setter for type and value, so we don't return
    // anything mutable?
    public RequestConstraint getConstraint() {
        return constraint;
    }

    public void setConstraint(RequestConstraint constraint) {
        this.constraint = constraint;
    }

    @Override
    public String toString() {
        return (param + " " + constraint.getConstraintType().getOperand() + " "
                + constraint.getConstraintValue()).trim();
    }
}