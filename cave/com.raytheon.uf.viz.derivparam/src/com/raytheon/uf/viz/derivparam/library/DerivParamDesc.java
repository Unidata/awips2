/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.derivparam.library;

import java.util.ArrayList;
import java.util.List;

import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Meta data about a derived parameter script.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 21, 2009  3576     rjpeter     Initial version
 * Jan 14, 2014  2661     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "DerivedParameter")
public class DerivParamDesc {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DerivParamDesc.class);

    @XmlAttribute(required = true)
    private String abbreviation;

    @XmlAttribute
    private String name;

    @XmlAttribute
    @XmlJavaTypeAdapter(value = UnitAdapter.class)
    private Unit<?> unit = Unit.ONE;

    @XmlElement(name = "Method")
    private List<DerivParamMethod> methods;

    @XmlAttribute
    private boolean internal = false;

    @XmlAttribute
    private boolean isVector;

    public void merge(DerivParamDesc that) {
        if (that.methods != null) {
            for (DerivParamMethod method : that.methods) {
                this.addMethod(method);
            }
        }
        if (this.abbreviation == null) {
            this.abbreviation = that.abbreviation;
        } else if (!this.abbreviation.equals(that.abbreviation)) {
            if (that.abbreviation != null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Warning conflicting abbreviation in merge: "
                                + this.abbreviation + ", " + that.abbreviation);
            }
        }
        if (this.name == null) {
            this.name = that.name;
        } else if (!this.name.equals(that.name)) {
            if (that.name != null) {
                statusHandler.handle(Priority.DEBUG,
                        "Overriding name in merge: " + this.abbreviation + ": "
                                + this.name + ", " + that.name);
            }
        }
        if (this.unit == Unit.ONE) {
            this.unit = that.unit;
        } else if (!this.unit.equals(that.unit)) {
            if (that.unit != Unit.ONE) {
                statusHandler.handle(Priority.PROBLEM,
                        "Warning conflicting unit in merge: "
                                + this.abbreviation + ": " + this.unit + ", "
                                + that.unit);
            }
        }
        // Currently ignore merging of internal/isVector.
    }

    public String getAbbreviation() {
        return abbreviation;
    }

    public void setAbbreviation(String abbreviation) {
        this.abbreviation = abbreviation;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Unit<?> getUnit() {
        return unit;
    }

    public void setUnit(Unit<?> unit) {
        this.unit = unit;
    }

    public List<DerivParamMethod> getMethods() {
        return methods;
    }

    public void setMethods(List<DerivParamMethod> methods) {
        this.methods = methods;
    }

    public void addMethod(DerivParamMethod method) {
        if (methods == null) {
            methods = new ArrayList<DerivParamMethod>();
        }

        methods.add(method);
    }

    public boolean isVector() {
        return isVector;
    }

    public void setVector(boolean isVector) {
        this.isVector = isVector;
    }

    public void setInternal(boolean internal) {
        this.internal = internal;
    }

    public boolean isInternal() {
        return internal;
    }

}
