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
package com.raytheon.uf.edex.plugin.nos.description;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.edex.netcdf.description.VariableDescription;

/**
 * Contains the data variable, dataset id, and parameter information for the
 * data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 25, 2015 4696       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class NosProductDescription {

    @XmlElement(required = true)
    private VariableDescription data;

    @XmlElement(required = true)
    private String units;

    @XmlElement(required = true)
    private String parameter;

    @XmlElement(required = true)
    private String parameterName;


    /**
     * Constructor.
     */
    public NosProductDescription() {
        super();
    }

    /**
     * @return the data
     */
    public VariableDescription getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(VariableDescription data) {
        this.data = data;
    }

    /**
     * @return the unit
     */
    public String getUnits() {
        return units;
    }

    /**
     * @param unit
     *            the unit to set
     */
    public void setUnits(String unit) {
        this.units = unit;
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the parameterName
     */
    public String getParameterName() {
        return parameterName;
    }

    /**
     * @param parameterName
     *            the parameterName to set
     */
    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    @Override
    public String toString() {
        return "NosProductDescription [data="
                + data.getName() + ", parameter=" + parameter + "]";
    }

}
