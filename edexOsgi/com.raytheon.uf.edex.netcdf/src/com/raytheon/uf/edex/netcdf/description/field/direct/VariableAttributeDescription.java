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
package com.raytheon.uf.edex.netcdf.description.field.direct;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Contains the name of a {@link Variable} and an {@link Attribute} of that
 * Variable, and provides methods to extract information about and from the
 * specified Attribute.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2016 5450       nabowle     Initial creation
 * May 19, 2016 5584       nabowle     Updates for consolidation.
 * 
 * </pre>
 * 
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class VariableAttributeDescription extends AttributeDescription {

    @XmlAttribute
    private String attributeName;

    @XmlAttribute
    private String variableName;

    /**
     * Constructor.
     */
    public VariableAttributeDescription() {
        super();
    }

    /**
     * Use {@link #getVariableName()} and {@link #getAttributeName()}.
     */
    @Deprecated
    @Override
    public String getName() {
        return this.variableName + ":" + this.attributeName;
    }


    /**
     * Use {@link #setVariableName(String)} and
     * {@link #setAttributeName(String)}.
     *
     * @throws IllegalArgumentException
     *             always
     */
    @Deprecated
    @Override
    public void setName(String name) {
        throw new IllegalArgumentException(
                "Not implemented. Use setVariableName(String) and setAttributeName(String)");
    }

    /**
     * @return the attributeName
     */
    public String getAttributeName() {
        return attributeName;
    }

    /**
     * @param attributeName
     *            the attributeName to set
     */
    public void setAttributeName(String attributeName) {
        this.attributeName = attributeName;
    }

    /**
     * @return the variableName
     */
    public String getVariableName() {
        return variableName;
    }

    /**
     * @param variableName
     *            the variableName to set
     */
    public void setVariableName(String variableName) {
        this.variableName = variableName;
    }

    @Override
    public boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException {
        validate();
        Attribute attr = getAttribute(file);
        if (attr == null) {
            throw new InvalidDescriptionException(
                    "Coudld not find a variable attribute for variable "
                            + this.variableName + " and attribute "
                            + this.attributeName);
        }
        return attr.getDataType().isNumeric();
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        validate();
        return getAttribute(file) != null;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (this.variableName == null) {
            throw new InvalidDescriptionException(
                    "variableName is not configured.");
        }
        if (this.attributeName == null) {
            throw new InvalidDescriptionException(
                    "attributeName is not configured.");
        }
    }

    @Override
    protected Attribute getAttribute(NetcdfFile file) {
        Variable var = file.findVariable(this.variableName);
        if (var == null) {
            return null;
        }
        return var.findAttribute(this.attributeName);
    }

    @Override
    protected boolean fieldsSet() {
        return this.variableName != null && this.attributeName != null;
    }
}
