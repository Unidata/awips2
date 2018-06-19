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
package com.raytheon.uf.edex.netcdf.description.field.indirect;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.AbstractFieldDescription;
import com.raytheon.uf.edex.netcdf.description.field.direct.AttributeDescription;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableAttributeDescription;
import com.raytheon.uf.edex.netcdf.description.field.direct.VariableDescription;

/**
 * A description that uses a {@link Variable}'s {@link Attribute} to identify an
 * other NetCDF field that contains the desired value(s).
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2016 5450       nabowle     Initial creation
 * May 19, 2016 5584       nabowle     Updates for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ReferencedFieldDescription extends AbstractFieldDescription {


    /**
     * The Variable's Attribute who's value contains the name of an other NetCDF
     * field that contains the desired value.
     */
    @XmlElement(required = true)
    private VariableAttributeDescription variableAttribute;

    /**
     * Optional index into {@link #variableAttribute}'s array value. If the
     * value is not an array, this should be null.
     */
    @XmlAttribute
    private Integer attributeIndex;

    /**
     * The search space to use when trying to locate the NetCDF field referenced
     * by {@link #variableAttribute}'s value.
     */
    @XmlAttribute
    private SearchSpace searchSpace = SearchSpace.VARIABLES_FIRST;

    /**
     * Constructor.
     */
    public ReferencedFieldDescription() {
        super();
    }

    /**
     * @return the variableAttribute
     */
    public VariableAttributeDescription getVariableAttribute() {
        return variableAttribute;
    }

    /**
     * @param variableAttribute
     *            the variableAttribute to set
     */
    public void setVariableAttribute(
            VariableAttributeDescription variableAttribute) {
        this.variableAttribute = variableAttribute;
    }

    /**
     * @return the searchSpace. VARIABLES_FIRST will be returned if null would
     *         be returned otherwise.
     */
    public SearchSpace getSearchSpace() {
        return searchSpace == null ? SearchSpace.VARIABLES_FIRST
                : this.searchSpace;
    }

    /**
     * @param searchSpace
     *            the searchSpace to set. If null, VARIABLES_FIRST is used.
     */
    public void setSearchSpace(SearchSpace searchSpace) {
        if (searchSpace == null) {
            this.searchSpace = SearchSpace.VARIABLES_FIRST;
        } else {
            this.searchSpace = searchSpace;
        }
    }

    /**
     * @return the attributeIndex
     */
    public Integer getAttributeIndex() {
        return attributeIndex;
    }

    /**
     * @param attributeIndex
     *            the attributeIndex to set
     */
    public void setAttributeIndex(Integer attributeIndex) {
        this.attributeIndex = attributeIndex;
    }

    @Override
    public String getString(NetcdfFile file) throws InvalidDescriptionException {
        AbstractFieldDescription referenced = findReferencedField(file);
        if (referenced != null) {
            return referenced.getString(file);
        }
        return null;
    }

    @Override
    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        AbstractFieldDescription referenced = findReferencedField(file);
        if (referenced != null) {
            return referenced.getString(file, index);
        }
        return null;
    }

    @Override
    public Number getNumber(NetcdfFile file) throws InvalidDescriptionException {
        AbstractFieldDescription referenced = findReferencedField(file);
        if (referenced != null) {
            return referenced.getNumber(file);
        }
        return null;
    }

    @Override
    public Number getNumber(NetcdfFile file, int n)
            throws InvalidDescriptionException {
        AbstractFieldDescription referenced = findReferencedField(file);
        if (referenced != null) {
            return referenced.getNumber(file, n);
        }
        return null;
    }

    @Override
    public long getLength(NetcdfFile file) throws InvalidDescriptionException {
        AbstractFieldDescription referenced;
        try {
            referenced = findReferencedField(file);
        } catch (InvalidDescriptionException e) {
            return 0;
        }
        if (referenced != null) {
            return referenced.getLength(file);
        }
        return 0;
    }

    @Override
    public boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException {
        AbstractFieldDescription referenced = findReferencedField(file);
        if (referenced != null) {
            return referenced.isNumeric(file);
        }
        return false;
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        AbstractFieldDescription referenced = findReferencedField(file);
        if (referenced != null) {
            return referenced.isPresent(file);
        }
        return false;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (this.variableAttribute == null) {
            throw new InvalidDescriptionException("variableAttribute is null.");
        }
        this.variableAttribute.validate();

        if (this.attributeIndex != null && this.attributeIndex.intValue() < 0) {
            throw new InvalidDescriptionException("attributeIndex is negative.");
        }
    }


    /**
     * Finds the referenced field, if possible, by first retrieving the field's
     * name from {@link #variableAttribute} and then finding the field in the
     * NetcdfFile.
     *
     * @param file
     *            The netcdf file.
     * @return The AbstractFieldDescription for the field if found, or null if
     *         not found.
     * @throws InvalidDescriptionException
     */
    public AbstractFieldDescription findReferencedField(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.variableAttribute == null) {
            return null;
        }

        String referencedName;
        if (this.attributeIndex != null) {
            referencedName = this.variableAttribute.getString(file,
                    this.attributeIndex.intValue());
        } else {
            referencedName = this.variableAttribute.getString(file);
        }
        if (referencedName == null) {
            return null;
        }

        AbstractFieldDescription referencedField = null;
        SearchSpace ss = getSearchSpace();
        Variable var;
        Attribute attr;
        switch (ss) {
        case VARIABLES_ONLY:
            var = file.findVariable(referencedName);
            if (var != null) {
                referencedField = new VariableDescription();
                referencedField.setName(referencedName);
            }
            break;
        case GLOBAL_ATTRIBUTES_ONLY:
            attr = file.findGlobalAttribute(referencedName);
            if (attr != null) {
                referencedField = new AttributeDescription();
                referencedField.setName(referencedName);
            }
            break;
        case VARIABLES_FIRST:
            var = file.findVariable(referencedName);
            if (var != null) {
                referencedField = new VariableDescription();
                referencedField.setName(referencedName);
            } else {
                attr = file.findGlobalAttribute(referencedName);
                if (attr != null) {
                    referencedField = new AttributeDescription();
                    referencedField.setName(referencedName);
                }
            }
            break;
        case GLOBAL_ATTRIBUTES_FIRST:
            attr = file.findGlobalAttribute(referencedName);
            if (attr != null) {
                referencedField = new AttributeDescription();
                referencedField.setName(referencedName);
            } else {
                var = file.findVariable(referencedName);
                if (var != null) {
                    referencedField = new VariableDescription();
                    referencedField.setName(referencedName);
                }
            }
            break;
        default:
            throw new InvalidDescriptionException("Unhandled search space "
                    + ss);
        }

        return referencedField;
    }

    public static enum SearchSpace {
        VARIABLES_FIRST,

        GLOBAL_ATTRIBUTES_FIRST,

        VARIABLES_ONLY,

        GLOBAL_ATTRIBUTES_ONLY;
    }
}
