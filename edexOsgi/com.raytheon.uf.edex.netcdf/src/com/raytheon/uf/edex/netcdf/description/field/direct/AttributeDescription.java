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

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.AbstractFieldDescription;

/**
 *
 * An attribute value is loaded from XML and then combined with a
 * {@link NetcdfFile} to extract a desired value. Normally the XML specifies an
 * attribute to read from the netCDF file but in some cases the netcdf file does
 * not have enough information so values can be directly in the xml.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * Aug 25, 2015  4699     nabowle   Extracted from Pointset netcdf plugin,
 *                                  renamed, and refactored.
 * Sep 09, 2015  4696     nabowle   Override new methods.
 * Dec 08, 2015  5059     nabowle   Add isNumeric() and isPresent().
 * Mar 21, 2016  5450     nabowle   Minor refactorings for VariableAttributeDescription.
 * May 19, 2016  5584     nabowle   Updates for consolidation.
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AttributeDescription extends AbstractFieldDescription {

    public AttributeDescription() {
        super();
    }

    @Override
    public String getString(NetcdfFile file) throws InvalidDescriptionException {
        if (fieldsSet()) {
            Attribute attribute = getAttribute(file);
            if (attribute == null) {
                return null;
            } else {
                return attribute.getStringValue();
            }
        }
        return null;
    }

    @Override
    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        if (fieldsSet()) {
            Attribute attribute = getAttribute(file);
            if (attribute == null) {
                return null;
            } else if (attribute.isArray()) {
                return attribute.getStringValue(index);
            } else {
                return attribute.getStringValue();
            }
        }
        return null;
    }

    @Override
    public Number getNumber(NetcdfFile file) throws InvalidDescriptionException {
        if (fieldsSet()) {
            Attribute attribute = getAttribute(file);
            if (attribute == null) {
                return null;
            } else {
                return attribute.getNumericValue();
            }
        }
        return null;
    }

    @Override
    public Number getNumber(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        if (fieldsSet()) {
            Attribute attribute = getAttribute(file);
            if (attribute == null) {
                return null;
            } else if (attribute.isArray()) {
                return attribute.getNumericValue(index);
            } else {
                return attribute.getNumericValue();
            }
        }
        return null;
    }

    @Override
    public long getLength(NetcdfFile file) {
        if (fieldsSet()) {
            Attribute attribute = getAttribute(file);
            if (attribute == null) {
                return 0;
            } else {
                return attribute.getLength();
            }
        }
        return 0;
    }

    @Override
    public boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.name == null) {
            throw new InvalidDescriptionException("name is null");
        }
        Attribute attr = getAttribute(file);
        if (attr == null) {
            throw new InvalidDescriptionException(
                    "Coudld not find a global attribute for " + this.name);
        }
        return attr.getDataType().isNumeric();
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        if (name == null) {
            throw new InvalidDescriptionException("name is null");
        }
        return getAttribute(file) != null;
    }

    /**
     * Gets the described Attribute from the file.
     *
     * @param file
     *            The file.
     * @return the described Attribute from the file, if present.
     */
    protected Attribute getAttribute(NetcdfFile file) {
        return file.findGlobalAttribute(this.name);
    }

    /**
     * @return true if the needed fields to find the Attribute are set, false
     *         otherwise.
     */
    protected boolean fieldsSet() {
        return name != null;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (this.name == null) {
            throw new InvalidDescriptionException("name is not configured.");
        }
    }
}
