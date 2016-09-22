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

import java.util.Arrays;
import java.util.IllegalFormatException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.DelegatesDescription;
import com.raytheon.uf.edex.netcdf.description.field.IFieldDescription;

/**
 * Applies a Format string to one or more Fields. If the field isn't present in
 * the NetCDF file, the value from a record's attributes will be used instead if
 * present.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 24, 2015 5059       nabowle     Initial creation
 * May 19, 2016 5584       nabowle     Updates for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FormattedFieldsDescription extends DelegatesDescription implements
        IFieldDescription {

    @XmlAttribute
    private String format;

    public String getName() {
        if (this.delegates != null
                && !this.delegates.isEmpty()) {
            return this.delegates.get(0).getName();
        }
        return null;
    }

    /**
     * Constructor.
     */
    public FormattedFieldsDescription() {
        super();
    }

    /**
     * Gets the formatValue for the field. If the field is present in the netcdf
     * file, the value from the file is returned. If the field isn't present but
     * a record is given, the value from the record's data attributes is
     * returned. Otherwise, null is returned.
     */
    protected Object getFormatValue(NetcdfFile file, IFieldDescription field,
            int index) throws InvalidDescriptionException {
        Object ret = null;

        if (field.isPresent(file)) {
            if (field.isNumeric(file)) {
                ret = field.getNumber(file, index);
            } else {
                ret = field.getString(file, index);
            }
        }
        return ret;
    }

    public String getString(NetcdfFile file) throws InvalidDescriptionException {
        return getString(file, 0);
    }

    @Override
    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        Object[] formatValues = new Object[this.delegates.size()];
        Object value;
        for (int i = 0; i < formatValues.length; i += 1) {
            value = getFormatValue(file, this.delegates.get(i), index);
            if (value == null) {
                return null;
            }
            formatValues[i] = value;
        }
        try {
            return String.format(format, formatValues);
        } catch (IllegalFormatException | NullPointerException e) {
            throw new InvalidDescriptionException(
                    "Error formatting the values.", e);
        }
    }

    @Override
    public Number getNumber(NetcdfFile file)
            throws InvalidDescriptionException {
        return getNumber(file, 0);
    }

    @Override
    public Number getNumber(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        String stringVal = getString(file, index);
        if (stringVal == null || stringVal.trim().isEmpty()) {
            return null;
        }
        try {
            return Double.parseDouble(stringVal);
        } catch (NumberFormatException e) {
            throw new InvalidDescriptionException(e);
        }
    }

    @Override
    public long getLength(NetcdfFile file) throws InvalidDescriptionException {
        long size = 1;
        long newSize;
        for (IFieldDescription delegate : this.delegates) {
            newSize = delegate.getLength(file);
            if (newSize != 1) {
                if (size == 1) {
                    size = newSize;
                } else if (size != newSize) {
                    throw new InvalidDescriptionException(
                            "the delegate lengths "
                                    + Arrays.toString(new long[] { size,
                                            newSize })
                                    + " are incompatible");
                }
            }
        }
        return size;
    }

    @Override
    public boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException {
        return false;
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.delegates == null || this.delegates.isEmpty()) {
            throw new InvalidDescriptionException("No fields are configured.");
        }
        for (IFieldDescription field : this.delegates) {
            if (!field.isPresent(file)) {
                return false;
            }
        }
        return true;
    }

    /**
     * @return the format
     */
    public String getFormat() {
        return format;
    }

    /**
     * @param format
     *            the format to set
     */
    public void setFormat(String format) {
        this.format = format;
    }
}
