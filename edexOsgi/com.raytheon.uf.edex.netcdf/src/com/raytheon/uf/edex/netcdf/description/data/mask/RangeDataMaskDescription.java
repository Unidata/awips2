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
package com.raytheon.uf.edex.netcdf.description.data.mask;

import java.nio.Buffer;
import java.util.Map;

import javax.xml.bind.annotation.XmlElement;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.util.BufferUtil;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;

/**
 * Masks data based on whether the values fall within a range.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 07, 2016 5584       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
public class RangeDataMaskDescription extends AbstractDataMaskDescription {

    @XmlElement
    private DelegateFieldDescription range;

    @XmlElement
    private DelegateFieldDescription lower;

    @XmlElement
    private DelegateFieldDescription upper;

    /**
     * Constructor.
     */
    public RangeDataMaskDescription() {
        super();
    }

    @Override
    public void applyMask(Buffer rawData, NetcdfFile file,
            Map<String, Integer> recordDimensionIndices)
            throws InvalidDescriptionException {
        double low = Double.NEGATIVE_INFINITY;
        double high = Double.POSITIVE_INFINITY;
        if (this.range != null) {
            if (this.range.isNumeric(file)) {
                low = this.range.getNumber(file, 0).doubleValue();
                high = this.range.getNumber(file, 1).doubleValue();
            } else {
                String val = this.range.getString(file);
                String[] vals = val.split(",");
                if (vals.length != 2) {
                    throw new InvalidDescriptionException(
                            "Unable to determine the range from " + val);
                }
                try {
                    low = Double.parseDouble(vals[0]);
                    high = Double.parseDouble(vals[1]);
                } catch (NumberFormatException e) {
                    throw new InvalidDescriptionException(
                            "Unable to determine the range from " + val);
                }
            }
        } else {
            if (this.lower != null) {
                if (this.lower.isNumeric(file)) {
                    low = this.lower.getNumber(file).doubleValue();
                } else {
                    String val = this.lower.getString(file);
                    try {
                        low = Double.parseDouble(val);
                    } catch (NumberFormatException e) {
                        throw new InvalidDescriptionException(
                                "Unable to determine the lower range value from "
                                        + val);
                    }
                }
            }
            if (this.upper != null) {
                if (this.upper.isNumeric(file)) {
                    high = this.upper.getNumber(file).doubleValue();
                } else {
                    String val = this.upper.getString(file);
                    try {
                        high = Double.parseDouble(val);
                    } catch (NumberFormatException e) {
                        throw new InvalidDescriptionException(
                                "Unable to determine the upper range value from "
                                        + val);
                    }
                }
            }
        }

        if (low > high) {
            double temp = low;
            low = high;
            high = temp;
        }

        boolean keep = isKeep();
        Number fill = getFill();
        rawData.rewind();
        for (int i = 0; i < rawData.limit(); i++) {
            double dataVal = BufferUtil.getNumber(rawData, i).doubleValue();
            boolean inRange = dataVal >= low && dataVal <= high;
            if (inRange != keep) {
                BufferUtil.replaceValue(rawData, i, fill);
            }
        }
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.range != null) {
            return this.range.isPresent(file);
        }
        if (this.lower != null) {
            if (this.upper != null) {
                return this.lower.isPresent(file) && this.upper.isPresent(file);
            } else {
                return this.lower.isPresent(file);
            }
        } else if (this.upper != null) {
            return this.upper.isPresent(file);
        }
        return false;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        super.validate();

        if (this.range == null && this.lower == null && this.upper == null) {
            throw new InvalidDescriptionException(
                    "range, lower, or upper must be configured");
        }

        if (this.range != null) {
            this.range.validate();
            if (this.lower != null || this.upper != null) {
                throw new InvalidDescriptionException(
                        "lower and upper cannot be configured if range is configured");
            }
        }

        if (this.lower != null) {
            this.lower.validate();
        }

        if (this.upper != null) {
            this.upper.validate();
        }
    }
}
