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

import java.io.IOException;
import java.nio.Buffer;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.ma2.Array;
import ucar.ma2.InvalidRangeException;
import ucar.ma2.Section;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.util.BufferUtil;
import com.raytheon.uf.edex.netcdf.decoder.exception.NetcdfDecoderException;
import com.raytheon.uf.edex.netcdf.decoder.util.NetcdfDecoderUtils;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Masks the data based on a discrete integer value.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 05, 2016  5450     nabowle     Initial creation.
 * Apr 19, 2016  5450     nabowle     Reduce memory requirements.
 * Jul 07, 2016  5584     nabowle     Refactor variable name into this class.
 * 
 * </pre>
 * 
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DiscreteDataMaskDescription extends AbstractDataMaskDescription {

    /** Name of the mask variable. */
    @XmlAttribute(required = true)
    private String name;

    /** Integer value of the mask to keep or replace. */
    @XmlAttribute(required = true)
    private Integer value;

    public DiscreteDataMaskDescription() {
        super();
    }

    @Override
    public void applyMask(Buffer rawData, NetcdfFile file,
            Map<String, Integer> recordDimensionIndices)
            throws InvalidDescriptionException {
        Variable var = file.findVariable(getName());
        if (var != null) {
            Section sect;
            Array maskData;
            try {
                sect = NetcdfDecoderUtils.getRecordSection(var,
                        recordDimensionIndices);
                maskData = var.read(sect);
            } catch (NetcdfDecoderException | IOException
                    | InvalidRangeException e) {
                throw new InvalidDescriptionException(
                        "Unable to read the mask variable.", e);
            }
            if (maskData.getSize() != rawData.capacity()) {
                throw new InvalidDescriptionException(
                        "Mask data is not the same length as the raw data.");
            }

            applyMask(rawData, maskData, file);
        }
    }

    /**
     * For each mask value and associated data value, the data value is changed
     * to the configured fill value if
     *
     * <pre>
     *  this.keep is true and mask value != this.value
     *  or
     *  this.keep is false and mask value == this.value
     * </pre>
     */
    private void applyMask(Buffer rawData, Array maskData, NetcdfFile file)
            throws InvalidDescriptionException {
        int maskVal;
        boolean keep = isKeep();
        Number fill = getFill();
        int i = 0;
        while (maskData.hasNext()) {
            maskVal = maskData.nextInt();
            if ((maskVal == this.value) ^ keep) {
                BufferUtil.replaceValue(rawData, i, fill);
            }
            i++;
        }
    }

    public Integer getValue() {
        return value;
    }

    public void setValue(Integer value) {
        this.value = value;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        super.validate();

        if (value == null) {
            throw new InvalidDescriptionException("mask value is required.");
        }

        if (name == null || name.trim().isEmpty()) {
            throw new InvalidDescriptionException(
                    "mask variable name is required.");
        }
    }

    @Override
    public boolean isPresent(NetcdfFile file) {
        return file.findVariable(name) != null;
    }
}
