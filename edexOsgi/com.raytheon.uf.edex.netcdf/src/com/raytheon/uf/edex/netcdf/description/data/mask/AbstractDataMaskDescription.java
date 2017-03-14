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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlSeeAlso;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Base class for describing a data mask.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 07, 2016 5450       nabowle     Initial creation
 * Apr 19, 2016 5450       nabowle     Reduce memory requirements.
 * Jul 07, 2016 5584       nabowle     refactor for range mask.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso({ DiscreteDataMaskDescription.class })
public abstract class AbstractDataMaskDescription {

    /** The fill value to apply when necessary. */
    @XmlElements({ @XmlElement(name = "fillDouble", type = Double.class),
            @XmlElement(name = "fillFloat", type = Float.class),
            @XmlElement(name = "fillLong", type = Long.class),
            @XmlElement(name = "fillInt", type = Integer.class),
            @XmlElement(name = "fillShort", type = Short.class),
            @XmlElement(name = "fillByte", type = Byte.class) })
    private Number fill;

    /**
     * If true, mask values that match the description will cause the associated
     * data to be kept, and unmatched mask values will cause the data to be
     * replaced with {@link #fill}.
     *
     * If false, mask values that match the description will cause the
     * associated data to be replaced with fill, and unmatched mask values will
     * cause the data to be retained.
     */
    @XmlAttribute
    private boolean keep;

    public AbstractDataMaskDescription() {
        super();
    }

    public boolean isKeep() {
        return keep;
    }

    public void setKeep(boolean keep) {
        this.keep = keep;
    }

    public Number getFill() {
        return fill;
    }

    public void setFill(Number fill) {
        this.fill = fill;
    }

    public void validate() throws InvalidDescriptionException {
        if (fill == null) {
            throw new InvalidDescriptionException("fill value is required.");
        }
    }

    /**
     * Applies a mask to the rawData based on this configured description.
     *
     * Note: The rawData Buffer is modified by this method.
     *
     * @param rawData
     *            The raw data to mask.
     * @param maskData
     *            The mask data.
     * @param file
     *            The netcdf file.
     * @param recordDimensionIndices
     *            The dimension indices that defined the Section for the raw
     *            data.
     * @throws InvalidDescriptionException
     *             if the mask is incorrectly configured.
     */
    public abstract void applyMask(Buffer rawData, NetcdfFile file,
            Map<String, Integer> recordDimensionIndices)
            throws InvalidDescriptionException;

    public abstract boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException;
}
