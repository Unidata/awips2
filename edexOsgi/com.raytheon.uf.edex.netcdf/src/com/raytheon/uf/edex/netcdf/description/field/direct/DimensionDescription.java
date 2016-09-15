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

import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.AbstractFieldDescription;

/**
 * Contains the name of a Dimension and allows retrieval of its value.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 9, 2016  5584       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DimensionDescription extends AbstractFieldDescription {

    /**
     * Constructor.
     */
    public DimensionDescription() {
        super();
    }

    @Override
    public String getString(NetcdfFile file) throws InvalidDescriptionException {
        Number num = getNumber(file);
        if (num == null) {
            return null;
        }
        return num.toString();
    }

    @Override
    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        Number num = getNumber(file, index);
        if (num == null) {
            return null;
        }
        return num.toString();
    }

    @Override
    public Number getNumber(NetcdfFile file) throws InvalidDescriptionException {
        if (this.name == null) {
            return null;
        }

        Dimension dim = file.findDimension(this.name);
        if (dim == null) {
            return null;
        }

        return Integer.valueOf(dim.getLength());
    }

    @Override
    public Number getNumber(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        return getNumber(file);
    }

    @Override
    public long getLength(NetcdfFile file) {
        try {
            if (isPresent(file)) {
                return 1L;
            }
        } catch (InvalidDescriptionException e) {
            // ignore
        }
        return 0L;
    }

    @Override
    public boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException {
        if (!isPresent(file)) {
            throw new InvalidDescriptionException(
                    "the configured dimension is not present");
        }
        return true;
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        return getNumber(file) != null;
    }

    /**
     * The length of variable length dimension cannot be known until the data is
     * read.
     *
     * @param file
     *            The netcdf file.
     * @return True if the dimension is variable length.
     * @throws InvalidDescriptionException
     *             if the name isn't configured or the dimension is not present.
     */
    public boolean isVariableLength(NetcdfFile file) throws InvalidDescriptionException {
        if (this.name == null) {
            throw new InvalidDescriptionException("name is not configured.");
        }

        Dimension dim = file.findDimension(this.name);
        if (dim == null) {
            throw new InvalidDescriptionException(
                    "the configured dimension is not present");
        }

        return dim.isVariableLength();
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (this.name == null) {
            throw new InvalidDescriptionException("name is not configured.");
        }
    }
}
