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
package com.raytheon.uf.edex.netcdf.description;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 *
 * Specifies a fixed value in the XML. Normally the XML would specify an
 * attribute or variable to read from the netCDF file but in some cases the
 * netcdf file does not have enough information so values can be directly in the
 * xml using this class.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 26, 2015  4699     nabowle   Initial creation
 * Sep 09, 2015  4696     nabowle   Add indexed retrieval and getLength().
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ValueDescription extends AbstractFieldDescription {

    @XmlAttribute(required = true)
    private String value;

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getString(NetcdfFile file)
            throws InvalidDescriptionException {
        if (value != null) {
            return value;
        }
        return null;
    }

    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        return getString(file);
    }

    public Number getNumber(NetcdfFile file)
            throws InvalidDescriptionException {
        if (value != null) {
            return Double.parseDouble(value);
        }
        return null;
    }

    public Number getNumber(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        return getNumber(file);
    }

    @Override
    public long getLength(NetcdfFile file) {
        return value == null ? 0 : 1;
    }
}
