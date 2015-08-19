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

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

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
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AttributeDescription extends AbstractFieldDescription {

    @Override
    public String getString(NetcdfFile file)
            throws InvalidDescriptionException {
        if (name != null) {
            Attribute attribute = file.findGlobalAttribute(this.name);
            if (attribute == null) {
                return null;
            } else {
                return attribute.getStringValue();
            }
        }
        return null;
    }

    @Override
    public Number getNumber(NetcdfFile file)
            throws InvalidDescriptionException {
        if (name != null) {
            Attribute attribute = file.findGlobalAttribute(this.name);
            if (attribute == null) {
                return null;
            } else {
                return attribute.getNumericValue();
            }
        }
        return null;
    }

}
