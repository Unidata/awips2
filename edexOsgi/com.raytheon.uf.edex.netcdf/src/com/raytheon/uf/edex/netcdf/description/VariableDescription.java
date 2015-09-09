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

import java.io.IOException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Contains the name of a variable in a {@link NetcdfFile} that holds needed
 * values, and provides means to extract scalar values from the variable.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * Aug 25, 2015  4699     nabowle   Extracted from Pointset netcdf plugin and
 *                                  refactored.
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class VariableDescription extends AbstractFieldDescription {

    @Override
    public String getString(NetcdfFile file)
            throws InvalidDescriptionException {
        if (name != null) {
            Variable var = file.findVariable(name);
            if (var == null) {
                return null;
            } else {
                try {
                    return var.readScalarString();
                } catch (IOException e) {
                    throw new InvalidDescriptionException(
                            "Cannot read a scalar string from " + name, e);
                }
            }
        }
        return null;
    }

    @Override
    public Number getNumber(NetcdfFile file)
            throws InvalidDescriptionException {
        if (name != null) {
            Variable var = file.findVariable(name);
            if (var == null) {
                return null;
            } else {
                try {
                    switch (var.getDataType()) {
                    case BYTE:
                        return var.readScalarByte();
                    case SHORT:
                        return var.readScalarShort();
                    case INT:
                        return var.readScalarInt();
                    case LONG:
                        return var.readScalarLong();
                    case FLOAT:
                        return var.readScalarFloat();
                    case DOUBLE:
                        return var.readScalarDouble();
                    default:
                        throw new InvalidDescriptionException(
                                "Cannot read a scalar number from " + name
                                        + " with of data type "
                                        + var.getDataType());
                    }
                } catch (IOException e) {
                    throw new InvalidDescriptionException(
                            "Error reading a scalar number from " + name, e);
                }
            }
        }
        return null;
    }

}
