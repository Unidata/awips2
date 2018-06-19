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

import java.io.IOException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import ucar.ma2.Array;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.AbstractFieldDescription;

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
 * Sep 09, 2015  4696     nabowle   Add indexed retrieval and getLength().
 * Dec 08, 2015  5059     nabowle   Add isNumeric() and isPresent().
 * May 19, 2016  5584     nabowle   Updates for consolidation.
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class VariableDescription extends AbstractFieldDescription {

    public VariableDescription() {
        super();
    }


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
    public String getString(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        if (name != null) {
            Variable var = file.findVariable(name);
            if (var == null) {
                return null;
            }

            if (var.getSize() == 1) {
                index = 0;
            }

            try {
                return var.read().getObject(index).toString();
            } catch (IOException e) {
                throw new InvalidDescriptionException(
                        "Cannot read a string at index " + index + " from "
                                + name, e);
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
                                        + " of data type " + var.getDataType());
                    }
                } catch (IOException e) {
                    throw new InvalidDescriptionException(
                            "Error reading a scalar number from " + name, e);
                }
            }
        }
        return null;
    }

    @Override
    public Number getNumber(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        if (name != null) {
            Variable var = file.findVariable(name);
            if (var == null) {
                return null;
            }
            if (var.getSize() == 1) {
                index = 0;
            }
            try {
                Array array = var.read();
                switch (var.getDataType()) {
                case BYTE:
                    return array.getByte(index);
                case SHORT:
                    return array.getShort(index);
                case INT:
                    return array.getInt(index);
                case LONG:
                    return array.getLong(index);
                case FLOAT:
                    return array.getFloat(index);
                case DOUBLE:
                    return array.getDouble(index);
                default:
                    throw new InvalidDescriptionException(
                            "Cannot read a number at index " + index + " from "
                                    + name + " of data type "
                                    + var.getDataType());
                }
            } catch (IOException e) {
                throw new InvalidDescriptionException(
                        "Error reading a number at index " + index + " from "
                                + name, e);
            }
        }
        return null;
    }

    @Override
    public long getLength(NetcdfFile file) {
        if (name != null) {
            Variable var = file.findVariable(name);
            if (var == null) {
                return 0;
            }
            return var.getSize();
        }
        return 0;
    }

    @Override
    public boolean isNumeric(NetcdfFile file)
            throws InvalidDescriptionException {
        if (this.name == null) {
            throw new InvalidDescriptionException("name is null");
        }
        Variable var = file.findVariable(this.name);
        if (var == null) {
            throw new InvalidDescriptionException(
                    "Coudld not find a variable for " + this.name);
        }
        return var.getDataType().isNumeric();
    }

    @Override
    public boolean isPresent(NetcdfFile file)
            throws InvalidDescriptionException {
        if (name == null) {
            throw new InvalidDescriptionException("name is null");
        }
        return file.findVariable(this.name) != null;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (this.name == null) {
            throw new InvalidDescriptionException("name is not configured.");
        }
    }

}
