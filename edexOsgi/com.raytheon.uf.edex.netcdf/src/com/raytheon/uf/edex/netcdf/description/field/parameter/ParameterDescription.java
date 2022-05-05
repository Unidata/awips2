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
package com.raytheon.uf.edex.netcdf.description.field.parameter;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.indirect.DelegateFieldDescription;

/**
 * Description of a Parameter.
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
public class ParameterDescription {

    public static final String PARAMETER_KEY = "parameter";

    @XmlElement
    private DelegateFieldDescription abbreviation;

    @XmlElement
    private DelegateFieldDescription name;

    @XmlElement
    private DelegateFieldDescription units;

    /**
     * Constructor.
     */
    public ParameterDescription() {
        super();
    }

    /**
     * Get the described Parameter.
     *
     * @param file
     *            The NetcdfFile to retrieve values from.
     * @return The created Parameter.
     * @throws InvalidDescriptionException
     *             if this description is invalid.
     */
    public Parameter getParameter(NetcdfFile file)
            throws InvalidDescriptionException {
        Parameter param = new Parameter();
        if (this.abbreviation != null) {
            param.setAbbreviation(this.abbreviation.getString(file));
        }
        if (this.name != null) {
            param.setName(this.name.getString(file));
        }
        if (this.units != null) {
            param.setUnitString(this.units.getString(file));
        }
        return param;
    }

    /**
     * Get the described Parameter.
     *
     * @param file
     *            The NetcdfFile to retrieve values from.
     * @param index
     *            The index into the fields to use when retrieving values.
     * @return The created Parameter.
     * @throws InvalidDescriptionException
     *             if this description is invalid.
     */
    public Parameter getParameter(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        Parameter param = new Parameter();
        if (this.abbreviation != null) {
            param.setAbbreviation(this.abbreviation.getString(file, index));
        }
        if (this.name != null) {
            param.setName(this.name.getString(file, index));
        }
        if (this.units != null) {
            param.setUnitString(this.units.getString(file, index));
        }
        return param;
    }

    /**
     * @return the abbreviation
     */
    public DelegateFieldDescription getAbbreviation() {
        return abbreviation;
    }

    /**
     * @param abbreviation
     *            the abbreviation to set
     */
    public void setAbbreviation(DelegateFieldDescription abbreviation) {
        this.abbreviation = abbreviation;
    }

    /**
     * @return the name
     */
    public DelegateFieldDescription getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(DelegateFieldDescription name) {
        this.name = name;
    }

    /**
     * @return the units
     */
    public DelegateFieldDescription getUnits() {
        return units;
    }

    /**
     * @param units
     *            the units to set
     */
    public void setUnits(DelegateFieldDescription units) {
        this.units = units;
    }

    /**
     * Validate this description.
     *
     * @throws InvalidDescriptionException
     *             If this description is invalid.
     */
    public void validate() throws InvalidDescriptionException {
        if (abbreviation != null) {
            try {
                this.abbreviation.validate();
            } catch (InvalidDescriptionException e) {
                throw new InvalidDescriptionException(
                        "abbreviation is invalid.", e);
            }
        }
        if (name != null) {
            try {
                this.name.validate();
            } catch (InvalidDescriptionException e) {
                throw new InvalidDescriptionException("name is invalid.", e);
            }
        }
        if (units != null) {
            try {
                this.units.validate();
            } catch (InvalidDescriptionException e) {
                throw new InvalidDescriptionException("units is invalid.", e);
            }
        }

        if (this.abbreviation == null && this.name == null
                && this.units == null) {
            throw new InvalidDescriptionException(
                    "no parameter information is configured.");
        }
    }
}
