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
package com.raytheon.uf.edex.netcdf.description.field.date;

import java.util.concurrent.TimeUnit;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.DelegateDescription;

/**
 * Describes the forecast time of a DataTime.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2015  4469       nabowle     Initial creation
 * Sep 9, 2015  4469       nabowle     Switch TimeUnits.
 * Apr 19, 2016 5450       nabowle     Add multi-date retrieval.
 * Apr 26, 2016 5450       nabowle     Add pattern and validate.
 * May 19, 2016 5584       nabowle     Updates for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ForecastDescription extends DelegateDescription {

    @XmlAttribute(required = true)
    private TimeUnit units;

    /**
     * Constructor.
     */
    public ForecastDescription() {
        super();
    }

    /**
     * Get the forecast from a file.
     *
     * @param file
     *            The netcdf file to get the forecast from.
     * @return The forecast, in seconds.
     * @throws InvalidDescriptionException
     */
    public int getForecast(NetcdfFile file) throws InvalidDescriptionException {
        Number val = this.getDelegate().getNumber(file);
        if (val == null) {
            throw new InvalidDescriptionException("cannot get the forecast");
        }
        return Long.valueOf(this.units.toSeconds(val.intValue())).intValue();

    }

    /**
     * Get the forecast from a file.
     *
     * @param file
     *            The netcdf file to get the forecast from.
     * @param index
     *            The index of the value to read in the data.
     * @return The forecast, in seconds.
     * @throws InvalidDescriptionException
     */
    public int getForecast(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        Number val = this.getDelegate().getNumber(file, index);
        if (val == null) {
            throw new InvalidDescriptionException("cannot get the forecast");
        }
        return Long.valueOf(this.units.toSeconds(val.intValue())).intValue();
    }

    /**
     * @return the units
     */
    public TimeUnit getUnits() {
        return units;
    }

    /**
     * @param units
     *            the units to set
     */
    public void setUnits(TimeUnit units) {
        this.units = units;
    }

    @Override
    public void validate() throws InvalidDescriptionException {
        if (this.units == null) {
            throw new InvalidDescriptionException("units must be configured.");
        }

        super.validate();
    }
}
