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
package com.raytheon.uf.edex.netcdf.description.date;

import java.util.concurrent.TimeUnit;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.edex.netcdf.description.AbstractFieldDescription;
import com.raytheon.uf.edex.netcdf.description.AttributeDescription;
import com.raytheon.uf.edex.netcdf.description.ValueDescription;
import com.raytheon.uf.edex.netcdf.description.VariableDescription;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

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
 * 
 * </pre>
 * 
 * @author nabowle
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ForecastDescription {

    @XmlElements({ @XmlElement(name = "value", type = ValueDescription.class),
            @XmlElement(name = "variable", type = VariableDescription.class),
            @XmlElement(name = "attribute", type = AttributeDescription.class) })
    private AbstractFieldDescription forecastField;

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
        int forecast = this.forecastField.getNumber(file).intValue();
        forecast = Long.valueOf(this.units.toSeconds(forecast)).intValue();
        return forecast;
    }

    /**
     * @return the forecast
     */
    public AbstractFieldDescription getForecastField() {
        return forecastField;
    }

    /**
     * @param forecast
     *            the forecast to set
     */
    public void setForecastField(AbstractFieldDescription forecastField) {
        this.forecastField = forecastField;
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

}
