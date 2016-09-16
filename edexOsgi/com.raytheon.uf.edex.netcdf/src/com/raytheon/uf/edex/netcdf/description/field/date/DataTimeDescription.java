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

import java.util.Calendar;
import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.netcdf.description.field.direct.DimensionDescription;

/**
 *
 * Contains the information necessary to extract a {@link DataTime} from the
 * global attributes of a {@link NetcdfFile}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * Aug 25, 2015  4699     nabowle   Extracted from Pointset netcdf plugin and
 *                                  refactored.
 * Jan 25, 2016  5208     bsteffen  Add validation.
 * Apr 19, 2016  5450     nabowle   Add multi-date retrieval.
 * Apr 27, 2016  5450     nabowle   Add validTime handling.
 * Jun 07, 2016  5452     bsteffen  Throw exception if valid time is before
 * Jun 09, 2016  5584     nabowle   Updates for consolidation.
 *                                  reftime
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DataTimeDescription {

    public static final String DATATIME_KEY = "dataTime";

    @XmlElements({
            @XmlElement(name = "formattedRefTime", type = FormattedDateValue.class),
            @XmlElement(name = "epochOffsetRefTime", type = EpochOffsetDateValue.class) })
    private AbstractDateValue refTime;

    @XmlElement
    private ForecastDescription forecast;

    @XmlElements({
            @XmlElement(name = "formattedValidTime", type = FormattedDateValue.class),
            @XmlElement(name = "epochOffsetValidTime", type = EpochOffsetDateValue.class) })
    private AbstractDateValue validTime;

    @XmlElement
    private DimensionDescription dimension;

    public AbstractDateValue getRefTime() {
        return refTime;
    }

    public void setRefTime(AbstractDateValue refTime) {
        this.refTime = refTime;
    }

    public ForecastDescription getForecast() {
        return forecast;
    }

    public void setForecast(ForecastDescription forecast) {
        this.forecast = forecast;
    }

    /**
     * @return the validTime
     */
    public AbstractDateValue getValidTime() {
        return validTime;
    }

    /**
     * @param valid
     *            the validTime to set
     */
    public void setValidTime(AbstractDateValue valid) {
        this.validTime = valid;
    }

    /**
     * @return the dimension
     */
    public DimensionDescription getDimension() {
        return dimension;
    }

    /**
     * @param dimension
     *            the dimension to set
     */
    public void setDimension(DimensionDescription dimension) {
        this.dimension = dimension;
    }

    public DataTime getDataTime(NetcdfFile file)
            throws InvalidDescriptionException {
        return getDataTime(file, 0);
    }

    public DataTime getDataTime(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        if (this.refTime != null) {
            Date refTime = this.refTime.getDate(file, index);
            if (refTime == null) {
                return null;
            }

            if (this.forecast != null) {
                return new DataTime(refTime, this.forecast.getForecast(file,
                        index));
            } else if (this.validTime != null) {
                Date validTime = this.validTime.getDate(file, index);
                if (validTime == null) {
                    throw new InvalidDescriptionException(
                            "Unable to retrieve the valid time.");
                }
                long diffMs = validTime.getTime() - refTime.getTime();
                int forecast = (int) (diffMs / 1000);
                if (forecast < 0) {
                    throw new InvalidDescriptionException("Valid time("
                            + TimeUtil.formatDate(validTime)
                            + ") must be after reference time("
                            + TimeUtil.formatDate(refTime) + ")");
                }
                return new DataTime(refTime, forecast);
            } else {
                return new DataTime(refTime);
            }

        } else if (this.forecast != null && this.validTime != null) {
            // calculate reftime from forecast and validTime
            int forecast = this.forecast.getForecast(file, index);
            Date validTime = this.validTime.getDate(file, index);
            if (validTime == null) {
                throw new InvalidDescriptionException(
                        "Unable to retrieve the valid time.");
            }
            Calendar refCal = TimeUtil.newGmtCalendar(validTime);
            refCal.add(Calendar.SECOND, -forecast);
            return new DataTime(refCal.getTime(), forecast);
        }
        return null;
    }

    public void validate() throws InvalidDescriptionException {
        if (refTime == null) {
            if (validTime == null || this.forecast == null) {
                throw new InvalidDescriptionException(
                        "reftime is not configured, so both validTime and forecast must be configured");
            }
        } else {
            try {
                refTime.validate();
            } catch (InvalidDescriptionException e) {
                throw new InvalidDescriptionException("Invalid ref time: "
                        + e.getMessage(), e);
            }

            if (validTime != null && this.forecast != null) {
                throw new InvalidDescriptionException(
                        "reftime, forecast, and validTime cannot all be configured.");
            }
        }

        if (this.forecast != null) {
            this.forecast.validate();
        }

        if (this.validTime != null) {
            this.validTime.validate();
        }
    }

}
