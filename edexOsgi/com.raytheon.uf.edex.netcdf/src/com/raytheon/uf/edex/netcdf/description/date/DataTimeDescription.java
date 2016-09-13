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

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

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
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DataTimeDescription {

    @XmlElements({
            @XmlElement(name = "formattedRefTime", type = FormattedDateValue.class),
            @XmlElement(name = "epochOffsetRefTime", type = EpochOffsetDateValue.class) })
    private AbstractDateValue refTime;

    @XmlElement
    private ForecastDescription forecast;

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

    public DataTime getDataTime(NetcdfFile file)
            throws InvalidDescriptionException {
        Date refTime = this.refTime.getDate(file);
        if (refTime == null) {
            return null;
        } else {
            if (this.forecast != null) {
                return new DataTime(refTime, this.forecast.getForecast(file));
            } else {
                return new DataTime(refTime);
            }
        }
    }

    public void validate() throws InvalidDescriptionException {
        if (refTime == null) {
            throw new InvalidDescriptionException(
                    "A ref time element is not present.");
        }
        try {
            refTime.validate();
        } catch (InvalidDescriptionException e) {
            throw new InvalidDescriptionException("Invalid ref time: "
                    + e.getMessage(), e);
        }
    }

}
