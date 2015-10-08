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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import ucar.nc2.NetcdfFile;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * 
 * Contains the information necessary to extract a {@link Date} from a
 * {@link NetcdfFile} where the value is an offset from the configured epoch.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 25, 2015  4699     nabowle   Initial creation
 * Sep 09, 2015  4696     nabowle   Switch TimeUnits. Rename offsetType for 
 *                                  consistency.
 * 
 * </pre>
 * 
 * @author nabowle
 */
@XmlAccessorType(XmlAccessType.NONE)
public class EpochOffsetDateValue extends AbstractDateValue{

    @XmlAttribute(required = true)
    private String epoch;

    @XmlAttribute(required = true)
    private TimeUnit units;

    private transient SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    private transient Calendar epochCal;

    public String getEpoch() {
        return epoch;
    }

    public void setEpoch(String epoch) {
        this.epoch = epoch;
    }

    public TimeUnit getUnits() {
        return units;
    }

    public void setUnits(TimeUnit units) {
        this.units = units;
    }

    @Override
    public synchronized Date getDate(NetcdfFile file)
            throws InvalidDescriptionException {
        Number offsetN = getField().getNumber(file);
        if (offsetN == null) {
            return null;
        }
        if (epochCal == null) {
            Date d;
            try {
                d = sdf.parse(epoch);
            } catch (ParseException e) {
                throw new InvalidDescriptionException(
                        "epoch must be in the format yyyy-MM-dd HH:mm:ss");
            }
            epochCal = TimeUtil.newGmtCalendar(d);
        }

        Calendar cal = (Calendar) epochCal.clone();

        int field;
        switch (this.units) {
        case DAYS:
            field = Calendar.DAY_OF_YEAR;
            break;
        case HOURS:
            field = Calendar.HOUR_OF_DAY;
            break;
        case SECONDS:
            field = Calendar.SECOND;
            break;
        case MILLISECONDS:
            field = Calendar.MILLISECOND;
            break;
        default:
            throw new InvalidDescriptionException("Unsupported offset type: "
                    + units);
        }
        cal.add(field, offsetN.intValue());
        return cal.getTime();
    }
}
