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
 * Apr 19, 2016  5450     nabowle   Add multi-date retrieval.
 * Apr 26, 2016  5450     nabowle   Fix floating-point date offsets.
 * May  5, 2016  5451     jschmid   Add roundTo.
 * May 19, 2016  5584     nabowle   Updates for consolidation.
 *
 * </pre>
 *
 * @author nabowle
 */
@XmlAccessorType(XmlAccessType.NONE)
public class EpochOffsetDateValue extends AbstractDateValue {

    @XmlAttribute(required = true)
    private String epoch;

    @XmlAttribute(required = true)
    private TimeUnit units;

    /**
     * Needed for rounding off inaccuracies when the time is stored inexactly in
     * a floating point format. Can be one of {DAYS, HOURS, SECONDS,
     * MILLISECONDS}.
     */
    @XmlAttribute()
    private TimeUnit roundTo;

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
    public synchronized Date getDate(NetcdfFile file, int index)
            throws InvalidDescriptionException {
        Number offsetN = this.getDelegate().getNumber(file, index);
        if (offsetN == null) {
            return null;
        }

        boolean isFloatingPoint = offsetN instanceof Double
                || offsetN instanceof Float;

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

        double dVal = offsetN.doubleValue();
        int iVal = (int) dVal;
        boolean isPositive = dVal >= 0;
        switch (this.units) {
        case DAYS:
            cal.add(Calendar.DAY_OF_YEAR, iVal);
            dVal -= iVal;
            if (canStop(isFloatingPoint, dVal, isPositive)) {
                break;
            }
            dVal *= TimeUtil.HOURS_PER_DAY;
            iVal = (int) dVal;
        case HOURS:
            cal.add(Calendar.HOUR_OF_DAY, iVal);
            dVal -= iVal;
            if (canStop(isFloatingPoint, dVal, isPositive)) {
                break;
            }
            dVal *= TimeUtil.SECONDS_PER_HOUR;
            iVal = (int) dVal;
        case SECONDS:
            cal.add(Calendar.SECOND, iVal);
            dVal -= iVal;
            if (canStop(isFloatingPoint, dVal, isPositive)) {
                break;
            }
            dVal *= TimeUtil.MILLIS_PER_SECOND;
            iVal = (int) dVal;
        case MILLISECONDS:
            cal.add(Calendar.MILLISECOND, iVal);
            break;
        default:
            throw new InvalidDescriptionException("Unsupported offset type: "
                    + units);
        }

        if (null != roundTo) {
            switch (roundTo) {
            case DAYS:
                if (cal.get(Calendar.HOUR_OF_DAY) >= 12) {
                    cal.add(Calendar.DAY_OF_YEAR, 1);
                }
                cal.set(Calendar.HOUR_OF_DAY, 0);
                cal.set(Calendar.MINUTE, 0);
                cal.set(Calendar.SECOND, 0);
                cal.set(Calendar.MILLISECOND, 0);
                break;

            case HOURS:
                if (cal.get(Calendar.MINUTE) >= 30) {
                    cal.add(Calendar.HOUR_OF_DAY, 1);
                }
                cal.set(Calendar.MINUTE, 0);
                cal.set(Calendar.SECOND, 0);
                cal.set(Calendar.MILLISECOND, 0);
                break;

            case MINUTES:
                if (cal.get(Calendar.SECOND) >= 30) {
                    cal.add(Calendar.MINUTE, 1);
                }
                cal.set(Calendar.SECOND, 0);
                cal.set(Calendar.MILLISECOND, 0);
                break;

            case SECONDS:
                if (cal.get(Calendar.MILLISECOND) >= 500) {
                    cal.add(Calendar.SECOND, 1);
                }
                cal.set(Calendar.MILLISECOND, 0);
                break;

            default:
                throw new InvalidDescriptionException(
                        "Unsupported roundTo value: " + roundTo.toString());
            }
        }

        return cal.getTime();
    }

    /**
     * Indicates if processing can stop.
     *
     * @param isFloatingPoint
     *            Indicates if the original number is a floating-point number.
     * @param val
     *            The current value.
     * @param isPositive
     *            If the value was original positive.
     * @return True if processing can stop. false if processing should continue.
     */
    private boolean canStop(boolean isFloatingPoint, double val,
            boolean isPositive) {
        return !isFloatingPoint || val == 0.0D || signChanged(val, isPositive);
    }

    /**
     * Determine if the sign has changed.
     *
     * @param val
     *            The current value.
     * @param isPositive
     *            Indicates if the value was originally positive.
     * @return True if the sign changed, false otherwise.
     */
    private boolean signChanged(double val, boolean isPositive) {
        return (isPositive && val < 0.0D) || (!isPositive && val > 0.0D);
    }
}
