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
package com.raytheon.uf.common.mpe.fieldgen;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Identifies a specific set of MPE precip data. Used for storage and retrieval
 * of data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2017 6407       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */
@DynamicSerialize
public class PrecipDataKey {

    private static final String DATE_FORMAT_PATTERN = "yyyy-MM-dd";

    private static final ThreadLocal<SimpleDateFormat> sdf = TimeUtil
            .buildThreadLocalSimpleDateFormat(DATE_FORMAT_PATTERN,
                    TimeUtil.GMT_TIME_ZONE);

    /*
     * These are not final variables for Dynamic Serialize purposes.
     */
    @DynamicSerializeElement
    private PrecipField field;

    @DynamicSerializeElement
    private Calendar calendar;

    public PrecipDataKey() {
    }

    public PrecipDataKey(final PrecipField field, final Calendar calendar) {
        if (field == null) {
            throw new IllegalArgumentException(
                    "Required argument 'field' cannot be NULL.");
        }
        if (calendar == null) {
            throw new IllegalArgumentException(
                    "Required argument 'calendar' cannot be NULL.");
        }
        this.field = field;
        this.calendar = calendar;
        /*
         * Eliminate all fields not relevant to MPE precipitation.
         */
        TimeUtil.minCalendarFields(this.calendar, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND);
    }

    public PrecipField getField() {
        return field;
    }

    public void setField(PrecipField field) {
        this.field = field;
    }

    public Calendar getCalendar() {
        return calendar;
    }

    public void setCalendar(Calendar calendar) {
        this.calendar = calendar;
    }

    public Date getDate() {
        return calendar.getTime();
    }

    public String getFormattedDate() {
        return sdf.get().format(getDate());
    }

    public int getHour() {
        return calendar.get(Calendar.HOUR_OF_DAY);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("PrecipDataKey [");
        sb.append("field=").append(field.name());
        sb.append(", date=").append(getFormattedDate());
        sb.append(", hour=").append(getHour());
        sb.append("]");

        return sb.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((calendar == null) ? 0 : calendar.hashCode());
        result = prime * result + ((field == null) ? 0 : field.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        PrecipDataKey other = (PrecipDataKey) obj;
        if (calendar == null) {
            if (other.calendar != null) {
                return false;
            }
        } else if (!calendar.equals(other.calendar)) {
            return false;
        }
        if (field != other.field) {
            return false;
        }
        return true;
    }
}