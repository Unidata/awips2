package com.raytheon.uf.common.time;

import java.util.Calendar;
import java.util.Iterator;
import java.util.TimeZone;

import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlTransient;

/**
 * A single DataTime object representing 2 DataTimes, useful for products which
 * are a combination of other products with potentially different times.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 10, 2011           rgeorge     Initial creation
 * Aug 08, 2013  2245     bsteffen    Make all DataTime comparisons consistent.
 * Oct 28, 2013  2491     bsteffen    Add @XmlTransient
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
@XmlTransient
public class CombinedDataTime extends DataTime {

    private static final long serialVersionUID = 1L;

    @Transient
    private DataTime primaryDataTime;

    @Transient
    private DataTime secondaryDataTime;

    /**
     * @param dataTime
     */

    public CombinedDataTime(DataTime primaryDataTime, DataTime secondaryDataTime) {
        super();
        if (secondaryDataTime instanceof CombinedDataTime
                || primaryDataTime == secondaryDataTime) {
            return;
        }
        this.primaryDataTime = primaryDataTime;
        this.fcstTime = primaryDataTime.fcstTime;
        this.levelValue = primaryDataTime.levelValue;
        this.refTime = primaryDataTime.refTime;
        this.utilityFlags = primaryDataTime.utilityFlags;
        this.validPeriod = new TimeRange(this.refTime,
                secondaryDataTime.refTime);
        this.visible = primaryDataTime.visible;
        if (secondaryDataTime != null) {
            this.secondaryDataTime = secondaryDataTime.clone();
        }
    }

    public DataTime getPrimaryDataTime() {
        return primaryDataTime;
    }

    /**
     * @return
     */
    public DataTime getAdditionalDataTime() {
        return secondaryDataTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.time.DataTime#isVisible()
     */
    public boolean isVisible() {
        return (getAdditionalDataTime() != null) && visible;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime
                * result
                + ((secondaryDataTime == null) ? 0 : secondaryDataTime
                        .hashCode());
        result = prime * result + fcstTime;
        result = prime * result
                + ((levelValue == null) ? 0 : levelValue.hashCode());
        if (utilityFlags == null) {
            result = prime * result + 0;
        } else {
            int h = 0;
            Iterator<FLAG> i = utilityFlags.iterator();
            while (i.hasNext()) {
                FLAG obj = i.next();
                if (obj != null)
                    h += obj.ordinal();
            }
            result = prime * result + h;
        }

        result = (int) (prime * result + ((validPeriod == null) ? 0
                : validPeriod.getStart().getTime()
                        + validPeriod.getEnd().getTime()));
        result = prime * result + (visible ? 1231 : 1237);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        return equals(obj, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.time.DataTime#equals(java.lang.Object,
     * boolean)
     */
    public boolean equals(Object obj, boolean ignoreSpatial) {

        if (obj == null || !(obj instanceof DataTime)) {
            return false;
        }

        DataTime that = (DataTime) obj;

        if (((DataTime) obj).getRefTime() == null) {
            return primaryDataTime.fcstTime == that.fcstTime;
        }
        if (that instanceof CombinedDataTime) {

            if (this.secondaryDataTime != null
                    && !this.secondaryDataTime
                            .equals(((CombinedDataTime) that).secondaryDataTime)) {
                return false;
            }
        }

        if (ignoreSpatial) {
            return (primaryDataTime.fcstTime == that.fcstTime && primaryDataTime.validPeriod
                    .equals(that.validPeriod));
        } else {
            return (primaryDataTime.fcstTime == that.fcstTime && primaryDataTime.levelValue
                    .equals(that.levelValue));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.time.DataTime#toString()
     */
    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append(super.toString());
        if (this.secondaryDataTime != null) {
            buffer.append(this.secondaryDataTime.toString());
        }
        return buffer.toString();
    }

    @Override
    public CombinedDataTime clone() {
        CombinedDataTime rval = new CombinedDataTime(this,
                this.secondaryDataTime);
        if (this.secondaryDataTime != null) {
            rval.secondaryDataTime = this.secondaryDataTime.clone();
        }
        return rval;
    }

    /**
     * @return the valid time
     */
    public Calendar getValidTime() {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        long primary = refTime.getTime() + (1000 * fcstTime);
        long secondary = secondaryDataTime.refTime.getTime()
                + (1000 * secondaryDataTime.fcstTime);

        if (fcstTime < secondaryDataTime.fcstTime) {
            cal.setTimeInMillis(secondary - (secondary - primary) / 2);
        } else if (fcstTime > secondaryDataTime.fcstTime) {
            cal.setTimeInMillis(primary - (primary - secondary) / 2);
        } else {
            cal.setTimeInMillis(primary);
        }
        return cal;
    }

    /**
     * @return a time matching forecast time in seconds
     */
    public long getMatchFcst() {
        long primary = 60 * (fcstTime / 60);
        long secondary = 60 * (secondaryDataTime.fcstTime / 60);

        if (fcstTime < secondaryDataTime.fcstTime) {
            return secondary - (secondary - primary) / 2;
        } else if (fcstTime > secondaryDataTime.fcstTime) {
            return primary - (primary - secondary) / 2;
        } else {
            return (primary);
        }
    }

    /**
     * @return a time matching ref time
     */
    public long getMatchRef() {
        long primary = refTime.getTime();
        long secondary = secondaryDataTime.refTime.getTime();

        if (refTime.getTime() < secondaryDataTime.refTime.getTime()) {
            return secondary - (secondary - primary) / 2;
        } else if (refTime.getTime() > secondaryDataTime.refTime.getTime()) {
            return primary - (primary - secondary) / 2;
        } else {
            return primary;
        }
    }

    /**
     * 
     * @return get the matching valid time
     */
    public long getMatchValid() {
        long primary = refTime.getTime() + 60 * ((fcstTime * 1000) / 60);
        long secondary = secondaryDataTime.refTime.getTime() + 60
                * ((secondaryDataTime.fcstTime * 1000) / 60);
        if (fcstTime < secondaryDataTime.fcstTime) {
            return (long) (secondary - (secondary - primary) * 0.75);
        } else if (fcstTime > secondaryDataTime.fcstTime) {
            return (long) (primary - (primary - secondary) * 0.75);
        } else {
            return primary;
        }
    }

}
