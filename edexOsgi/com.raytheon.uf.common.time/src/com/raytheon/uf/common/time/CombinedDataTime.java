package com.raytheon.uf.common.time;

import java.util.Calendar;
import java.util.Iterator;
import java.util.TimeZone;

import javax.persistence.Transient;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2011            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
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
        this.majorKey = primaryDataTime.majorKey;
        this.minorKey = primaryDataTime.minorKey;
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
        result = prime * result + ((majorKey == null) ? 0 : majorKey.ordinal());
        result = prime * result + ((minorKey == null) ? 0 : minorKey.ordinal());
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

    /**
     * Returns true if the left hand side is greater than the right hand side
     * 
     * @param rhs
     *            the right hand side
     * @return true if left hand side is greater than
     */
    public boolean greaterThan(DataTime rhs) {

        if (rhs.getRefTime() == null) {
            return (fcstTime > rhs.getFcstTime());

        } else {
            if (matchMode) {
                switch (majorKey) {
                case INITIAL_TIME:
                    if (getMatchRef() > rhs.getMatchRef())
                        return true;
                    if (getMatchRef() < rhs.getMatchRef())
                        return false;
                    break;
                case FORECAST_TIME:
                    if (getRefTime().getTime() == 0)
                        return false;
                    if (getMatchFcst() > rhs.getMatchFcst())
                        return true;
                    if (getMatchFcst() < rhs.getMatchFcst())
                        return false;
                    break;
                case VALID_TIME:
                    if (getMatchValid() > rhs.getMatchValid())
                        return true;
                    if (getMatchValid() < rhs.getMatchValid())
                        return false;
                }
                switch (minorKey) {
                case INITIAL_TIME:
                    if (getMatchRef() > rhs.getMatchRef())
                        return true;
                    if (getMatchRef() < rhs.getMatchRef())
                        return false;
                    break;
                case FORECAST_TIME:
                    if (getMatchFcst() > rhs.getMatchFcst())
                        return true;
                    if (getMatchFcst() < rhs.getMatchFcst())
                        return false;
                    break;
                case VALID_TIME:
                    if (getMatchFcst() > rhs.getMatchFcst())
                        return true;
                    if (getMatchFcst() < rhs.getMatchFcst())
                        return false;
                }
                if (getLevelValue() > rhs.getLevelValue()) {
                    return true;
                }
            } else {
                switch (majorKey) {
                case INITIAL_TIME:
                    if (refTime.getTime() > rhs.refTime.getTime())
                        return true;
                    if (refTime.getTime() < rhs.refTime.getTime())
                        return false;
                    break;
                case FORECAST_TIME:
                    if (fcstTime > rhs.fcstTime)
                        return true;
                    if (fcstTime < rhs.fcstTime)
                        return false;
                    break;
                case VALID_TIME:
                    if (getValidTime().getTimeInMillis() > rhs.getValidTime()
                            .getTimeInMillis())
                        return true;
                    if (getValidTime().getTimeInMillis() < rhs.getValidTime()
                            .getTimeInMillis())
                        return false;
                    if (refTime.getTime() > rhs.getRefTime().getTime())
                        return true;
                    if (refTime.getTime() < rhs.getRefTime().getTime())
                        return false;
                }
                switch (minorKey) {
                case INITIAL_TIME:
                    if (refTime.getTime() > rhs.refTime.getTime())
                        return true;
                    break;
                case FORECAST_TIME:
                    if (fcstTime > rhs.fcstTime)
                        return true;
                    break;
                case VALID_TIME:
                    if (refTime.getTime() + (fcstTime * 1000) > rhs.refTime
                            .getTime() + (rhs.fcstTime * 1000))
                        return true;
                }
            }
        }
        return false;

    }
}
