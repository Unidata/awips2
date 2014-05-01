/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.filter;

import java.util.Date;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * In memory data record filtering using time fields
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class TemporalFilter extends AbstractPdoFilter {

    public static enum TimeOp {
        AnyInteracts, After, Before, Begins, BegunBy, TContains, During, TEquals, TOverlaps, Meets, OverlappedBy, MetBy, Ends, EndedBy
    };

    protected DataTime time;

    protected TimeOp op;

    /**
     * @param time
     * @param op
     */
    public TemporalFilter(TimeOp op, DataTime time) {
        this.time = time;
        this.op = op;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.filter.AbstractFilterOp#matches(com.raytheon
     * .uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public boolean matches(PluginDataObject pdo) {
        DataTime other = pdo.getDataTime();
        if (other == null) {
            return false;
        }
        TimeRange otherRange = other.getValidPeriod();
        Date t1Start = otherRange.getStart();
        Date t1End = otherRange.getEnd();
        TimeRange range = time.getValidPeriod();
        Date t2Start = range.getStart();
        Date t2End = range.getEnd();
        switch (op) {
        case AnyInteracts:
            if (isRange(time)) {
                return beforeOrEqual(t1Start, t2End)
                        && afterOrEqual(t1End, t2Start);
            } else {
                return beforeOrEqual(t1Start, t2Start)
                        && afterOrEqual(t1End, t2End);
            }
        case After:
            return t1Start.after(t2End);
        case Before:
            return t1End.before(t2Start);
        case Begins:
            if (isRange(time)) {
                if (isRange(other)) {
                    return t1Start.equals(t2Start) && t1End.before(t2End);
                } else {
                    return t1Start.equals(t2Start);
                }
            }
			break;
        case BegunBy:
            if (isRange(other)) {
                if (isRange(time)) {
                    return t1Start.equals(t2Start) && t1End.after(t2End);
                } else {
                    return t1Start.equals(t2Start);
                }
            }
			break;
        case During:
            if (isRange(time)) {
                return t1Start.after(t2Start) && t1End.before(t2End);
            }
			break;
        case EndedBy:
            if (isRange(other)) {
                if (isRange(time)) {
                    return t1Start.before(t2Start) && t1End.equals(t2End);
                } else {
                    return t1End.equals(t2End);
                }
            }
			break;
        case Ends:
            if (isRange(time)) {
                if (isRange(other)) {
                    return t1Start.after(t2Start) && t1End.equals(t2End);
                } else {
                    return t1End.equals(t2End);
                }
            }
			break;
        case Meets:
            if (isRange(other) && isRange(time)) {
                return t1End.equals(t2Start);
            }
			break;
        case MetBy:
            if (isRange(other) && isRange(time)) {
                return t1Start.equals(t2End);
            }
            break;
        case OverlappedBy:
            if (isRange(other) && isRange(time)) {
                return t1Start.after(t2Start) && t1Start.before(t2End)
                        && t1End.after(t2End);
            }
			break;
        case TContains:
            if (isRange(other)) {
                if (isRange(time)) {
                    return t1Start.before(t2Start) && t2End.before(t1End);
                } else {
                    return t1Start.before(t2Start) && t1End.after(t2End);
                }
            }
			break;
        case TEquals:
            if (!(isRange(other) ^ isRange(time))) {
                return t1Start.equals(t2Start) && t1End.equals(t2End);
            }
        case TOverlaps:
            if (isRange(other) && isRange(time)) {
                return t1Start.before(t2Start) && t1End.after(t2Start)
                        && t1End.before(t2End);
            }
			break;
        }

        return false;
    }

    /**
     * @param one
     * @param two
     * @return true if one is before or equal to two
     */
    public static boolean beforeOrEqual(Date one, Date two) {
        return one.equals(two) || one.before(two);
    }

    /**
     * @param one
     * @param two
     * @return true if one is after or equal to two
     */
    public static boolean afterOrEqual(Date one, Date two) {
        return one.equals(two) || one.after(two);
    }

    public static boolean isRange(DataTime time) {
        return time.getUtilityFlags().contains(DataTime.FLAG.PERIOD_USED);
    }

    /**
     * @return the time
     */
    public DataTime getTime() {
        return time;
    }

    /**
     * @return the op
     */
    public TimeOp getOp() {
        return op;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String tstamp;
        if (isRange(this.time)) {
            tstamp = time.getValidPeriod().toString();
        } else {
            tstamp = time.getRefTime().toString();
        }
        return "[time " + op + " " + tstamp + "]";
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((op == null) ? 0 : op.hashCode());
        result = prime * result + ((time == null) ? 0 : time.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        TemporalFilter other = (TemporalFilter) obj;
        if (op != other.op)
            return false;
        if (time == null) {
            if (other.time != null)
                return false;
        } else if (!time.equals(other.time))
            return false;
        return true;
    }

}
