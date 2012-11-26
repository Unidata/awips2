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
package com.raytheon.uf.common.time;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Bin Offset:
 * 
 * Defines the offset in the positive and negative direction expressed in
 * seconds (these quantities are always positive). Also defines a virtual offset
 * which is added to any query times (may be negative). This is useful for when
 * querying for data that is a delta from the effective time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class BinOffset implements ISerializableObject {

    @DynamicSerializeElement
    private int posOffset;

    @DynamicSerializeElement
    private int negOffset;

    @DynamicSerializeElement
    private int virtualOffset;

    public BinOffset() {
        this(0, 0, 0);
    }

    public BinOffset(int posOffset, int negOffset) {
        this(posOffset, negOffset, 0);
    }

    public BinOffset(int posOffset, int negOffset, int virtualOffset) {
        setPosOffset(posOffset);
        setNegOffset(negOffset);
        setVirtualOffset(virtualOffset);
    }

    public int getInterval() {
        return posOffset + negOffset;
    }

    /**
     * Takes range and offsets into account to determine what the time should be
     * normalized to in the range
     * 
     * @param timeInMillis
     *            the time to normalize in millis
     * @return the normalized time in millis
     */
    // public long getNormalizedTime(long timeInMillis) {
    // long range = getInterval() * 1000l;
    // long posInMillis = posOffset * 1000l;
    // long adjustedTime = (timeInMillis / range) * range;
    // if (timeInMillis >= (adjustedTime + posInMillis)) {
    // adjustedTime += range;
    // }
    // return adjustedTime;
    // }
    /**
     * Takes range and offsets into account to determine what the time should be
     * normalized to in the range
     * 
     * @param time
     *            the time to normalize
     * @return the normalized time
     */
    public DataTime getNormalizedTime(DataTime time) {
        // nothing to offset
        if (posOffset == 0 && negOffset == 0 && virtualOffset == 0) {
            return time;
        }

        /**
         * Logic for adjustedTime: round down to lower interval, check to see if
         * the time is in between the lower time interval plus the pos offset,
         * if so use it, otherwise use the next interval
         */
        long timeInMillis = time.getRefTimeAsCalendar().getTimeInMillis();
        long adjustedTime = timeInMillis;
        long range = getInterval() * 1000l;

        if (range != 0) {
            long posInMillis = posOffset * 1000l;
            adjustedTime = ((timeInMillis / range) * range);
            if ((timeInMillis) >= (adjustedTime + posInMillis)) {
                adjustedTime += range;
            }
        }
        adjustedTime -= virtualOffset * 1000l;
        DataTime normalizedTime;
        if (time.getUtilityFlags().contains(DataTime.FLAG.FCST_USED)) {
            normalizedTime = new DataTime(new Date(adjustedTime),
                    time.getFcstTime());
        } else {
            normalizedTime = new DataTime(new Date(adjustedTime));
        }
        return normalizedTime;
    }

    /**
     * Takes range and offsets into account to determine what the times should
     * be normalized to in the range
     * 
     * @param times
     *            the times to normalize
     * @return the normalized times
     */
    public Set<DataTime> getNormalizedTimes(DataTime[] times) {
        Set<DataTime> set = new HashSet<DataTime>();
        for (DataTime dt : times) {
            set.add(getNormalizedTime(dt));
        }
        return set;
    }

    public TimeRange getTimeRange(DataTime baseTime) {
        long negSkew = -negOffset * 1000l;
        long posSkew = posOffset * 1000l;
        posSkew += virtualOffset * 1000l;
        negSkew += virtualOffset * 1000l;

        // Construct a time range
        long start = baseTime.getValidTime().getTimeInMillis() + negSkew;
        long end = baseTime.getValidTime().getTimeInMillis() + posSkew;

        TimeRange tr = new TimeRange(start, end);
        return tr;
    }

    public TimeRange getPruningRange(DataTime firstTime, DataTime lastTime) {
        // extra time slop to not throw away close values
        int overlapTime = 60 * 1000;

        long start = firstTime.getValidTime().getTimeInMillis()
                - Math.abs(virtualOffset * 1000 * 4) - overlapTime;
        long end = lastTime.getValidTime().getTimeInMillis()
                + Math.abs(virtualOffset * 1000 * 4) + overlapTime;

        return new TimeRange(start, end);
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
        result = prime * result + negOffset;
        result = prime * result + posOffset;
        result = prime * result + virtualOffset;
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
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
        BinOffset other = (BinOffset) obj;
        if (negOffset != other.negOffset) {
            return false;
        }
        if (posOffset != other.posOffset) {
            return false;
        }
        if (virtualOffset != other.virtualOffset) {
            return false;
        }
        return true;
    }

    /**
     * @return the posOffset
     */
    public int getPosOffset() {
        return posOffset;
    }

    /**
     * @param posOffset
     *            the posOffset to set
     */
    @XmlAttribute
    public void setPosOffset(int posOffset) {
        Validate.isTrue(posOffset >= 0, "posOffset should be >= 0");
        this.posOffset = posOffset;
    }

    /**
     * @return the negOffset
     */
    public int getNegOffset() {
        return negOffset;
    }

    /**
     * @param negOffset
     *            the negOffset to set
     */
    @XmlAttribute
    public void setNegOffset(int negOffset) {
        Validate.isTrue(negOffset >= 0, "negOffset should be >= 0");
        this.negOffset = negOffset;
    }

    /**
     * @return the virtualOffset
     */
    public int getVirtualOffset() {
        return virtualOffset;
    }

    /**
     * @param virtualOffset
     *            the virtualOffset to set
     */
    @XmlAttribute
    public void setVirtualOffset(int virtualOffset) {
        this.virtualOffset = virtualOffset;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "BinOffset [posOffset=" + posOffset + ", negOffset=" + negOffset
                + ", virtualOffset=" + virtualOffset + "]";
    }

}