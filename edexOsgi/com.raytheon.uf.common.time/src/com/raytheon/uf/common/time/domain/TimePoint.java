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
package com.raytheon.uf.common.time.domain;

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.common.time.domain.api.ITimeInterval;
import com.raytheon.uf.common.time.domain.api.ITimePoint;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Implementation of {@link ITimePoint}. Intentionally package-private as it is
 * an implementation detail, and not part of the public API. All access should
 * be constrained through {@link TimePoints}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlJavaTypeAdapter(value = ITimePointTypeAdapter.class)
@DynamicSerializeTypeAdapter(factory = ITimePointTypeAdapter.class)
class TimePoint implements ITimePoint {

    private final long milliseconds;

    /**
     * Construct a {@link TimePoint} for the specified milliseconds.
     * 
     * @param milliseconds
     */
    TimePoint(long milliseconds) {
        this.milliseconds = milliseconds;
    }

    /**
     * Deep-copy another {@link ITimePoint}.
     * 
     * @param toCopy
     */
    TimePoint(ITimePoint toCopy) {
        this(toCopy.asMilliseconds());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Date asDate() {
        return new Date(milliseconds);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long asMilliseconds() {
        return milliseconds;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isAfter(ITimePoint anotherTimePoint) {
        return this.asMilliseconds() > anotherTimePoint.asMilliseconds();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBefore(ITimePoint anotherTimePoint) {
        return this.asMilliseconds() < anotherTimePoint.asMilliseconds();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSame(ITimePoint anotherPoint) {
        return this.asMilliseconds() == anotherPoint.asMilliseconds();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isWithin(ITimeInterval interval) {
        return interval.containsTimePoint(this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ITimePoint) {
            ITimePoint other = (ITimePoint) obj;
            return this.isSame(other);
        }
        return super.equals(obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return (int) asMilliseconds();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return TimeUtil.formatDate(asDate());
    }
}
