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

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.common.time.domain.api.IDuration;
import com.raytheon.uf.common.time.domain.api.ITimeInterval;
import com.raytheon.uf.common.time.domain.api.ITimePoint;

/**
 * Implementation of {@link ITimeInterval}. Intentionally package-private as it
 * is an implementation detail, and not part of the public API. All access
 * should be constrained through {@link TimeIntervals}.
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
@XmlJavaTypeAdapter(value = ITimeIntervalTypeAdapter.class)
@DynamicSerializeTypeAdapter(factory = ITimeIntervalTypeAdapter.class)
class TimeInterval implements ITimeInterval {

    private final ITimePoint intervalStart;

    private final ITimePoint intervalEnd;

    /**
     * Constructor.
     * 
     * @param intervalStart
     *            the start of the interval
     * @param intervalEnd
     *            the end of the interval
     */
    TimeInterval(ITimePoint intervalStart, ITimePoint intervalEnd) {
        this.intervalStart = intervalStart;
        this.intervalEnd = intervalEnd;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ITimePoint getStart() {
        return intervalStart;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ITimePoint getEnd() {
        return intervalEnd;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean containsTimePoint(ITimePoint timePoint) {
        final boolean intervalStartSameOrBefore = intervalStart
                .isBefore(timePoint) || intervalStart.isSame(timePoint);
        final boolean intervalEndSameOrAfter = intervalEnd.isAfter(timePoint)
                || intervalEnd.isSame(timePoint);

        return intervalStartSameOrBefore && intervalEndSameOrAfter;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IDuration getDuration() {
        return Durations.between(intervalStart, intervalEnd);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ITimeInterval) {
            ITimeInterval other = (ITimeInterval) obj;

            return this.getStart().equals(other.getStart())
                    && this.getEnd().equals(other.getEnd());
        }
        return super.equals(obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return getStart().hashCode() + getEnd().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("start [").append(getStart()).append("]");
        sb.append(" end [").append(getEnd()).append("]");
        sb.append(" duration [").append(getDuration()).append("]");

        return sb.toString();
    }
}
