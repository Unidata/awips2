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

import java.util.concurrent.TimeUnit;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.common.time.domain.api.IDuration;

/**
 * Implementation of {@link IDuration}. Intentionally package-private as it is
 * an implementation detail, and not part of the public API. All access should
 * be constrained through {@link Durations}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlJavaTypeAdapter(value = IDurationTypeAdapter.class)
@DynamicSerializeTypeAdapter(factory = IDurationTypeAdapter.class)
class Duration implements IDuration {

    private final long valueAsNanoseconds;

    /**
     * Constructor.
     * 
     * @param value
     *            the unit value
     * @param unit
     *            the unit
     */
    Duration(long value, TimeUnit unit) {
        this.valueAsNanoseconds = unit.toNanos(value);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getNanos() {
        return convert(TimeUnit.NANOSECONDS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getMicros() {
        return convert(TimeUnit.MICROSECONDS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getMillis() {
        return convert(TimeUnit.MILLISECONDS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getSeconds() {
        return convert(TimeUnit.SECONDS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getMinutes() {
        return convert(TimeUnit.MINUTES);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getHours() {
        return convert(TimeUnit.HOURS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getDays() {
        return convert(TimeUnit.DAYS);
    }

    private long convert(TimeUnit targetUnit) {
        return targetUnit.convert(valueAsNanoseconds, TimeUnit.NANOSECONDS);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IDuration) {
            IDuration other = (IDuration) obj;
            return other.getNanos() == this.getNanos();
        }
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        return (int) this.getNanos();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Duration plus(IDuration anotherDuration) {
        return new Duration(getNanos() + anotherDuration.getNanos(),
                TimeUnit.NANOSECONDS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Duration minus(IDuration anotherDuration) {
        return new Duration(getNanos() - anotherDuration.getNanos(),
                TimeUnit.NANOSECONDS);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return Durations.toString(this);
    }

}
