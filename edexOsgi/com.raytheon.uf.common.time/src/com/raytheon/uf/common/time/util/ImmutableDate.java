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
package com.raytheon.uf.common.time.util;

import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.common.time.SimulatedTime;

/**
 * An immutable version of {@link Date}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Initial creation
 * Sep 19, 2012 0726       jspinks      Added XmlJavaTypeAdaptor annotation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlJavaTypeAdapter(value=ImmutableDateAdapter.class) 
// @Immutable
@DynamicSerialize
@DynamicSerializeTypeAdapter(factory = ImmutableDate.class)
public final class ImmutableDate extends Date implements
        ISerializationTypeAdapter<ImmutableDate> {

    private static final long serialVersionUID = -4228048757897650258L;

    private static final String IMMUTABLE_MSG = "This version of date is immutable.";

    private static final String IMMUTABLE_AND_DEPRECATED_MSG = IMMUTABLE_MSG
            + "  In addition, this method is deprecated.";

    private final int hashCode;

    /**
     * Construct an {@link ImmutableDate} from the current time.
     */
    public ImmutableDate() {
        this(SimulatedTime.getSystemTime().getMillis());
    }


    /**
     * Construct an {@link ImmutableDate} from a {@link Date} object.
     * 
     * @param date
     *            the date
     */
    public ImmutableDate(Date date) {
        this(date.getTime());
    }

    /**
     * Construct an {@link ImmutableDate} from the specified time in
     * milliseconds.
     * 
     * @param time
     *            the millliseconds
     */
    public ImmutableDate(long time) {
        super(time);

        this.hashCode = super.hashCode();
    }

    /**
     * Overridden to throw {@link UnsupportedOperationException}.
     * 
     * @param year
     * @throws UnsupportedOperationException
     */
    @Override
    public void setYear(int year) {
        throw new UnsupportedOperationException(
                IMMUTABLE_AND_DEPRECATED_MSG);
    }

    /**
     * Overridden to throw {@link UnsupportedOperationException}.
     * 
     * @param month
     * @throws UnsupportedOperationException
     */
    @Override
    public void setMonth(int month) {
        throw new UnsupportedOperationException(IMMUTABLE_AND_DEPRECATED_MSG);
    }

    /**
     * Overridden to throw {@link UnsupportedOperationException}.
     * 
     * @param date
     * @throws UnsupportedOperationException
     */
    @Override
    public void setDate(int date) {
        throw new UnsupportedOperationException(IMMUTABLE_AND_DEPRECATED_MSG);
    }

    /**
     * Overridden to throw {@link UnsupportedOperationException}.
     * 
     * @param hours
     * @throws UnsupportedOperationException
     */
    @Override
    public void setHours(int hours) {
        throw new UnsupportedOperationException(IMMUTABLE_AND_DEPRECATED_MSG);
    }

    /**
     * Overridden to throw {@link UnsupportedOperationException}.
     * 
     * @param minutes
     * @throws UnsupportedOperationException
     */
    @Override
    public void setMinutes(int minutes) {
        throw new UnsupportedOperationException(IMMUTABLE_AND_DEPRECATED_MSG);
    }

    /**
     * Overridden to throw {@link UnsupportedOperationException}.
     * 
     * @param seconds
     * @throws UnsupportedOperationException
     */
    @Override
    public void setSeconds(int seconds) {
        throw new UnsupportedOperationException(IMMUTABLE_AND_DEPRECATED_MSG);
    }

    /**
     * Overridden to throw {@link UnsupportedOperationException}.
     * 
     * @param time
     * @throws UnsupportedOperationException
     */
    @Override
    public void setTime(long time) {
        throw new UnsupportedOperationException(IMMUTABLE_MSG);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return hashCode;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void serialize(ISerializationContext serializer, ImmutableDate object)
            throws SerializationException {
        serializer.writeI64(object.getTime());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ImmutableDate deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return new ImmutableDate(deserializer.readI64());
    }
}
