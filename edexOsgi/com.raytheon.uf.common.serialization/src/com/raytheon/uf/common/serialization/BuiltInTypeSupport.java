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
package com.raytheon.uf.common.serialization;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

/**
 * Provides serialization support for certain Java built-in classes
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2008  #1448      chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class BuiltInTypeSupport {

    private BuiltInTypeSupport() {

    }

    /**
     * 
     * Serialization support for {@link java.util.Date}
     * 
     * @author chammack
     * @version 1.0
     */
    public static class DateSerializer implements
            ISerializationTypeAdapter<Date> {

        @Override
        public Date deserialize(IDeserializationContext deserializer)
                throws SerializationException {
            long t = deserializer.readI64();
            return new Date(t);
        }

        @Override
        public void serialize(ISerializationContext serializer, Date object)
                throws SerializationException {
            serializer.writeI64(object.getTime());
        }

    }

    /**
     * 
     * Serialization support for {@link java.sql.Timestamp}
     * 
     * @author chammack
     * @version 1.0
     */
    public static class TimestampSerializer implements
            ISerializationTypeAdapter<Timestamp> {

        @Override
        public Timestamp deserialize(IDeserializationContext deserializer)
                throws SerializationException {
            long t = deserializer.readI64();
            return new Timestamp(t);
        }

        @Override
        public void serialize(ISerializationContext serializer, Timestamp object)
                throws SerializationException {
            serializer.writeI64(object.getTime());
        }

    }

    /**
     * Serialization for {@link java.util.Calendar}
     * 
     * 
     * @author chammack
     * @version 1.0
     */
    public static class CalendarSerializer implements
            ISerializationTypeAdapter<Calendar> {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.edex.esb.serialize.ISerializationFactory#deserialize
         * (com.raytheon.edex.esb.serialize.ISerializer)
         */
        @Override
        public Calendar deserialize(IDeserializationContext arg0)
                throws SerializationException {
            long t = arg0.readI64();
            Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            c.setTimeInMillis(t);
            return c;
        }

        @Override
        public void serialize(ISerializationContext arg0, Calendar arg1)
                throws SerializationException {

            long t = arg1.getTime().getTime();
            arg0.writeI64(t);
        }
    }

    public static class SqlDateSerializer implements
            ISerializationTypeAdapter<java.sql.Date> {

        @Override
        public java.sql.Date deserialize(IDeserializationContext deserializer)
                throws SerializationException {
            long t = deserializer.readI64();
            return new java.sql.Date(t);
        }

        @Override
        public void serialize(ISerializationContext serializer,
                java.sql.Date object) throws SerializationException {
            serializer.writeI64(object.getTime());
        }

    }

    /**
     * NOTE: this serializer converts BigDecimal to a double for serialization
     * so does not maintain the full precision of the BigDecimal type
     */
    public static class BigDecimalSerializer implements
            ISerializationTypeAdapter<BigDecimal> {

        /*
         * (non-Javadoc)
         * 
         * @seecom.raytheon.uf.common.serialization.ISerializationTypeAdapter#
         * deserialize
         * (com.raytheon.uf.common.serialization.IDeserializationContext)
         */
        @Override
        public BigDecimal deserialize(IDeserializationContext deserializer)
                throws SerializationException {
            return new BigDecimal(deserializer.readDouble());
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
         * (com.raytheon.uf.common.serialization.ISerializationContext,
         * java.lang.Object)
         */
        @Override
        public void serialize(ISerializationContext serializer,
                BigDecimal object) throws SerializationException {
            serializer.writeDouble(object.doubleValue());
        }

    }
}
