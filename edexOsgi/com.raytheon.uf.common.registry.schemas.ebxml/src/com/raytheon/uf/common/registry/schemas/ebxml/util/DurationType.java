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
package com.raytheon.uf.common.registry.schemas.ebxml.util;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;

import org.hibernate.HibernateException;
import org.hibernate.usertype.UserType;

/**
 * Custom Hibernate type used to persist a java.xml.datatype.Duration object to
 * the database
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 6, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DurationType implements UserType {

    /** Stored as a varchar in the databse */
    private static final int[] SQL_TYPES = { Types.BIGINT };

    @Override
    public Object assemble(Serializable cached, Object owner)
            throws HibernateException {
        return cached;
    }

    @Override
    public Object deepCopy(Object value) throws HibernateException {
        return value;
    }

    @Override
    public Serializable disassemble(Object value) throws HibernateException {
        return (Serializable) value;
    }

    @Override
    public boolean equals(Object x, Object y) throws HibernateException {
        if (x instanceof Duration && y instanceof Duration) {
            return x.equals(y);
        } else {
            return false;
        }
    }

    @Override
    public int hashCode(Object arg0) throws HibernateException {
        return 0;
    }

    @Override
    public boolean isMutable() {
        return false;
    }

    @Override
    public Object nullSafeGet(ResultSet resultSet, String[] names, Object owner)
            throws HibernateException, SQLException {
        long durationInMilliSeconds = resultSet.getLong((names[0]));

        try {
            return DatatypeFactory.newInstance().newDuration(
                    durationInMilliSeconds);
        } catch (DatatypeConfigurationException e) {
            throw new HibernateException(
                    "Error creating new XMLGregorianCalendar", e);
        }
    }

    @Override
    public void nullSafeSet(PreparedStatement statement, Object value, int index)
            throws HibernateException, SQLException {
        if (value == null) {
            statement.setDate(index, null);
        } else {
            Duration dur = (Duration) value;
            statement.setLong(index, dur.getTimeInMillis(new java.util.Date()));
        }

    }

    @Override
    public Object replace(Object original, Object target, Object owner)
            throws HibernateException {
        return original;
    }

    @Override
    public Class<?> returnedClass() {
        return Duration.class;
    }

    @Override
    public int[] sqlTypes() {
        return DurationType.SQL_TYPES;
    }

}
