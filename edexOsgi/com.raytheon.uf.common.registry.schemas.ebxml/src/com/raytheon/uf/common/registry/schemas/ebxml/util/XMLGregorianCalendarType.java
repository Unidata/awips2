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
import java.sql.Timestamp;
import java.sql.Types;
import java.util.GregorianCalendar;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.usertype.UserType;

/**
 * Adapter class to enable storing XMLGregorianCalendar types to the database
 * using Hibernate TODO Add Description
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2012 #184       bphillip     Initial creation
 * 4/9/2013     1802       bphillip    Added null check
 * 7/29/2013    2191       bphillip    Fixed equals method
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * Mar 20, 2019 6140       tgurney     Hibernate 5 UserType fix
 *
 * </pre>
 *
 * @author bphillip
 */
public class XMLGregorianCalendarType implements UserType {

    /** Stored as a varchar in the databse */
    private static final int[] SQL_TYPES = { Types.TIMESTAMP };

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
        if (x instanceof XMLGregorianCalendar
                && y instanceof XMLGregorianCalendar) {
            try {
                return x.equals(y);
            } catch (ClassCastException e) {
                return y.equals(x);
            }
        }
        return false;
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
    public Object nullSafeGet(ResultSet rs, String[] names,
            SharedSessionContractImplementor session, Object owner)
            throws HibernateException, SQLException {
        GregorianCalendar cal = new GregorianCalendar();
        Timestamp date = rs.getTimestamp(names[0]);
        if (date == null) {
            return null;
        }
        cal.setTimeInMillis(date.getTime());

        try {
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(cal);
        } catch (DatatypeConfigurationException e) {
            throw new HibernateException(
                    "Error creating new XMLGregorianCalendar", e);
        }
    }

    @Override
    public void nullSafeSet(PreparedStatement st, Object value, int index,
            SharedSessionContractImplementor session)
            throws HibernateException, SQLException {
        if (value == null) {
            st.setDate(index, null);
        } else {
            XMLGregorianCalendar cal = (XMLGregorianCalendar) value;
            st.setTimestamp(index, new Timestamp(
                    cal.toGregorianCalendar().getTime().getTime()));
        }

    }

    @Override
    public Object replace(Object original, Object target, Object owner)
            throws HibernateException {
        return original;
    }

    @Override
    public Class<?> returnedClass() {
        return XMLGregorianCalendar.class;
    }

    @Override
    public int[] sqlTypes() {
        return XMLGregorianCalendarType.SQL_TYPES;
    }
}
