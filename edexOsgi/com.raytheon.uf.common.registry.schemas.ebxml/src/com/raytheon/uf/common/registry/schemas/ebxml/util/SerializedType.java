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

import javax.xml.datatype.XMLGregorianCalendar;

import org.hibernate.HibernateException;
import org.hibernate.usertype.UserType;

/**
 * A serialized type
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----                          Initial creation
 * Oct 31, 2013 2361      njensen      Use specific JAXBManager instead of SerializationUtil
 * Nov 14, 2013 2552      bkowal       EbxmlJaxbManager is now accessed via getInstance 
 * Dec 04, 2013 2584      dhladky      Version based EbxmlJaxbManager
 * 
 * </pre>
 * 
 */
public class SerializedType implements UserType {

    /** Stored as a varchar in the databse */
    private static final int[] SQL_TYPES = { Types.VARCHAR };

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
        if (x != null) {
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
        String obj = resultSet.getString(names[0]);

        if (obj != null) {
            try { // We always marshall to current version for to XML conversions
                return EbxmlJaxbManager.getInstance().getJaxbManager()
                        .unmarshalFromXml(obj);
            } catch (Exception e) {
                throw new HibernateException("Error retrieving AnyType data", e);
            }
        } else {
            return null;
        }
    }

    @Override
    public void nullSafeSet(PreparedStatement statement, Object value, int index)
            throws HibernateException, SQLException {
        if (value == null) {
            statement.setString(index, null);
        } else {
            try { // We always marshall to current version for to XML conversions
                statement.setString(index, EbxmlJaxbManager.getInstance()
                        .getJaxbManager().marshalToXml(value));
            } catch (Exception e) {
                throw new HibernateException("Error storing AnyType data", e);
            }
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
        return SerializedType.SQL_TYPES;
    }

}
