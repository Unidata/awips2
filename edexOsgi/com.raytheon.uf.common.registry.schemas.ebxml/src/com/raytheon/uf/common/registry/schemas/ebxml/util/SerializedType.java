package com.raytheon.uf.common.registry.schemas.ebxml.util;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import javax.xml.datatype.XMLGregorianCalendar;

import org.hibernate.HibernateException;
import org.hibernate.usertype.UserType;

import com.raytheon.uf.common.serialization.SerializationUtil;

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
            try {
                return SerializationUtil.getJaxbManager().unmarshalFromXml(obj);
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
            try {
                ;
                statement.setString(index, SerializationUtil.getJaxbManager()
                        .marshalToXml(value));
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
