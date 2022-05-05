package com.raytheon.uf.common.dataplugin.gfe.db.type;

import java.awt.Point;
import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Objects;

import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.usertype.UserType;

/**
 * Hibernate type adapter for java.awt.Point
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Feb 11, 2020  7596     randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */
public class AwtPointType implements UserType {

    @Override
    public int[] sqlTypes() {
        return new int[] { Types.INTEGER, Types.INTEGER };
    }

    @Override
    public Class<?> returnedClass() {
        return Point.class;
    }

    @Override
    public boolean equals(Object x, Object y) throws HibernateException {
        if (x == null) {
            return y == null;
        }

        if (y == null) {
            return false;
        }

        return ((Point) x).equals(y);
    }

    @Override
    public int hashCode(Object x) throws HibernateException {
        if (x == null) {
            return 0;
        }

        return x.hashCode();
    }

    @Override
    public Object nullSafeGet(ResultSet rs, String[] names,
            SharedSessionContractImplementor session, Object owner)
            throws HibernateException, SQLException {
        if (rs.wasNull()) {
            return null;
        }

        int x = rs.getInt(names[0]);
        int y = rs.getInt(names[1]);

        Point pt = new Point(x, y);
        return pt;
    }

    @Override
    public void nullSafeSet(PreparedStatement st, Object value, int index,
            SharedSessionContractImplementor session)
            throws HibernateException, SQLException {
        if (Objects.isNull(value)) {
            st.setNull(index, Types.INTEGER);
        } else {
            Point pt = (Point) value;
            st.setInt(index, pt.x);
            st.setInt(index + 1, pt.y);
        }
    }

    @Override
    public Object deepCopy(Object value) throws HibernateException {
        return new Point((Point) value);
    }

    @Override
    public boolean isMutable() {
        return true;
    }

    @Override
    public Serializable disassemble(Object value) throws HibernateException {
        return (Serializable) deepCopy(value);
    }

    @Override
    public Object assemble(Serializable cached, Object owner)
            throws HibernateException {
        return deepCopy(cached);
    }

    @Override
    public Object replace(Object original, Object target, Object owner)
            throws HibernateException {
        if (original != null) {
            return deepCopy(original);
        }
        return null;
    }

}
