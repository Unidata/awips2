package com.raytheon.uf.common.dataplugin.gfe.db.type;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Objects;

import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.usertype.UserType;
import org.locationtech.jts.geom.Coordinate;

/**
 * Hibernate type adapter for 2D JTS coordinates
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
public class Coordinate2DType implements UserType {

    @Override
    public int[] sqlTypes() {
        return new int[] { Types.DOUBLE, Types.DOUBLE };
    }

    @Override
    public Class<?> returnedClass() {
        return Coordinate.class;
    }

    @Override
    public boolean equals(Object x, Object y) throws HibernateException {
        if (x == null) {
            return y == null;
        }

        if (y == null) {
            return false;
        }

        return ((Coordinate) x).equals2D((Coordinate) y);
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

        double x = rs.getDouble(names[0]);
        double y = rs.getDouble(names[1]);

        Coordinate coord = new Coordinate(x, y);
        return coord;
    }

    @Override
    public void nullSafeSet(PreparedStatement st, Object value, int index,
            SharedSessionContractImplementor session)
            throws HibernateException, SQLException {
        if (Objects.isNull(value)) {
            st.setNull(index, Types.DOUBLE);
        } else {
            Coordinate coord = (Coordinate) value;
            st.setDouble(index, coord.x);
            st.setDouble(index + 1, coord.y);
        }
    }

    @Override
    public Object deepCopy(Object value) throws HibernateException {
        return new Coordinate((Coordinate) value);
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
