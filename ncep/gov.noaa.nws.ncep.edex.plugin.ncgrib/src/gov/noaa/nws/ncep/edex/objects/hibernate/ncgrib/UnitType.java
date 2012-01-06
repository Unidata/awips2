package gov.noaa.nws.ncep.edex.objects.hibernate.ncgrib;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.hibernate.HibernateException;
import org.hibernate.usertype.UserType;

public class UnitType implements UserType {

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
        if (x == null) {
            return false;
        } else {
            return ((Unit<?>) x).equals(y);
        }
    }

    @Override
    public int hashCode(Object x) throws HibernateException {
        return 0;
    }

    @Override
    public boolean isMutable() {
        return false;
    }

    @Override
    public Object nullSafeGet(ResultSet resultSet, String[] names, Object owner)
            throws HibernateException, SQLException {
        Unit<?> unit = null;
        try {
            String unitName = resultSet.getString(names[0]);
            if (unitName == null) {
                unit = Unit.ONE;
            } else if (unitName.equals("")) {
                unit = Unit.ONE;
            } else {
                unit = (Unit<?>) UnitFormat.getUCUMInstance().parseObject(
                        unitName);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return unit;
    }

    @Override
    public void nullSafeSet(PreparedStatement statement, Object value, int index)
            throws HibernateException, SQLException {
        if (value == null) {
            statement.setString(index, null);
        } else {
            statement.setString(index, value.toString());
        }
    }

    @Override
    public Object replace(Object original, Object target, Object owner)
            throws HibernateException {
        return original;
    }

    @Override
    public Class<?> returnedClass() {
        return Unit.class;
    }

    @Override
    public int[] sqlTypes() {
        return UnitType.SQL_TYPES;
    }

}
