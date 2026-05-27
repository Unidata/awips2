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
package com.raytheon.uf.common.plugin.hpe.data;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Objects;

import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.usertype.UserType;

import com.raytheon.uf.common.plugin.hpe.data.HpeEnums.HpeBiasSource;

/**
 * Custom {@link UserType} to convert the hpe bias source in a
 * {@link HpeRadarResult} back and forth from a {@link HpeBiasSource} to a
 * {@link String}. TODO: if we ever upgrade to JPA 2.1, this entire class can be
 * replaced with an implementation of the much simpler JPA Converter.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2016 5631       bkowal      Initial creation
 * Mar 20, 2019 6140       tgurney     Hibernate 5 UserType fix
 *
 * </pre>
 *
 * @author bkowal
 */

public class HpeBiasSourceUserType implements UserType {

    private static final int[] SQL_TYPES = { Types.VARCHAR };

    public HpeBiasSourceUserType() {
    }

    @Override
    public int[] sqlTypes() {
        return SQL_TYPES;
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Class returnedClass() {
        return Enum.class;
    }

    @Override
    public boolean equals(Object x, Object y) throws HibernateException {
        return Objects.equals(x, y);
    }

    @Override
    public int hashCode(Object x) throws HibernateException {
        assert x != null;
        return x.hashCode();
    }

    @Override
    public Object nullSafeGet(ResultSet rs, String[] names,
            SharedSessionContractImplementor session, Object owner)
            throws HibernateException, SQLException {
        String name = rs.getString(names[0]);
        if (rs.wasNull()) {
            return null;
        }
        return HpeBiasSource.fromString(name);
    }

    @Override
    public void nullSafeSet(PreparedStatement st, Object value, int index,
            SharedSessionContractImplementor session)
            throws HibernateException, SQLException {
        if (value == null) {
            st.setNull(index, Types.VARCHAR);
        } else {
            if (value instanceof HpeBiasSource) {
                st.setString(index, ((HpeBiasSource) value).getBiasSource());
            } else {
                throw new IllegalArgumentException(
                        "Cannot convert value. Was expecting a value of type: "
                                + HpeBiasSource.class.getName()
                                + "; received a value of type: "
                                + value.getClass().getName() + ".");
            }
        }
    }

    @Override
    public Object deepCopy(Object value) throws HibernateException {
        return value;
    }

    @Override
    public boolean isMutable() {
        return false;
    }

    @Override
    public Serializable disassemble(Object value) throws HibernateException {
        return (Serializable) value;
    }

    @Override
    public Object assemble(Serializable cached, Object owner)
            throws HibernateException {
        return cached;
    }

    @Override
    public Object replace(Object original, Object target, Object owner)
            throws HibernateException {
        return original;
    }
}
