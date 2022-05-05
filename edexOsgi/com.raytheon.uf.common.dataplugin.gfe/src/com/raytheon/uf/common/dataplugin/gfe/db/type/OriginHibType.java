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

package com.raytheon.uf.common.dataplugin.gfe.db.type;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.hibernate.usertype.UserType;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;

/**
 * Custom Hibernate type for the GridDataHistory.OriginType enumeration. This
 * class defines how a GridDataHistory.OriginType enumeration object is saved
 * and retrieved from the database.
 *
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 4/18/08      875        bphillip    Initial Creation
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * Mar 18, 2019 6140       tgurney     Hibernate 5 UserType fix
 *
 * </pre>
 *
 * @author bphillip
 */
public class OriginHibType implements UserType {

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
        if (x instanceof GridDataHistory.OriginType
                && y instanceof GridDataHistory.OriginType) {
            return ((GridDataHistory.OriginType) x).equals(y);
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
    public Object nullSafeGet(ResultSet rs, String[] names,
            SharedSessionContractImplementor session, Object owner)
            throws HibernateException, SQLException {
        return GridDataHistory.OriginType.valueOf(rs.getString(names[0]));
    }

    @Override
    public void nullSafeSet(PreparedStatement st, Object value, int index,
            SharedSessionContractImplementor session)
            throws HibernateException, SQLException {
        if (value == null) {
            st.setString(index, null);
        } else {
            if (value instanceof GridDataHistory.OriginType) {
                st.setString(index,
                        ((GridDataHistory.OriginType) value).name());
            } else {
                throw new HibernateException("value is not of type "
                        + GridDataHistory.OriginType.class.getName());
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
        return GridDataHistory.OriginType.class;
    }

    @Override
    public int[] sqlTypes() {
        return OriginHibType.SQL_TYPES;
    }
}
