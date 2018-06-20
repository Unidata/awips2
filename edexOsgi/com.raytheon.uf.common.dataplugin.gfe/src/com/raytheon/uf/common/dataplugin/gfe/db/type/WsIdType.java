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
import org.hibernate.engine.spi.SessionImplementor;
import org.hibernate.usertype.UserType;

import com.raytheon.uf.common.message.WsId;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2009            randerso     Initial creation
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class WsIdType implements UserType {

    private static final int[] SQL_TYPES = { Types.VARCHAR };

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#assemble(java.io.Serializable,
     * java.lang.Object)
     */
    @Override
    public Object assemble(Serializable cached, Object owner)
            throws HibernateException {
        return cached;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#deepCopy(java.lang.Object)
     */
    @Override
    public Object deepCopy(Object value) throws HibernateException {
        return value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#disassemble(java.lang.Object)
     */
    @Override
    public Serializable disassemble(Object value) throws HibernateException {
        return (Serializable) value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#equals(java.lang.Object,
     * java.lang.Object)
     */
    @Override
    public boolean equals(Object x, Object y) throws HibernateException {
        if (x instanceof WsId && y instanceof WsId) {
            return x.equals(y);
        } else {
            return false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#hashCode(java.lang.Object)
     */
    @Override
    public int hashCode(Object arg0) throws HibernateException {
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#isMutable()
     */
    @Override
    public boolean isMutable() {
        return false;
    }
    
    @Override
    public Object nullSafeGet(ResultSet rs, String[] names,
            SessionImplementor session, Object owner)
            throws HibernateException, SQLException {
        String s = rs.getString(names[0]);
        if (s == null) {
            return null;
        }

        return new WsId(rs.getString(names[0]));
    }

    @Override
    public void nullSafeSet(PreparedStatement st, Object value, int index,
            SessionImplementor session) throws HibernateException, SQLException {
        if (value == null) {
            st.setString(index, null);
        } else {
            st.setString(index, value.toString());
        }
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#replace(java.lang.Object,
     * java.lang.Object, java.lang.Object)
     */
    @Override
    public Object replace(Object original, Object target, Object owner)
            throws HibernateException {
        return original;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#returnedClass()
     */
    @Override
    public Class<?> returnedClass() {
        return WsId.class;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#sqlTypes()
     */
    @Override
    public int[] sqlTypes() {
        return WsIdType.SQL_TYPES;
    }
}
