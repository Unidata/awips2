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
/*
 * GeometryType.java
 * 
 * PostGIS extension for PostgreSQL JDBC driver - EJB3 Tutorial
 * 
 * (C) 2006  Norman Barker <norman.barker@gmail.com>
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA or visit the web at
 * http://www.gnu.org.
 * 
 * $Id: GeometryType.java 2531 2006-11-22 10:42:17Z mschaber $
 */

// Note that for now I have "imported" this class from the postgis ejb3 source
// tree. Currently there is no postgis jar that exposes this class. 
// package org.postgis.hibernate;
package com.raytheon.edex.db.objects.hibernate;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import org.hibernate.HibernateException;
import org.hibernate.usertype.UserType;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKBWriter;

/**
 * @author nbarker $date 16/8/06
 */
public class GeometryType implements UserType {
    private static final int[] SQL_TYPES = { Types.BLOB };

    /**
     * @see org.hibernate.usertype.UserType#deepCopy(java.lang.Object)
     */
    public Object deepCopy(Object value) throws HibernateException {
        return value;
    }

    /**
     * @see org.hibernate.usertype.UserType#equals(java.lang.Object,
     *      java.lang.Object)
     */
    public boolean equals(Object x, Object y) throws HibernateException {
        if (x == y) {
            return true;
        } else if (x == null || y == null) {
            return false;
        } else {
            return x.equals(y);
        }
    }

    /**
     * @see org.hibernate.usertype.UserType#hashCode(java.lang.Object)
     */
    public int hashCode(Object arg0) throws HibernateException {
        // TODO Auto-generated method stub
        return 0;
    }

    /**
     * @see org.hibernate.usertype.UserType#isMutable()
     */
    public boolean isMutable() {
        return false;
    }

    /**
     * )
     * 
     * @see org.hibernate.usertype.UserType#nullSafeGet(java.sql.ResultSet,
     *      java.lang.String[], java.lang.Object)
     */
    public Geometry nullSafeGet(ResultSet resultSet, String[] names,
            Object owner) throws HibernateException, SQLException {

        WKBReader wkbReader = new WKBReader();
        Geometry result = null;
        String hex = resultSet.getString(names[0]);
        if (hex != null) {
            try {
                result = wkbReader.read(WKBReader.hexToBytes(hex));
            } catch (ParseException e) {
                throw new HibernateException(
                        "Error parsing JTS geometry object", e);
            }
        }
        return result;
    }

    /**
     * @see org.hibernate.usertype.UserType#nullSafeSet(java.sql.PreparedStatement,
     *      java.lang.Object, int)
     */
    public void nullSafeSet(PreparedStatement statement, Object value, int index)
            throws HibernateException, SQLException {
        if (value == null) {
            statement.setBytes(index, null);
        } else {
            statement.setBytes(index, new WKBWriter().write((Geometry) value));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#replace(java.lang.Object,
     * java.lang.Object, java.lang.Object)
     */
    public Object replace(Object original, Object target, Object owner)
            throws HibernateException {
        return original;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hibernate.usertype.UserType#returnedClass()
     */
    public Class<?> returnedClass() {
        return Geometry.class;
    }

    /**
     * @see org.hibernate.usertype.UserType#sqlTypes()
     */
    public int[] sqlTypes() {
        return GeometryType.SQL_TYPES;
    }

    /**
     * @see org.hibernate.usertype.UserType#assemble(java.io.Serializable,
     *      java.lang.Object)
     */
    public Object assemble(Serializable cached, Object owner)
            throws HibernateException {
        return cached;
    }

    /**
     * @see org.hibernate.usertype.UserType#disassemble(java.lang.Object)
     */
    public Serializable disassemble(Object value) throws HibernateException {
        return (Serializable) value;
    }

}
