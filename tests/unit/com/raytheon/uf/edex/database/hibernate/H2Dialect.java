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
package com.raytheon.uf.edex.database.hibernate;

import java.sql.Types;

import org.hibernate.HibernateException;

/**
 * Extend default H2Dialect to allow for a larger default column length, when
 * required.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2013 1693       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class H2Dialect extends org.hibernate.dialect.H2Dialect {
    private static final int LARGE_DEFAULT_LENGTH = 3000;

    @Override
    public String getTypeName(int code, int length, int precision, int scale)
            throws HibernateException {
        // Overridden to return a larger column size for default length binary
        // columns, to emulate Postgres' column size growing ability
        if (code == Types.VARBINARY
                && length == org.hibernate.mapping.Column.DEFAULT_LENGTH) {
            length = LARGE_DEFAULT_LENGTH;
        }
        return super.getTypeName(code, length, precision, scale);
    }
}
