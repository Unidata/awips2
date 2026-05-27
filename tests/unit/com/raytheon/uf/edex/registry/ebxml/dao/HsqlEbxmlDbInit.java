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
package com.raytheon.uf.edex.registry.ebxml.dao;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.jdbc.Work;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Extends {@link DbInit} to load the database for HSQL.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2013 1650       djohnson     Initial creation
 * 8/15/2013    1682       bphillip     Added additional actions to initDb
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class HsqlEbxmlDbInit extends DbInit {

    /**
     * {@inheritDoc}
     */
    @Override
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void initDb() throws Exception {
        AnnotationConfiguration aConfig = this.getAnnotationConfiguration();
        try {
            dropTables(aConfig);
        } catch (Throwable t) {
            t.printStackTrace();
        }
        createSchema();
        createTables(aConfig);
        // populateDB();
    }

    private void createSchema() {
        final Work work = new Work() {
            @Override
            public void execute(Connection connection) throws SQLException {
                Statement stmt = connection.createStatement();
                stmt.execute("create schema IF NOT EXISTS ebxml;");
                connection.commit();
                stmt.close();

            }
        };

        executeWork(work);
    }

}
