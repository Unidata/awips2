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

import java.util.ArrayList;
import java.util.List;

import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.impl.SessionFactoryImpl;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Validates the Postgres instance of EBXML.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2013 1693       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class EbxmlPostgresValidationStrategy implements
        IEbxmlDatabaseValidationStrategy {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EbxmlPostgresValidationStrategy.class);

    /** Query to check which tables exist in the ebxml database */
    private static final String TABLE_CHECK_QUERY = "SELECT tablename FROM pg_tables where schemaname = 'ebxml';";

    /** Constant used for table regeneration */
    private static final String DROP_TABLE = "drop table ";

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isDbValid(AnnotationConfiguration aConfig,
            SessionFactory sessionFactory) {
        statusHandler.info("Verifying RegRep database...");
        final List<String> existingTables = new ArrayList<String>();
        List<String> definedTables = new ArrayList<String>();
        @SuppressWarnings("unchecked")
        List<String> tables = sessionFactory.getCurrentSession()
                .createSQLQuery(TABLE_CHECK_QUERY).list();
        for (String table : tables) {
            existingTables.add("ebxml." + table);
        }

        final String[] dropSqls = aConfig
                .generateDropSchemaScript(((SessionFactoryImpl) sessionFactory)
                        .getDialect());
        for (String sql : dropSqls) {
            if (sql.startsWith(DROP_TABLE)) {
                // Drop the table names to all lower case since this is the form
                // the database expects
                definedTables.add(sql.replace(DROP_TABLE, "").toLowerCase());
            }
        }

        // Check if the table set defined by Hibernate matches the table set
        // defined in the database already
        if (existingTables.size() <= definedTables.size()
                && !existingTables.containsAll(definedTables)) {
            for (String defined : definedTables) {
                if (!existingTables.contains(defined)) {
                    statusHandler.warn("MISSING TABLE: " + defined);
                }
            }
            return false;
        }
        return true;
    }

}
