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
package com.raytheon.uf.edex.registry.federation;

import org.hibernate.cfg.Configuration;

import com.raytheon.uf.edex.database.init.DbInit;

/**
 * <pre>
 * 
 * Creates the database tables necessary for federation replication to function
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 19, 2014  2769     bphillip  Initial Creation
 * Oct 16, 2014  3454     bphillip  Upgrading to Hibernate 4
 * May 11, 2015  4448     bphillip  Separated EBXML Registry from Data Delivery
 * May 11, 2016  5638     tjensen   Added ReplicationRegistry to hibernate
 * Aug 05, 2016  5810     tjensen   Added ReplicationSiteEvent and removed
 *                                  ReplicationRegistry from hibernate
 * Aug 18, 2016  5810     tjensen   Update Table check query
 * 
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class FederationDbInit extends DbInit {

    /** Query to check which tables exist in the ebxml database */
    private static final String TABLE_CHECK_QUERY = "SELECT tablename FROM pg_tables where schemaname = 'awips' and "
            + "(tablename='registry_replication_events' or tablename='registry_replication_site_events');";

    protected FederationDbInit() {
        super("Data Delivery Federation");
    }

    @Override
    protected String getTableCheckQuery() {
        return TABLE_CHECK_QUERY;
    }

    @Override
    protected Configuration getConfiguration() {
        /*
         * Create a new configuration object which holds all the classes that
         * this Hibernate SessionFactory is aware of
         */
        Configuration aConfig = new Configuration();
        aConfig.addAnnotatedClass(ReplicationEvent.class);
        aConfig.addAnnotatedClass(ReplicationSiteEvent.class);
        return aConfig;
    }
}
