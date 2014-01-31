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

package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import org.hibernate.cfg.AnnotationConfiguration;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.edex.database.init.DbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrievalAttributes;

/**
 * The DbInit class is responsible for ensuring that the appropriate tables are
 * present in the bandwidth manager database implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 31, 2012 726         jspinks     Copied and refactored from ebxml registry DbInit
 * Oct 26, 2012 1286        djohnson    Renamed to Hibernate specific.
 * Apr 30, 2013 1960        djohnson    Extend the generalized DbInit.
 * Jun 24, 2013 2106        djohnson    Add {@link BandwidthBucket} to annotated classes.
 * Jul 11, 2013 2106        djohnson    Add {@link SubscriptionRetrievalAttributes}.
 * Jan 08, 2013 2645        bgonzale    Added RegistryBandwidthRecord to configuration annotated class list.
 * </pre>
 * 
 * @author jspinks
 * @version 1
 */
@Transactional
@Service
public class HibernateBandwidthDbInit extends DbInit implements
        IBandwidthDbInit {

    /**
     * Creates a new instance of DbInit. This constructor should only be called
     * once when loaded by the Spring container.
     * 
     * @param bandwidthDao
     *            the dao to use
     * 
     */
    public HibernateBandwidthDbInit() {
        super("bandwidth manager");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected AnnotationConfiguration getAnnotationConfiguration() {
        /*
         * Create a new configuration object which holds all the classes that
         * this Hibernate SessionFactory is aware of
         */
        AnnotationConfiguration aConfig = new AnnotationConfiguration();
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthDataSetUpdate.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrievalAttributes.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.registry.RegistryBandwidthRecord.class);
        return aConfig;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init() throws Exception {
        initDb();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getTableCheckQuery() {
        // This intentionally returns a query that will force the tables to be
        // recreated
        return "SELECT 'alwaysRecreateTables'";
    }
}
