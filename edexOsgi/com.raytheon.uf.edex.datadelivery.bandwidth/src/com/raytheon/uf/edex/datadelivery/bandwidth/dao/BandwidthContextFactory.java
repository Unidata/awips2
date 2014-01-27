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
package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import java.io.File;

import com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthMap;
import com.raytheon.uf.edex.datadelivery.bandwidth.BandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.hibernate.HibernateBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.BandwidthInitializer;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;

/**
 * Factory used to produce contextual differences in the
 * {@link IBandwidthManager}. For instance, currently the
 * {@link IBandwidthManager} can be run in three locations with slightly
 * different behavioral semantics, EDEX, In-Memory for proposed changes, and
 * integration testing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2012 1286       djohnson     Initial creation
 * Jun 24, 2013 2106       djohnson     Add {@link #getBandwidthBucketDao()}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface BandwidthContextFactory {

    /**
     * Retrieve the {@link HibernateBandwidthDbInit} implementation.
     * 
     * @return the db init.
     */
    IBandwidthDbInit getBandwidthDbInit();

    /**
     * Retrieve the {@link IBandwidthDao} implementation.
     * 
     * @return the dao
     */
    IBandwidthDao getBandwidthDao();

    /**
     * Retrieve the {@link IBandwidthBucketDao} implementation.
     * 
     * @return the dao
     */
    IBandwidthBucketDao getBandwidthBucketDao();

    /**
     * Retrieve the {@link BandwidthInitializer} implementation.
     * 
     * @return
     */
    BandwidthInitializer getBandwidthInitializer();

    /**
     * Get the configuration file for the {@link BandwidthMap}.
     * 
     * @return the configuration file reference
     */
    File getBandwidthMapConfigFile();

    /**
     * Retrieve the {@link BandwidthManager} instance.
     * 
     * @param dbInit
     *            the database init
     * @param bandwidthDao
     *            the bandwidth dao
     * @param retrievalManager
     *            the retrieval manager
     * @param bandwidthDaoUtil
     *            the dao util instance
     * @return the {@link BandwidthManager} reference
     */
    IBandwidthManager getBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil);
}
