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

import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.edex.database.dao.ISessionManagedDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthDataSetUpdate;

/**
 * DAO for {@link BandwidthDataSetUpdate} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
interface IBandwidthDataSetUpdateDao extends
        ISessionManagedDao<Long, BandwidthDataSetUpdate> {

    /**
     * Get {@link BandwidthDataSetUpdate} instances by the provider and dataset
     * name.
     * 
     * @param providerName
     * @param dataSetName
     * @return
     */
    List<BandwidthDataSetUpdate> getByProviderDataSet(String providerName,
            String dataSetName);

    /**
     * Get {@link BandwidthDataSetUpdate} instances by the provider name,
     * dataset name, and base reference time.
     * 
     * @param providerName
     * @param dataSetName
     * @param baseReferenceTime
     * @return
     */
    List<BandwidthDataSetUpdate> getByProviderDataSetReferenceTime(
            String providerName, String dataSetName, Calendar baseReferenceTime);

}