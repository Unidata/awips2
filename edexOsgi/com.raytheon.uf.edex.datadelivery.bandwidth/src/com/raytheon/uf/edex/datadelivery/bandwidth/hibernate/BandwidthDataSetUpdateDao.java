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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthDataSetUpdate;

/**
 * DAO that handles {@link BandwidthDataSetUpdate} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2013 1543       djohnson     Initial creation
 * Feb 22, 2013 1543       djohnson     Made public as YAJSW doesn't like Spring exceptions.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BandwidthDataSetUpdateDao extends
        SessionManagedDao<Long, BandwidthDataSetUpdate> implements IBandwidthDataSetUpdateDao {

    private static final String GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET = "from BandwidthDataSetUpdate d where "
            + "d.providerName = :providerName and "
            + "d.dataSetName = :dataSetName order by dataSetBaseTime desc";

    private static final String GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME = "from BandwidthDataSetUpdate d where "
            + "d.providerName = :providerName and "
            + "d.dataSetName = :dataSetName and "
            + "d.dataSetBaseTime = :dataSetBaseTime "
            + "order by dataSetBaseTime desc";

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<BandwidthDataSetUpdate> getEntityClass() {
        return BandwidthDataSetUpdate.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthDataSetUpdate> getByProviderDataSet(
            String providerName, String dataSetName) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("providerName", providerName);
        params.put("dataSetName", dataSetName);
        return query(GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET, params);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<BandwidthDataSetUpdate> getByProviderDataSetReferenceTime(
            String providerName, String dataSetName, Calendar baseReferenceTime) {
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("providerName", providerName);
        params.put("dataSetName", dataSetName);
        params.put("dataSetBaseTime", baseReferenceTime);
        return query(
                GET_DATASETMETADATA_BY_PROVIDER_AND_DATASET_AND_BASEREFERENCETIME,
                params);
    }

}
