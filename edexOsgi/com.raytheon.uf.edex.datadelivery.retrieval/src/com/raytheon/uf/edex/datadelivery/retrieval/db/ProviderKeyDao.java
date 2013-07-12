package com.raytheon.uf.edex.datadelivery.retrieval.db;

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

import java.util.List;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Provider Key Dao
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2012 2180       dhladky      Provider Key storage
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ProviderKeyDao extends CoreDao {
    
    /**
     * Creates a new ProviderKeyDao
     */
    public ProviderKeyDao() {
        super(DaoConfig.forClass(ProviderKeyRecord.class));
    }

    /**
     * Retrieves a ProviderkeyRecord with the providerName
     * 
     * @param providerName
     * @return The Providerkey with the corresponding providerName
     */
    public ProviderKeyRecord queryByProvider(String providerName)
            throws DataAccessLayerException {
        List<?> providers = queryBySingleCriteria("providerName", providerName);
        if (providers.isEmpty()) {
            return null;
        } else {
            return (ProviderKeyRecord) providers.get(0);
        }
    }

    /**
     * Add or update an existing ProviderKey Record
     * 
     * @param record
     */
    public void addOrUpdateRecord(ProviderKeyRecord record) throws Exception {

        persist(record);
    }
}
