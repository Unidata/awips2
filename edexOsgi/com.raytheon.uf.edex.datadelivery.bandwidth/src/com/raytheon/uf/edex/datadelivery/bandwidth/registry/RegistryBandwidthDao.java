package com.raytheon.uf.edex.datadelivery.bandwidth.registry;

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

import java.util.ArrayList;
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
 * Nov 15, 2013 1736       dhladky     Dao for registry bandwidth gathering
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class RegistryBandwidthDao extends CoreDao {
    
    private static final String FIELD = "timePeriod";
    
    private static final String GREATERTHANEQUAL = ">=";
    
    private static final String LESSTHAN = "<";
    
    /**
     * Creates a new RegistryBandwidthDao
     */
    public RegistryBandwidthDao() {
        super(DaoConfig.forClass(RegistryBandwidthRecord.class));
    }

    /**
     * Retrieves a RegistryBandwidthRecord with the timePeriod
     * All times are in seconds since 0 GMT of each day
     * returns null if none exists
     * @param startTime
     * @param endTime
     * @return The Record with the corresponding timePeriod
     */

    public RegistryBandwidthRecord queryByTimeRange(long startMillis, long endMillis)
            throws DataAccessLayerException {
        
        List<String> fields = new ArrayList<String>(2);
        List<Object> values = new ArrayList<Object>(2);
        List<String> operands  = new ArrayList<String>(2);
        fields.add(FIELD);
        values.add(startMillis);
        operands.add(GREATERTHANEQUAL);
        fields.add(FIELD);
        values.add(endMillis);
        operands.add(LESSTHAN);
        
        List<?> timePeriods = queryByCriteria(fields, values, operands, 1, FIELD, false);
        if (timePeriods.isEmpty()) {
            return null;
        } else {
            return (RegistryBandwidthRecord)timePeriods.get(0);
        }
    }
    
    /**
     * Add or update an existing RegistryBandwidthRecord
     * 
     * @param record
     */
    public void addOrUpdateRecord(RegistryBandwidthRecord record)
            throws Exception {

        // only persist records we have an active period for
        if (record.getTimePeriod() != null) {
            persist(record);
        }
    }
}
