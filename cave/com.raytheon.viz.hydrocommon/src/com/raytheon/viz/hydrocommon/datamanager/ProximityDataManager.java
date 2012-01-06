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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.ProximityData;

/**
 * This class is the data manager for the proximity data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02 Dec 2008              lvenable    Initial creation
 * 
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class ProximityDataManager extends HydroDataManager
{
    /**
     * Instance of this class.
     */
    private static ProximityDataManager manager = null;
    
    /**
     * Select statement.
     */
    private final String SELECT_STATEMENT = "SELECT * FROM proximity";
    
    /**
     * Private constructor.
     */
    private ProximityDataManager()
    {        
    }
    
    /**
     * Get an instance of this class.
     * @return An instance of this class.
     */
    public static synchronized ProximityDataManager getInstance() {
        if (manager == null) {
            manager = new ProximityDataManager();
        }

        return manager;
    }
    
    /**
     * Get the proximity data.
     * @return Array of Proximity data.
     * @throws VizException Database exception.
     */
    public ArrayList<ProximityData> getProximityData() throws VizException 
    {
        ArrayList<ProximityData> rval = new ArrayList<ProximityData>();
        
        QueryResult result = runMappedQuery(SELECT_STATEMENT);

        for (QueryResultRow currRow : result.getRows()) {
            rval.add(new ProximityData(currRow, result.getColumnNames()));
        }
        
        return rval;
    }
}
