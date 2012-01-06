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
import com.raytheon.viz.hydrocommon.data.LocationAreaData;

/**
 * This class is the data manager for the location area data.
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
public class LocationAreaManager extends HydroDataManager
{
    /**
     * Instance of this class.
     */
    private static LocationAreaManager manager = null;
    
    /**
     * Select statement.
     */
    private final String SELECT_STATEMENT = "SELECT * FROM locarea";
    
    /**
     * Insert statement.
     */
    private final String INSERT_STATEMENT = "INSERT INTO locarea (lid, area) VALUES ('%s', '%s')";
    
    /**
     * Delete statement.
     */
    private final String DELETE_STATEMENT = "DELETE FROM locarea";
    
    /**
     * Update statement.
     */
    private final String UPDATE_STATEMENT = "UPDATE locarea SET area='%s' WHERE lid='%s'";
    
    /**
     * Private constructor.
     */
    private LocationAreaManager()
    {        
    }
    
    /**
     * Get an instance of this class.
     * @return An instance of this class.
     */
    public static synchronized LocationAreaManager getInstance() {
        if (manager == null) {
            manager = new LocationAreaManager();
        }

        return manager;
    }
    
    /**
     * Get the location area data.
     * @param lid Location ID.
     * @return Array of location area data.
     * @throws VizException Database exception.
     */
    public ArrayList<LocationAreaData> getLocationAreaData(String lid) throws VizException 
    {
        ArrayList<LocationAreaData> rval = new ArrayList<LocationAreaData>();
        
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE lid='"
                + lid + "'");

        for (QueryResultRow currRow : result.getRows()) {
            rval.add(new LocationAreaData(currRow, result.getColumnNames()));
        }
        
        return rval;
    }
    
    /**
     * Delete an existing location area record.
     * @param locationID Location ID.
     * @throws VizException Database exception.
     */
    public void deleteRecord(String locationID) throws VizException
    {
        StringBuilder query = new StringBuilder(DELETE_STATEMENT);
        String whereClaus = String.format(" WHERE lid = '%s'", 
                locationID);
        query.append(whereClaus);
        
        runStatement(query.toString());
    }
    
    /**
     * Insert location area data.
     * @param data Location area data.
     * @throws VizException Database exception.
     */
    public void insertLocationAreaData(LocationAreaData data) throws VizException
    {
        String query = String.format(INSERT_STATEMENT, data.getLid(),
                data.getArea());
        
        runStatement(query);
    }
    
    /**
     * Update existing location area data.
     * @param data Location area data.
     * @throws VizException Database exception.
     */
    public void updateLocationAreaData(LocationAreaData data) throws VizException
    {        
        String query = String.format(UPDATE_STATEMENT, data.getArea(),
                data.getLid());
        
        runStatement(query);
    }
    
    /**
     * Check of a location area record exists.
     * @param lid Location ID.
     * @return True if the record exists, false otherwise.
     * @throws VizException Database exception.
     */
    public boolean recordExists(String lid) throws VizException
    {
        StringBuilder query = new StringBuilder(SELECT_STATEMENT);
        String whereClaus = String.format(" WHERE lid = '%s'", lid);
        
        query.append(whereClaus);
        
        QueryResult result = runMappedQuery(query.toString());
        
        if (result.getResultCount() == 0)
        {
            return false;
        }
        
        return true;
    }
}
