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
import com.raytheon.viz.hydrocommon.data.DescriptionData;

/**
 * This class is the data manager for the description data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02 Dec 2008              lvenable    Initial creation
 * Sep 03, 2015 4846        rjpeter     List out columns in select.
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class DescriptionDataManager extends HydroDataManager
{
    /**
     * Instance of this class.
     */
    private static DescriptionDataManager manager = null;
    
    /**
     * Select statement.
     */
    private final String SELECT_STATEMENT = "SELECT lid, bed, divert, remark, ice, proximity, reach, res, topo FROM descrip";
    
    /**
     * Insert statement.
     */
    private final String INSERT_STATEMENT = 
        "INSERT INTO descrip (lid, bed, divert, remark, ice, proximity, reach, res, topo) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')";
    
    /**
     * Delete statement.
     */
    private final String DELETE_STATEMENT = "DELETE FROM descrip";
    
    /**
     * Update statement.
     */
    private final String UPDATE_STATEMENT =
        "UPDATE descrip SET bed='%s', divert='%s', remark='%s', ice='%s', proximity='%s', reach='%s', res='%s', topo='%s' WHERE lid='%s'";
    
    /**
     * Private Constructor.
     */
    private DescriptionDataManager()
    {        
    }
    
    /**
     * Get an instance of this class.
     * @return An instance of this class.
     */
    public static synchronized DescriptionDataManager getInstance() {
        if (manager == null) {
            manager = new DescriptionDataManager();
        }

        return manager;
    }
    
    /**
     * Get the description data.
     * @param lid Location ID.
     * @return An array of description data.
     * @throws VizException Database exception.
     */
    public ArrayList<DescriptionData> getDescriptionData(String lid) throws VizException 
    {
        ArrayList<DescriptionData> rval = new ArrayList<DescriptionData>();
        
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE lid='"
                + lid + "'");

        for (QueryResultRow currRow : result.getRows()) {
            rval.add(new DescriptionData(currRow, result.getColumnNames()));
        }
        
        return rval;
    }
    
    /**
     * Delete a specified description.
     * @param lid Location ID.
     * @throws VizException Database exception.
     */
    public void deleteDescription(String lid) throws VizException
    {
        StringBuilder query = new StringBuilder(DELETE_STATEMENT);
        String whereClaus = String.format(" WHERE lid = '%s'", lid);
        query.append(whereClaus);
        
        runStatement(query.toString());
    }
    
    /**
     * Insert description data.
     * @param descData Description data.
     * @throws VizException Database exception.
     */
    public void insertDescriptionData(DescriptionData descData) throws VizException
    {
        String query = String.format(INSERT_STATEMENT, descData.getLid(),
                descData.getStreamBed(), descData.getDivert(), descData.getRemark(),
                descData.getIce(), descData.getProximity(), descData.getReach(),
                descData.getRegulation(), descData.getTopo());
        
        runStatement(query);
    }
    
    /**
     * Update description data.
     * @param descData Description data.
     * @throws VizException Database exception.
     */
    public void updateDescriptionData(DescriptionData descData) throws VizException
    {
        String query = String.format(UPDATE_STATEMENT, 
                descData.getStreamBed(), descData.getDivert(), descData.getRemark(),
                descData.getIce(), descData.getProximity(), descData.getReach(),
                descData.getRegulation(), descData.getTopo(), descData.getLid());
        
        runStatement(query);
    }
    
    /**
     * Check if a record exists.
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
