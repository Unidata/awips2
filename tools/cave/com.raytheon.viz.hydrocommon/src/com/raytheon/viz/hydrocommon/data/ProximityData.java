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
package com.raytheon.viz.hydrocommon.data;

import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;

/**
 * This class contains the proximity data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 02 Dec 2008             lvenable    Initial creation
 * 
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class ProximityData extends HydroDBData
{
    /**
     * Proximity string.
     */
    private String proximity;
    
    /**
     * Constructor.
     */
    public ProximityData()
    {
        proximity = "";
    }
    
    /**
     * Constructor.
     * @param data Result data.
     * @param dataMap Column to Index map.
     */
    public ProximityData(QueryResultRow data, Map<String, Integer> dataMap)
    {
        setProximity(getDBValue("proximity", data, dataMap, ""));
    }
    
    /**
     * Get the proximity.
     * @return The proximity.
     */
    public String getProximity()
    {
        return proximity;
    }

    /**
     * Set the proximity.
     * @param proximity The proximity.
     */
    public void setProximity(String proximity)
    {
        this.proximity = proximity;
    }
}
