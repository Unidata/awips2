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
 * This class contains the location area data.
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
public class LocationAreaData extends HydroDBData
{
    /**
     * Location ID.
     */
    private String lid;
    
    /**
     * Affected area.
     */
    private String area;
    
    /**
     * Constructor.
     */
    public LocationAreaData()
    {
        lid = "";
        area = "";
    }
    
    /**
     * Constructor.
     * @param data Result data.
     * @param dataMap Column to Index map.
     */
    public LocationAreaData(QueryResultRow data, Map<String, Integer> dataMap)
    {
        setLid(getDBValue("lid", data, dataMap, "ZZZZZZ"));
        setArea(getDBValue("area", data, dataMap, ""));
    }
    
    /**
     * Get the location ID.
     * @return The location ID.
     */
    public String getLid()
    {
        return lid;
    }
    
    /**
     * Set the location ID.
     * @param lid The location ID.
     */
    public void setLid(String lid)
    {
        this.lid = lid;
    }
    
    /**
     * Get the affected area.
     * @return The affected area.
     */
    public String getArea()
    {
        return area;
    }
    
    /**
     * Set the affected area.
     * @param area The affected area.
     */
    public void setArea(String area)
    {
        this.area = area;
    }   
}
