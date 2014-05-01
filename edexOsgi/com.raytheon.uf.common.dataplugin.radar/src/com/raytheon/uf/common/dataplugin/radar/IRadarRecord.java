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
package com.raytheon.uf.common.dataplugin.radar;

import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2009            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public interface IRadarRecord {

    /**
     * Gets all the storm ids for the specific radar record
     * 
     * @return
     */
    public List<String> getStormIDList();

    /**
     * Gets the message specific values like averages and constants
     * 
     * @param valueName
     *            - the key for the value type that you want to get back
     * @return
     */
    public String getProductVals(RadarConstants.MapValues type, String id,
            RadarConstants.MapValues valueName);

    /**
     * Gets the packet specific values for each storm
     * 
     * @param stormID
     *            - stormID for the storm that you want to retrieve information
     *            for
     * @param valueName
     *            - the key for the value type that you want to get back
     * @return
     */
    public int getPacketVals(String stormID, String valueName);

}
