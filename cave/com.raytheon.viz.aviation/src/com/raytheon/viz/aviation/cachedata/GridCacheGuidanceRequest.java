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
package com.raytheon.viz.aviation.cachedata;

import java.util.HashMap;
import java.util.Map;

/**
 * Guidance request used for caching Grid data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2011 8065       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class GridCacheGuidanceRequest extends CacheGuidanceRequest {
    // TODO remove use the siteID in CacheGuidanceRequest.
    protected String siteID;

    /**
     * Build the Grid tag string for desired site.
     * 
     * @param siteID
     * @return tag
     */
    public static String getTag(String siteID) {
        return GuidanceType.GRID + ":" + siteID;
    }

    @Override
    public Map<String, Object> getPythonArguments() {
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("siteID", siteID);
        return map;
    }

    // TODO remove use the one in CacheGuidanceRequest
    public String getSiteID() {
        return this.siteID;
    }

    // TODOremove use the one in CacheGuidanceRequest
    public void setSiteID(String siteID) {
        this.siteID = siteID;
    }
}
