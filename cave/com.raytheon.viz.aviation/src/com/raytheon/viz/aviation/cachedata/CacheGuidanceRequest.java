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

import com.raytheon.viz.aviation.guidance.GuidanceRequest;

/**
 * The base Guidance Request classed used for caching data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2011 8065       rferrel     Initial creation
 * 09Apr2014    #3005      lvenable    Added hashcode method.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CacheGuidanceRequest extends GuidanceRequest {
    protected String siteID;

    @Override
    public Map<String, Object> getPythonArguments() {
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("siteID", siteID);
        return map;
    }

    /**
     * The site associated with the request.
     * 
     * @return siteID
     */
    public String getSiteID() {
        return this.siteID;
    }

    /**
     * Set the site ID associated with the request.
     * 
     * @param siteID
     */
    public void setSiteID(String siteID) {
        this.siteID = siteID;
    }

    @Override
    public boolean equals(Object obj) {
        // Limit matches to other cache requests.
        if (obj instanceof CacheGuidanceRequest) {
            return tag.equals(((CacheGuidanceRequest) obj).tag);
        }
        return false;
    }

    @Override
    public int hashCode() {
        int result = super.hashCode();
        final int prime = 31;
        result = (prime * result) + ((siteID == null) ? 0 : siteID.hashCode());
        return result;
    }

}
