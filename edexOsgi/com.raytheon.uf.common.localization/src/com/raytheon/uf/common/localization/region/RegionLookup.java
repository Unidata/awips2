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
package com.raytheon.uf.common.localization.region;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.region.RegionSites.Region;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Utility class for retrieving regions based on site information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2014            mnash       Initial creation
 * Apr 25, 2014  2060      njensen     Use JAXB instead of JAXBManager
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RegionLookup {

    private static final IUFStatusHandler handler = UFStatus
            .getHandler(RegionLookup.class);

    private static Map<String, String> wfoToRegion;

    /**
     * Don't want/need to construct this class.
     */
    private RegionLookup() {
    }

    /**
     * Returns the region based on the site that is passed in.
     * 
     * @param site
     * @return
     */
    public static synchronized String getWfoRegion(String site) {
        if (wfoToRegion == null) {
            RegionSites sites = parseFile();
            if (sites != null) {
                wfoToRegion = new HashMap<String, String>();
                for (Region region : sites.getRegion()) {
                    for (String s : region.wfo) {
                        wfoToRegion.put(s, region.regionId);
                    }
                }
            } else {
                wfoToRegion = new HashMap<String, String>();
            }
        }
        return wfoToRegion.get(site);
    }

    /**
     * Parses the regions.xml file that resides within the plugin
     * 
     * @return
     */
    private static RegionSites parseFile() {
        RegionSites sites = null;
        InputStream stream = RegionLookup.class
                .getResourceAsStream("/regions.xml");
        try {
            sites = JAXB.unmarshal(stream, RegionSites.class);
        } catch (Exception e) {
            handler.error("Unable to unmarshal regions.xml file", e);
        }

        return sites;
    }
}