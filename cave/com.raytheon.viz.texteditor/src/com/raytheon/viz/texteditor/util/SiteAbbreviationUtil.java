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
package com.raytheon.viz.texteditor.util;

import java.util.List;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.request.GetWmoIdRequest;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * A grouping of static methods to obtain site id and to query for site
 * information in various, seldom changed, tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2009            bwoodle     Initial creation
 * Sep 09, 2012 14668      rferrel     Change default WMO ID to 'blank'.
 * Mar 23, 2016 5343       bkowal      Ensure the afos id used in the afoslookup
 *                                     query has a length of nine.
 * Aug 10, 2016 5801       tgurney     Replace afos_to_awips database query with
 *                                     an EDEX request
 * 
 * </pre>
 * 
 * @author bwoodle
 */

public class SiteAbbreviationUtil {

    private static String mySiteNode;

    /**
     * Returns the 3 letter site node for the current localization.
     * 
     * @return
     */
    public static String getMySiteNode() {
        if (mySiteNode == null) {
            mySiteNode = getSiteNode(LocalizationManager.getInstance()
                    .getCurrentSite());
        }

        return mySiteNode;
    }

    /**
     * Grabs the site node from the afos_lookup_table.dat file.
     * 
     * @return The 3 letter site node, i.e. "OMA" or empty string when query
     *         does not return a result
     */
    public static String getSiteNode(String threeLetterSiteId) {
        if (threeLetterSiteId == null) {
            return "";
        }
        SiteMap siteMap = SiteMap.getInstance();
        String fourLetterId = siteMap.getSite4LetterId(threeLetterSiteId);
        String rval = siteMap.getAFOSTableMap(fourLetterId);
        if (rval == null) {
            rval = "";
        }
        return rval;
    }

    /**
     * 
     * @param afosId
     *            e.g.: "OMASVROAX"
     * @return the ttaaii, e.g. "WUUS53", or "-" if no match was found or an
     *         error occurred.
     */
    public static String getTtaaii(String afosId) {
        String rval = "-";
        GetWmoIdRequest request = new GetWmoIdRequest();
        request.setAfosId(afosId);
        Object response = null;
        try {
            response = ThriftClient.sendRequest(request);
        } catch (VizException e) {
            // do nothing
        }
        if (response instanceof AfosWmoIdDataContainer) {
            AfosWmoIdDataContainer container = (AfosWmoIdDataContainer) response;
            List<AfosToAwips> list = container.getIdList();
            if (!list.isEmpty()) {
                rval = list.get(0).getWmottaaii();
            }
        }
        return rval;
    }
}
