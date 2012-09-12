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

import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.texteditor.TextWarningConstants;

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
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
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
     * Grabs the site node from the FXA database afoslookup table.
     * 
     * @return The 3 letter site node, i.e. "OMA" or empty string when query
     *         does not return a result
     */
    public static String getSiteNode(String threeLetterSiteId) {
        if (threeLetterSiteId == null) {
            return "";
        }
        String fourLetterId = SiteMap.getInstance().getSite4LetterId(
                threeLetterSiteId);
        String query = "SELECT ccc FROM afoslookup WHERE origin = '"
                + fourLetterId + "';";
        String rval = "";

        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query,
                    TextWarningConstants.FXA_DB, QueryLanguage.SQL);
            for (Object[] item : results) {
                if (item[0] != null) {
                    rval = item[0].toString();
                    break;
                }
            }

        } catch (VizException e) {
            // Nothing - it doesn't matter if this fails.
        }
        return rval;
    }

    /**
     * 
     * @param afosId
     *            e.g.: "OMASVROAX"
     * @return
     */
    public static String getTtaaii(String afosId) {
        String query = "SELECT wmottaaii FROM afos_to_awips WHERE afosid = '"
                + afosId + "';";
        String rval = "blank";

        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query,
                    TextWarningConstants.FXA_DB, QueryLanguage.SQL);
            for (Object[] item : results) {
                if (item[0] != null) {
                    rval = item[0].toString();
                    break;
                }
            }

        } catch (VizException e) {
            // Nothing - it doesn't matter if this fails.
        }

        return rval;
    }

}
