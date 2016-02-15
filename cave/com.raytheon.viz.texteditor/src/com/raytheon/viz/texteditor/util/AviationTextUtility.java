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

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.apache.commons.configuration.ConfigurationException;

import com.raytheon.uf.common.dataplugin.text.request.StdTextProductServerRequest;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.avnconfig.ITafSiteConfig;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;
import com.raytheon.viz.avnconfig.TafSiteData;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.msgs.IAviationObserver;

/**
 * This class is a utility for the Aviation Plugin.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * -----------  -------  ---------   --------------------------
 * May 15, 2008	1119        grichard    Initial creation.
 * 07/28/2009   2610        rjpeter     Moved error handling to alert viz.
 * 04/14/2010   4734        mhuang      Corrected StdTextProduct import dependency
 * 05/10/2010   2187        cjeanbap    Added StdTextProductFactory functionality.
 * 09/11/2013   2277        mschenke    Removed unused function
 * Feb 12, 2016 4860        skorolev    Added CAVE mode. Corrected setting of siteWmoId.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class AviationTextUtility implements IAviationObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AviationTextUtility.class);

    private static final String LINE_BREAK = "\n";

    /**
     * Method to save a temporary working version of a TAF bulletin to the text
     * database.
     * 
     * @param tmpStr
     *            -- the temporary working version of a TAF bulletin
     */
    @Override
    public void saveTafBulletin(String tmpStr) {
        // Convert the text to upper case
        tmpStr = tmpStr.toUpperCase();
        String currentDate = getCurrentDate();

        // Get siteId from TAF text
        String siteID = tmpStr.split(LINE_BREAK)[1].trim().substring(0, 4);

        // Set the node based on localization.
        String siteNode = LocalizationManager.getInstance().getCurrentSite();

        // Set the Site ID based on localization.
        String siteName = SiteMap.getInstance().getSite4LetterId(siteNode);
        if ((siteName == null) || (siteName.equals(""))) {
            siteName = "CCCC";
        }
        // Set the WMOID based on configuration file for current localization.
        String siteWmoId = "";
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            TafSiteData siteData = config.getSite(siteID);
            siteWmoId = siteData.wmo.split(" ")[0];
        } catch (ConfigurationException | IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        if (!siteWmoId.isEmpty()) {
            String currentHeader = getHeaderTextField(siteWmoId, siteName,
                    currentDate, "\n", "WRKTAF");

            StdTextProductServerRequest request = new StdTextProductServerRequest();
            request.setWmoid(siteWmoId);
            request.setSite(siteName);
            request.setCccid(siteNode);
            request.setNnnid("WRK");
            request.setXxxid("TAF");
            request.setHdrtime(currentDate);
            request.setBbbid("NOR");
            request.setCreatetime(TimeUtil.currentTimeMillis());
            request.setProduct(currentHeader + "\n" + tmpStr);
            request.setOperationalFlag(CAVEMode.getMode() != CAVEMode.PRACTICE);
            try {
                ThriftClient.sendRequest(request);
            } catch (VizException e1) {
                statusHandler.handle(UFStatus.Priority.ERROR,
                        "Error retrieving metadata", e1);
            }
        } else {
            statusHandler.handle(Priority.ERROR,
                    "TAF not saved to database. Unable to determine WmoId for site: "
                            + siteID);
        }
    }

    /**
     * Gets Current Date
     * 
     * @return
     */
    private String getCurrentDate() {
        Date now = SimulatedTime.getSystemTime().getTime();
        SimpleDateFormat formatter = new SimpleDateFormat("ddHHmm");
        formatter.setTimeZone(TimeZone.getTimeZone("GMT"));
        return (formatter.format(now));
    }

    /**
     * Gets Header Text Field
     * 
     * @param wmoId
     * @param siteId
     * @param dateId
     * @param separator
     * @param nnnxxx
     * @return
     */
    private String getHeaderTextField(String wmoId, String siteId,
            String dateId, String separator, String nnnxxx) {
        return wmoId + " " + siteId + " " + dateId + separator + nnnxxx;

    }

}
