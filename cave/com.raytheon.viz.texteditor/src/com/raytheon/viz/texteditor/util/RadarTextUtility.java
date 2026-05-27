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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.text.request.StdTextProductServerRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.texteditor.msgs.IRadarObserver;

/**
 * This class is a utility for the Radar Plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 8, 2009  2104       grichard    Initial creation.
 * 07/28/2009   2610       rjpeter     Moved error handling to alert viz.
 * 04/14/2010   4734       mhuang      Corrected StdTextProduct import 
 *                                      dependency
 * 05/10/2010   2187       cjeanbap    Added StdTextProductFactory 
 *                                      functionality.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class RadarTextUtility implements IRadarObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarTextUtility.class);

    @Override
    public void saveRadarTextProd(String textProd) {
        String str = ""; // working string
        String wmoId = ""; // WMO ID
        String siteId = ""; // Site ID
        String siteNode = ""; // Site node
        String category = ""; // Category
        String designator = ""; // Designator
        String currentHeader = ""; // Current Header
        StringBuilder strBldr = new StringBuilder();
        int lineNumber = 0; // initialize the line number
        String currentDate = getCurrentDate(); // Get the current date
        textProd = textProd.toUpperCase(); // Convert the text to uppercase
        BufferedReader reader = new BufferedReader(new StringReader(textProd));

        try {
            while ((str = reader.readLine()) != null) {

                // Process the one-line WMO Header
                if (lineNumber == 0) {
                    wmoId = getWmoId(str.getBytes());
                    siteId = getSiteId(str.getBytes());
                    siteNode = getCcc(str.getBytes());
                    lineNumber++;
                    continue;
                }

                // Process the one-line AWIPS ID
                if (lineNumber == 1) {
                    category = getNnn(str);
                    designator = getXxx(str);
                    lineNumber++;
                    continue;
                }

                // Collect the body of the text product
                strBldr.append(str);
                lineNumber++;
            }

            currentHeader = getHeaderTextField(wmoId, siteId, currentDate,
                    "\n", category + designator);

            // System.out.println("Current header: " + currentHeader);

            StdTextProductServerRequest request = new StdTextProductServerRequest();
            request.setWmoid(wmoId);
            request.setSite(siteId);
            request.setCccid(siteNode);
            request.setNnnid(category);
            request.setXxxid(designator);
            request.setHdrtime(currentDate);
            request.setBbbid("NOR");
            request.setCreatetime(System.currentTimeMillis());
            request.setProduct(currentHeader + "\n" + strBldr.toString());

            try {
                ThriftClient.sendRequest(request);
            } catch (VizException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving metadata", e1);
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Get the WFO's WMO ID
     * 
     * @return the WMO identifier of the WFO
     */
    private String getWmoId(byte[] bytes) {
        WMOHeader wmoHeader = new WMOHeader(bytes);
        if (wmoHeader.getWmoHeader() == null) {
            return "";
        }
        String[] wmoHeadings = wmoHeader.getWmoHeader().split(" ");
        return wmoHeadings[0];
    }

    /**
     * Get the WFO's Site ID
     * 
     * @return the site identifier of the WFO
     */
    private String getSiteId(byte[] bytes) {
        WMOHeader wmoHeader = new WMOHeader(bytes);
        if (wmoHeader.getWmoHeader() == null) {
            return "";
        }
        String[] wmoHeadings = wmoHeader.getWmoHeader().split(" ");
        return wmoHeadings[1];
    }

    /**
     * Get the AWIPS ID's category (C-C-C) part
     * 
     * @return the site node
     */
    private String getCcc(byte[] bytes) {
        WMOHeader wmoHeader = new WMOHeader(bytes);
        if (wmoHeader.getWmoHeader() == null) {
            return "";
        }
        String[] wmoHeadings = wmoHeader.getWmoHeader().split(" ");
        return wmoHeadings[1].substring(1);
    }

    /**
     * Get the AWIPS ID's category (N-N-N) part
     * 
     * @param awipsStr
     * @return the category
     */
    private String getNnn(String awipsStr) {
        return awipsStr.substring(0, 3);
    }

    /**
     * Get the AWIPS ID's designator (X-X-X) part
     * 
     * @param awipsStr
     * @return the designator
     */
    private String getXxx(String awipsStr) {
        return awipsStr.substring(3, 6);
    }

    /**
     * Get the current date
     * 
     * @return current date
     */
    private String getCurrentDate() {
        Date now = SimulatedTime.getSystemTime().getTime();
        SimpleDateFormat formatter = new SimpleDateFormat("ddHHmm");
        formatter.setTimeZone(TimeZone.getTimeZone("GMT"));
        return (formatter.format(now));
    }

    /**
     * Get the header text field
     * 
     * @param wmoId
     *            - WMO ID
     * @param siteId
     *            - Site ID
     * @param dateId
     *            - Date ID
     * @param separator
     * @param nnnxxx
     * @return header text field
     */
    private String getHeaderTextField(String wmoId, String siteId,
            String dateId, String separator, String nnnxxx) {
        return wmoId + " " + siteId + " " + dateId + separator + nnnxxx;

    }

}
