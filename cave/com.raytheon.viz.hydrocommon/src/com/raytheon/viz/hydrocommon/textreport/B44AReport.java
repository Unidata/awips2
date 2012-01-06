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
package com.raytheon.viz.hydrocommon.textreport;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;

/**
 * B-44A (Cooperative) Report.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009 2260       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class B44AReport extends TextReport {
    private static final String b44aHdrCooperative = "                           U.S. DEPARTMENT OF COMMERCE           NWS-FORM B-44A";

    /* "B44A Page" Types (for printing also) */
    private static final int B44A_COOPERATIVE = 0;

    private static final int B44A_ALLPAGES = 1;

    private int b44aFirstPage = B44A_COOPERATIVE;

    private int b44aLastPage = B44A_COOPERATIVE;

    private String lid = null;

    /**
     * Constructor.
     * 
     * @param lid
     *      The location id
     */
    public B44AReport(String lid) {
        this.lid = lid;
    }

    /**
     * Get the text of the report.
     * 
     * @param page
     *      The page number to generate
     */
    @Override
    public String getText(int page) {
        String text = null;

        if (page == B44A_ALLPAGES) {
            text = getAllPages();
        } else if (page == B44A_COOPERATIVE) {
            text = b44ACooperative();
        }

        return text;
    }

    /**
     * Get the text for the dialog.
     */
    @Override
    public String loadTextWidget() {
        String text = getText(B44A_ALLPAGES);
        text = text.replace("null", "    ");
        return text;
    }

    /**
     * Build the text for the report.
     * 
     * @return
     *      The report text
     */
    private String b44ACooperative() {
        StringBuilder buffer = new StringBuilder();

        buffer.append(b44aHdrCooperative + "\n");
        buffer
                .append("                 NATIONAL OCEANIC AND ATMOSPHERIC ADMINISTRATION\n");
        buffer
                .append("                            NATIONAL WEATHER SERVICE\n\n");
        buffer
                .append("                      UNOFFICIAL COOPERATIVE STATION REPORT\n\n");
        buffer
                .append("----------------------------  IDENTIFICATION SECTION  --------------------------\n\n");

        TextReportData data = TextReportDataManager.getInstance().getData(lid);
        
        GeoUtil util = GeoUtil.getInstance();
        String lat;
        String lon;
        String date;
        String name;
        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();

        buffer.append(String.format("%12s: %-37s  %3s: %s\n", "Station Name",
                data.getLocation().getName(), "LID", lid));
        buffer.append(String.format("%12s: %-20s    %18s: %s\n", "State", data
                .getLocation().getState(), "County", data.getLocation()
                .getCounty()));

        if (data.getRiverstat().getLat() > 0) {
            lat = util.cvt_latlon_from_double(data.getRiverstat().getLat());
            lon = util.cvt_latlon_from_double(data.getRiverstat().getLon());
        } else if (data.getLocation().getLat() > 0) {
            lat = util.cvt_latlon_from_double(data.getLocation().getLat());
            lon = util.cvt_latlon_from_double(data.getLocation().getLon());
        } else {
            lat = "MISSING";
            lon = "MISSING";
        }

        buffer.append(String.format("%12s: %-20s    %18s: %s\n", "Latitude",
                lat, "Elevation", data.getLocation().getElev()));
        buffer.append(String.format("%12s: %-20s    %18s: %s\n", "Longitude",
                lon, "Hydrologic Unit No", data.getLocation().getHu()));

        date = "";
        if (data.getLocation().getSbd() != null) {
            date = sdf.format(data.getLocation().getSbd());
        }
        buffer.append(String.format(
                "Station Begin Date: %s\n       River Basin: %s\n\n", date,
                data.getLocation().getRb()));

        buffer
                .append("-------------------------------  OBSERVER SECTION  -----------------------------\n\n");

        name = "";
        if ((data.getObserver().getFirstname() != null)
                && (data.getObserver().getFirstname().length() > 0)) {
            name = data.getObserver().getFirstname() + " ";
        }

        if ((data.getObserver().getLastname() != null)
                && (data.getObserver().getLastname().length() > 0)) {
            name.concat(data.getObserver().getLastname());
        }

        String dos;
        if (data.getObserver().getDos() != null) {
            dos = sdf.format(data.getObserver().getDos());
        } else {
            dos = " ";
        }
        
        buffer.append(String.format(
                "Observer: %-41s  DOS: %-10s  Gender: %s\n", name, dos,
                        data.getObserver().getGn()));

        buffer.append(String.format(" Address: %-30s\n", data.getObserver()
                .getA1()));
        buffer.append(String.format("          %-30s      Home Phone: %s\n",
                data.getObserver().getA2(), data.getObserver().getHphone()));
        buffer.append(String.format("          %-30s    Office Phone: %s\n",
                data.getObserver().getA3(), data.getObserver().getPhone()));
        buffer.append(String.format("          %-30s       Recipient: %s\n",
                data.getObserver().getCity(), data.getObserver().getRecip()));
        buffer.append(String.format(
                "          %-2s  %-10s        %10s         Comms: %s\n", data
                        .getObserver().getState(), data.getObserver().getZip(),
                " ", data.getObserver().getComm()));
        buffer.append(String.format("   Email: %s\n", data.getObserver()
                .getEmail()));
        buffer.append(String.format("  Duties: %s\n\n", data.getObserver()
                .getRprt()));

        buffer
                .append("--------------------------  STATION MANAGEMENT SECTION  ------------------------\n\n");

        buffer.append(String.format(
                "HSA: %-3s          WFO: %-3s           RFC: %s\n\n", data
                        .getLocation().getHsa(), data.getLocation().getWfo(),
                data.getLocation().getRfc()));
        buffer
                .append("-----------------------------------  REMARKS  ----------------------------------\n\n");
        if ((data.getLocation().getLremark() != null)
                && (data.getLocation().getLremark().length() > 0)) {
            String[] wrappedLines = TextUtil.wordWrap(data.getLocation().getLremark() + "\n", 80, 0);
            for (String s: wrappedLines) {
                buffer.append(s + "\n");
            }            
        }

        /* try to place FOOTER at the bottom */
        for (int i = 0; i < 59 - 59 - countNewlines(buffer.toString()); i++) {
            buffer.append("\n");
        }

        now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();

        buffer.append(String.format(
                "   Effective Date: %-10s   Hydrologist: %s\n",
                sdf.format(now), TextReportDataManager.getInstance()
                        .getHydrologist()));

        return buffer.toString();
    }

    /**
     * Generate all pages of the report.
     * 
     * @return
     *      The full text
     */
    private String getAllPages() {
        StringBuffer buffer = new StringBuffer();
        String text = null;

        /* Get the text for the given page(s). */
        for (int i = b44aFirstPage; i <= b44aLastPage; i++) {
            text = getText(i);
            buffer.append(paginate(text));
        }

        return buffer.toString();
    }
}
