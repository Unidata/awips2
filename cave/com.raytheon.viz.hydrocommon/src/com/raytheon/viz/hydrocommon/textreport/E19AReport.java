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

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;

/**
 * E-19A Report.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009 2260       mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class E19AReport extends E19Report {
    private static final int E19A_SUMMARY = 0;
    private static final int E19A_ALLPAGES = 1;

    private int e19aFirstPage = E19A_SUMMARY;
    private int e19aLastPage = E19A_SUMMARY;
    
    private static final String e19AHeader = "                           U.S. DEPARTMENT OF COMMERCE           NWS-FORM E-19A";
    
    /**
     * Constructor.
     * 
     * @param lid
     *      The location id
     */
    public E19AReport (String lid) {
        super(lid);
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

        if (page == E19A_ALLPAGES) {
            text = getAllPages();
        } else if (page == E19A_SUMMARY) {
            text = getE19ASummary();
        }

        return text;
    }

    /**
     * Get the text for the dialog.
     */
    @Override
    public String loadTextWidget() {
        String text = getText(E19A_ALLPAGES);
        text = text.replace("null", "    ");
        return text;
    }
    
    /**
     * Build the text for the report.
     * 
     * @return
     *      The report text
     */
    private String getE19ASummary() {
        StringBuilder buffer = new StringBuilder();
        
        /* Get current time and convert to a YYYY value */
        Date d = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        int year = Integer.parseInt(yearFormat.format(d));
        
        TextReportData data = TextReportDataManager.getInstance().getData(lid);
        
        buffer.append(e19AHeader);
        buffer.append("\n");        
        buffer.append("                 NATIONAL OCEANIC AND ATMOSPHERIC ADMINISTRATION\n");
        buffer.append("                            NATIONAL WEATHER SERVICE\n\n");
        buffer.append("           REPORT ON RIVER GAGE STATION\n\n");
        buffer.append("------------------------------------  SITE  ------------------------------------\n\n");
        buffer.append(String.format("         LID: %-11s %10s PROXIMITY: %s\n", lid, " ", data.getDescrip().getProximity()));
        buffer.append(String.format("        NAME: %s\n      STREAM: %s\n", data.getLocation().getName(), data.getRiverstat().getStream()));
        String cs = String.format("%s %s", data.getLocation().getCounty(), data.getLocation().getState());
        buffer.append(String.format("COUNTY/STATE: %-26s BASIN: %-30s\n\n", cs, data.getLocation().getRb()));
        
        String da = null;
        if (data.getRiverstat().getDa() != HydroConstants.MISSING_VALUE) {
            da = String.format("%9.2f", data.getRiverstat().getDa());
        } else {
            da = "         ";
        }
        
        String fs = null;
        if (data.getRiverstat().getFs() != HydroConstants.MISSING_VALUE) {
            fs = String.format("%8.2f", data.getRiverstat().getFs());
        } else {
            fs = "        ";
        }
        
        buffer.append(String.format("  DRAINAGE:%s          FLOOD STAGE: %s      ", da, fs));
        buffer.append(String.format("STATION NO: %-11s\n", data.getLocation().getSn()));
        
        String mile = null;
        if (data.getRiverstat().getDa() != HydroConstants.MISSING_VALUE) {
            mile = String.format("%8.2f", data.getRiverstat().getMile());
        } else {
            mile = "        ";
        }
        
        String wstg = null;
        if (data.getRiverstat().getFs() != HydroConstants.MISSING_VALUE) {
            wstg = String.format("%8.2f", data.getRiverstat().getWstg());
        } else {
            wstg = "        ";
        }
        
        buffer.append(String.format("RIVER MILE: %s         ACTION STAGE: %s      ", mile, wstg));
        buffer.append(String.format("   USGS NO: %-11s\n", data.getRiverstat().getGsno()));

        String zd = null;
        if (data.getRiverstat().getDa() != HydroConstants.MISSING_VALUE) {
            zd = String.format("%8.3f", data.getRiverstat().getZd());
        } else {
            zd = "        ";
        }
        
        String bf = null;
        if (data.getRiverstat().getFs() != HydroConstants.MISSING_VALUE) {
            bf = String.format("%8.2f", data.getRiverstat().getBf());
        } else {
            bf = "        ";
        }

        buffer.append(String.format("ZERO DATUM: %s       BANKFULL STAGE: %s      ", zd, bf));
        buffer.append(String.format("   NESS ID: %-8s\n", data.getDcp().getGoes()));
        
        String cb = null;
        if (data.getRiverstat().getDa() != HydroConstants.MISSING_VALUE) {
            cb = String.format("%8.3f", data.getRiverstat().getCb());
        } else {
            cb = "        ";
        }
        
        String pool = null;
        if (data.getRiverstat().getFs() != HydroConstants.MISSING_VALUE) {
            pool = String.format("%8.2f", data.getRiverstat().getBf());
        } else {
            pool = "        ";
        }

        buffer.append(String.format("  CHECKBAR: %s          NORMAL POOL: %s      ", cb, pool));
        buffer.append(String.format("       RFC: %-6s\n", data.getLocation().getRfc()));
        
        String lat;
        if (data.getRiverstat().getLat() > 0) {
            lat = GeoUtil.getInstance().cvt_latlon_from_double(data.getRiverstat().getLat());
        } else {
            lat = "          ";
        }
        
        buffer.append(String.format("  LATITUDE: %-10s      TIDAL EFFECTS: %-9s     ", lat, data.getRiverstat().getTide()));
        buffer.append(String.format("       HSA: %-4s\n", data.getLocation().getHsa()));
        
        String lon;
        if (data.getRiverstat().getLon() > 0) {
            lon = GeoUtil.getInstance().cvt_latlon_from_double(data.getRiverstat().getLon());
        } else {
            lon = "          ";
        }
        
        String value = " ";
        if (data.getFloodCat().getMajorStage() != HydroConstants.MISSING_VALUE) {
            value = String.valueOf(data.getFloodCat().getMajorStage());
        }
        buffer.append(String.format(" LONGITUDE: %-10s          FLOODCATS:    MAJOR: %s\n", lon, value));
        
        if (data.getFloodCat().getModerateStage() != HydroConstants.MISSING_VALUE) {
            value = String.valueOf(data.getFloodCat().getModerateStage());
        } else {
            value = " ";
        }
        buffer.append(String.format(" %30s            MODERATE: %s\n", " ", value));
        
        if (data.getFloodCat().getMinorStage() != HydroConstants.MISSING_VALUE) {
            value = String.valueOf(data.getFloodCat().getMinorStage());
        } else {
            value = " ";
        }
        buffer.append(String.format("       %-10s %-21s       MINOR: %s\n", " ", " ", value));
        buffer.append(String.format("PERIOD OF RECORD: %-31s\n\n", data.getRiverstat().getPor()));
        
        buffer.append("----------------------------------  OBSERVER  ----------------------------------\n\n");
        
        String name = "";
        if ((data.getObserver().getFirstname() != null)
                && (data.getObserver().getFirstname().length() > 0)) {
            name = data.getObserver().getFirstname() + " ";
        }

        if ((data.getObserver().getLastname() != null)
                && (data.getObserver().getLastname().length() > 0)) {
            name.concat(data.getObserver().getLastname());
        }

        buffer.append(String.format("  %-40s\n", name));
        
        String dos = null;
        if (data.getObserver().getDos() != null) {
            dos = sdf.format(data.getObserver().getDos());
        } else {
            dos = " ";
        }
        buffer.append(String.format("  %-30s  SERVICE DATE: %-10s    SPONSOR: %-8s\n", data.getObserver().getA1(), dos, data.getObserver().getSponsor()));
        
        if (data.getObserver().getRate() != HydroConstants.MISSING_VALUE) {
            buffer.append(String.format("  %-30s        CD-404: %-9s         RATE: $%7.2f\n", data.getObserver().getA2(), data.getObserver().getOrnr(), data.getObserver().getRate()));
        } else {
            buffer.append(String.format("  %-30s        CD-404: %-9s         RATE: $\n", data.getObserver().getA2(), data.getObserver().getOrnr()));
        }
        buffer.append(String.format("  %-30s    HOME PHONE: %-18s\n", data.getObserver().getA3(), data.getObserver().getHphone()));
        buffer.append(String.format("  %-30s    WORK PHONE: %-18s\n", " ", data.getObserver().getPhone()));
        buffer.append(String.format("%-43s\n\n", String.format("  %s  %s  %s", data.getObserver().getCity(), data.getObserver().getState(), data.getObserver().getZip())));
        buffer.append(String.format("    EMAIL: %s\n", data.getObserver().getEmail()));
        buffer.append(String.format("   DUTIES: %s\n", data.getObserver().getRprt()));
        buffer.append(String.format("RECIPIENT: %-16s   COMMS TYPE: %-11s      TASK: %-14s\n\n", data.getObserver().getRecip(), data.getObserver().getComm(), data.getObserver().getTsk()));
        
        buffer.append("------------------------------------  GAGES  -----------------------------------\n\n");
        
        buffer.append(String.format("TELEM TYPE: %-11s%5sTELEM OWNER: %-11s     PHONE: %-13s\n",
                data.getTelem().getType(), " ", data.getTelem().getOwner(), data.getTelem().getPhone()));
        
        buffer.append(String.format("    DCP ID: %-8s%10sDCP OWNER: %s\n\n", data.getDcp().getGoes(), " ", data.getDcp().getOwner()));
        buffer.append("     LATEST GAGE TYPE           START DATE              OWNER OF GAGE\n");
        if (data.getGage().getBegin() != null) {
            buffer.append(String.format("       %-11s                %10s              %-11s\n", data.getGage().getType(), sdf.format(data.getGage().getBegin()), data.getGage().getOwner()));
        }
        
        buffer.append("\n");
        
        buffer.append("-----------------------------------  CRESTS  -----------------------------------\n\n");
        buffer.append(String.format("              %30s LEVEL       DATE\n", " "));
        
        buffer.append(String.format("    HIGHEST BASED ON GAGE READING:         %s\n", getHighestCrest(E19_GAGE_READING_CREST, null)));
        
        buffer.append(String.format(" HIGHEST BASED ON HIGH WATERMARKS:         %s\n", getHighestCrest(E19_HIGH_WATERMARKS_CREST, null)));
        
        String tenYrsAgo = "1/01/" + (year - 10);
        buffer.append(String.format("         HIGHEST SINCE %10s:         %s\n", tenYrsAgo, getHighestCrest(E19_TIME_CREST, tenYrsAgo)));
        
        String currentYear = "1/01/" + year;
        buffer.append(String.format("         HIGHEST SINCE %10s:         %s\n", currentYear, getHighestCrest(E19_TIME_CREST, currentYear)));
        
        buffer.append("-----------------------------------  REMARKS  ----------------------------------\n\n");
        
        if ((data.getRiverstat().getRemark() != null)
                && (data.getRiverstat().getRemark().length() > 0)) {
            String[] wrappedLines = TextUtil.wordWrap(data.getRiverstat().getRemark() + "\n", 80, 0);
            for (String s: wrappedLines) {
                buffer.append(s + "\n");
            }            
        }
        buffer.append("\n\n\n");
        
        /* try to place FOOTER at the bottom */
        for (int i = 0; i < 59 - 59 - countNewlines(buffer.toString()); i++) {
            buffer.append("\n");
        }

        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        String reviseDate = " ";
        
        if (data.getRiverstat().getRrevise() != null) {
            reviseDate = sdf.format(data.getRiverstat().getRrevise());
        }
        
        buffer.append(String.format(
                "HYDROLOGIST: %-32s  REVISED, PRINTED DATES: %-10s, %-10s\n", TextReportDataManager.getInstance().getHydrologist(), reviseDate, sdf.format(now)));
        
        return buffer.toString();
    }
    
    /**
     * Get the full text of the report.
     * 
     * @return
     *      The full report text
     */
    private String getAllPages() {
        StringBuilder buffer = new StringBuilder();
        String text = null;
        
        /* Get the text for the given page(s). */
        for (int i = e19aFirstPage; i <= e19aLastPage; i++) {
            text = getText(i);
            buffer.append(paginate(text));
        }
        
        return buffer.toString();
    }
}
