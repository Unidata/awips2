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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Benchmark;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Contacts;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Crest;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Datum;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Flood;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Gage;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.LowWater;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Pub;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;

/**
 * E-19 Report.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009 2260       mpduff     Initial creation
 * Apr 25, 2012 14499      wkwock     Refine format, query, etc
 * Oct 14, 2012 15454      wkwock     Fix can not generate e19 if damage filed is empty
 * Jan 23, 2013 15443      lbousaidi  added NP form feed foe all the pages.
 * Mar  8, 2017 17119      jdeng      E-19 history, gages with end date not listed
 * 10/24/2017   DR19052    qzhu       Unable to pull up text reports for certain stations
 * Jun 20, 2018 #6764      dgilling   Fix layout of the cover page.
 * Jun 27, 2018 #6876      dgilling   Fix layout of multi-page sections.
 * Aug 05, 2019 #6876      bsteffen   Fix missing benchmark and repeating gage remarks.
 * </pre>
 *
 * @author mpduff
 */

public class E19Report extends TextReport {

    /*
     * "E19 Page" Types (for printing also)
     */

    protected static final int E19_COVER = 0;

    protected static final int E19_MAPPAGE = 1;

    protected static final int E19_BENCHMARKS = 2;

    protected static final int E19_GAGES = 3;

    protected static final int E19_HISTORY = 4;

    protected static final int E19_CRESTS = 5;

    protected static final int E19_LOWWATER = 6;

    protected static final int E19_CONDITIONS = 7;

    protected static final int E19_DAMAGE = 8;

    protected static final int E19_STAFFGAGE = 9;

    protected static final int E19_CONTACTS = 10;

    protected static final int E19_ALLPAGES = 11;

    /*
     * "Highest Crest" Types
     */

    protected static final int E19_GAGE_READING_CREST = 0;

    protected static final int E19_HIGH_WATERMARKS_CREST = 1;

    protected static final int E19_TIME_CREST = 2;

    /*
     * for E19_CreateFooter ()
     */
    protected static final int E19_STANDARD_LEFT_MARGIN = 10;

    /*
     * to use info->loc->lrevise
     */
    protected static final int E19_LREVISE_TYPE = 0;

    /*
     * to use info->river->rrevise
     */
    protected static final int E19_RREVISE_TYPE = 1;

    private static final int E19_FIRST_PAGE = E19_COVER;

    private static final int E19_LAST_PAGE = E19_CONTACTS;

    private static final char SEP_CHAR = 124;

    private boolean initFooter = true;

    private int prevPageType = -1;

    private int subpageNum = 0;

    private TextReportData locData = null;

    /**
     * Constructor.
     *
     * @param lid
     *            The location id
     */
    public E19Report(String lid) {
        this.lid = lid;
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Get the text of the report.
     *
     * @param page
     *            The page number to generate
     */
    @Override
    public String getText(int page) {
        String text = null;
        switch (page) {
        case E19_ALLPAGES:
            text = E19AllPages();
            break;
        case E19_COVER:
            text = E19Cover();
            break;
        case E19_MAPPAGE:
            text = E19MapPage();
            break;
        case E19_BENCHMARKS:
            text = E19Benchmarks();
            break;
        case E19_GAGES:
            text = E19Gages();
            break;
        case E19_HISTORY:
            text = E19History();
            break;
        case E19_CRESTS:
            text = E19Crests();
            break;
        case E19_LOWWATER:
            text = E19LowWater();
            break;
        case E19_CONDITIONS:
            text = E19Conditions();
            break;
        case E19_DAMAGE:
            text = E19Damage();
            break;
        case E19_STAFFGAGE:
            text = E19StaffGage();
            break;
        case E19_CONTACTS:
            text = E19Contacts();
            break;
        }

        return text;
    }

    /**
     * Get the text for the dialog.
     */
    @Override
    public String loadTextWidget() {
        /* Get the complete E19 text and display it. */
        String text = getText(E19_ALLPAGES);
        text = text.replace("null", "").replace("\f", "\n\n\n");

        return text;
    }

    /**
     * Generate all pages.
     *
     * @return The text for all pages
     */
    private String E19AllPages() {
        StringBuilder allText = new StringBuilder();
        String text = null;
        locData = TextReportDataManager.getInstance().getLocationData(lid);
        /* Get the text for the given page(s). */
        for (int i = E19_FIRST_PAGE; i <= E19_LAST_PAGE; i++) {
            text = getText(i);
            allText.append(text);
        }

        return allText.toString();
    }

    /**
     * Generate the cover page
     *
     * @return The cover page text
     */
    private String E19Cover() {
        StringBuilder buffer = new StringBuilder();

        TextReportData data = TextReportDataManager.getInstance()
                .getDataForReports(lid, 0);
        buffer.append(TextReportConstants.E19_HDR_COVER);
        buffer.append("\n\n");
        buffer.append("                         U.S. DEPARTMENT OF COMMERCE\n");
        buffer.append("                 NATIONAL OCEANIC AND ATMOSPHERIC ");
        buffer.append("ADMINISTRATION\n");
        buffer.append(
                "                            NATIONAL WEATHER SERVICE\n\n");
        buffer.append(
                "                          REPORT ON RIVER GAGE STATION\n\n");

        String revisedDate = " ";
        if (data.getRiverstat().getRrevise() != null) {
            revisedDate = sdf.format(data.getRiverstat().getRrevise());
        }

        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        String printedDate = sdf.format(now);

        buffer.append(String.format("%40s %10s, %10s\n\n\n",
                "REVISED, PRINTED DATES:", revisedDate, printedDate));

        buffer.append(String.format("LOCATION: %s\n  STREAM: %s\n",
                locData.getLocation().getName(),
                data.getRiverstat().getStream()));
        buffer.append(String.format("   BASIN: %-30s   HSA: %s\n",
                locData.getLocation().getRb(), locData.getLocation().getHsa()));
        buffer.append("\n");

        buffer.append("REFERENCES:\n");
        TextReportData dataRefer = TextReportDataManager.getInstance()
                .getReferenceData(lid);
        int count = 0;
        if (dataRefer.getRefer() != null) {
            count = dataRefer.getRefer().size();
        }

        if (count > 0) {
            for (String s : dataRefer.getRefer()) {
                buffer.append(String.format("      %s\n", s));
            }
        }

        // try to place ABBREVIATIONS at the bottom
        for (int i = 0; i < 16 - count; i++) {
            buffer.append("\n");
        }

        buffer.append("\nABBREVIATIONS:\n\n");
        buffer.append(
                "  BM  - bench mark             EPA   - Environmental Protection Agency\n");
        buffer.append(
                "  DS  - downstream             IBWC  - International Boundary and Water Comm.\n");
        buffer.append(
                "  US  - upstream               MSRC  - Mississippi River Commission\n");
        buffer.append(
                "  HW  - high water             MORC  - Missouri River Commission\n");
        buffer.append(
                "  LW  - low water              NOAA  - National Oceanic and Atmospheric Admin.\n");
        buffer.append(
                "  RB  - right bank             NOS   - National Ocean Survey\n");
        buffer.append(
                "  LB  - left bank              NWS   - National Weather Service\n");
        buffer.append(
                "  MGL - mean gulf level        TVA   - Tennessee Valley Authority\n");
        buffer.append(
                "  MLW - mean low water         USACE - U.S. Army Corps of Engineers\n");
        buffer.append(
                "  MSL - mean sea level         USBR  - U.S. Bureau of Reclamation\n");
        buffer.append(
                "  MLT - mean low tide          USGS  - U.S. Geological Survey\n");
        buffer.append(
                "  MT  - mean tide              USWB  - U.S. Weather Bureau\n");
        buffer.append(
                "  WQ  - water quality          NGVD  - National Geodetic Vertical Datum\n");
        buffer.append(
                "  RM  - reference mark         NAD   - North American Datum\n");
        buffer.append("  RP  - reference point\n");
        buffer.append("\n\n\n");

        buffer.append(String.format(
                "                                 LOCATION IDENTIFICATION: %s\n",
                lid));
        buffer.append(
                String.format(
                        "                                        NWS INDEX NUMBER: %s\n",
                        locData.getLocation().getSn()));
        buffer.append(
                String.format(
                        "                                             USGS NUMBER: %s\n",
                        data.getRiverstat().getGsno()));
        buffer.append('\f');

        return buffer.toString();
    }

    /**
     * Get the map page
     *
     * @return The map page text
     */
    private String E19MapPage() {
        StringBuilder buffer = new StringBuilder();

        TextReportData data = TextReportDataManager.getInstance()
                .getDataForReports(lid, 0);
        buffer.append(TextReportConstants.E19_HDR_MAPPAGE);
        buffer.append("\n\n");

        buffer.append(
                String.format("%20s: %-10s             SOURCE: %s\n",
                        "LATITUDE",
                        GeoUtil.getInstance().cvt_latlon_from_double(
                                data.getRiverstat().getLat()),
                        data.getRiverstat().getRsource()));
        buffer.append(
                String.format("%20s: %-10s", "LONGITUDE", GeoUtil.getInstance()
                        .cvt_latlon_from_double(data.getRiverstat().getLon())));

        // try to place FOOTER at the bottom
        buffer.append(advanceToFooter(0, buffer.toString()));

        buffer.append(createFooter(data, E19_RREVISE_TYPE, getDate(),
                "NWS FORM E-19", E19_MAPPAGE, "GAGE MAP", null,
                E19_STANDARD_LEFT_MARGIN)).append('\f');

        return buffer.toString();
    }

    /**
     * Get the benchmark page
     *
     * @return The benchmark page text
     */

    private String E19Benchmarks() {
        StringBuilder buffer = new StringBuilder(
                TextReportConstants.E19_HDR_BENCHMARKS).append("\n\n");

        TextReportData dataRivStat = TextReportDataManager.getInstance()
                .getDataForReports(lid, 0);
        String tmp3 = "        ";
        if (dataRivStat.getRiverstat()
                .getZd() != HydroConstants.MISSING_VALUE) {
            tmp3 = String.format("%8.3f", dataRivStat.getRiverstat().getZd());
        }

        String tmp1 = String.format(
                "       ELEVATION OF GAGE ZERO: %s %7s VERTICAL DATUM: %-21s\n",
                tmp3, " ", dataRivStat.getRiverstat().getVdatum());

        if (dataRivStat.getRiverstat()
                .getCb() != HydroConstants.MISSING_VALUE) {
            tmp3 = String.format("%8.3f", dataRivStat.getRiverstat().getCb());
        } else {
            tmp3 = "        ";
        }

        String tmp2 = String.format(
                "     LEVELING AGENCY AND DATE: %-21s  CHECKBAR: %s\n",
                dataRivStat.getRiverstat().getLevel(), tmp3);

        tmp3 = String.format("                RATING AGENCY: %-21s\n\n",
                dataRivStat.getRiverstat().getRated());
        buffer.append(tmp1).append(tmp2).append(tmp3);

        int count1 = countNewlines(buffer.toString());

        tmp2 = String.format(
                "          BENCHMARK %4sDESCRIPTION%63s    GAGE ZERO      DATUM\n",
                " ", " ");
        tmp3 = String.format(
                "          ----------%4s%74s    ---------     -------\n",
                "----",
                "--------------------------------------------------------------------------");
        buffer.append(tmp2).append(tmp3);

        int count2 = countNewlines(tmp2) + countNewlines(tmp3);

        int available = getLinesPerPage() - count1 - count2 - 5;
        int avail = available;

        TextReportData data = TextReportDataManager.getInstance()
                .getDataForReports(lid, 1);
        List<Benchmark> benchList = data.getBenchmark();

        int numCols = 74;
        int leftMargin = 24;
        int loop = 0;
        for (Benchmark bm : benchList) {
            String[] remark = {};
            if (bm.getRemark() != null) {
                remark = TextUtil.wordWrap(bm.getRemark(), numCols, leftMargin);
            } else {
                continue;
            }

            int needed = remark.length + 2;

            if (needed > avail) {
                // try to place FOOTER at the bottom
                buffer.append(advanceToFooter(loop, buffer.toString()));

                // Do footer.
                String footer = createFooter(dataRivStat, E19_RREVISE_TYPE,
                        getDate(), "NWS FORM E-19", E19_BENCHMARKS,
                        "BENCHMARKS", null, E19_STANDARD_LEFT_MARGIN);
                buffer.append(footer).append('\f');

                // Do column header.
                tmp2 = String.format(
                        "          BENCHMARK %4sDESCRIPTION%63s    GAGE ZERO      DATUM\n",
                        " ", " ");
                tmp3 = String.format(
                        "          ----------%4s%74s    ---------     -------\n",
                        "----",
                        "--------------------------------------------------------------------------");
                buffer.append(tmp2).append(tmp3);

                avail = available + count1;
                loop++;
            }

            // Formatting for Line 1.
            tmp1 = StringUtils.EMPTY;
            if (!CollectionUtil.isNullOrEmpty(remark)) {
                tmp1 = remark[0].trim();
            }

            tmp3 = "        ";
            if ((bm.getElev() != HydroConstants.MISSING_VALUE) && (dataRivStat
                    .getRiverstat().getZd() != HydroConstants.MISSING_VALUE)) {
                tmp3 = String.format("%8.3f",
                        (bm.getElev() - dataRivStat.getRiverstat().getZd()));
            }

            String tmp4 = "        ";
            if (bm.getElev() != HydroConstants.MISSING_VALUE) {
                tmp4 = String.format("%8.3f", bm.getElev());
            }
            tmp2 = String.format("          %-7s       %-74s     %s    %s\n",
                    bm.getBnum(), tmp1, tmp3, tmp4);
            buffer.append(tmp2);

            // Formatting for all additional lines.
            for (int i = 1; i < remark.length; i++) {
                buffer.append(remark[i]).append('\n');
            }
            buffer.append('\n');

            avail -= needed;
        }
        buffer.append('\n');

        // try to place FOOTER at the bottom
        buffer.append(advanceToFooter(0, buffer.toString()));

        String footer = createFooter(dataRivStat, E19_RREVISE_TYPE, getDate(),
                "NWS FORM E-19", E19_BENCHMARKS, "BENCHMARKS", null,
                E19_STANDARD_LEFT_MARGIN);
        buffer.append(footer).append('\f');

        return buffer.toString();
    }

    /**
     * Get the gages page
     *
     * @return The gages page text
     */
    private String E19Gages() {
        StringBuilder buffer = new StringBuilder(
                TextReportConstants.E19_HDR_GAGES).append("\n\n").append(
                        "                 DCP                                        TELEM\n\n");

        TextReportData data = TextReportDataManager.getInstance()
                .getDataForReports(lid, 2);
        String tmp1 = String.format(
                "        NESS ID: %-9s %12s  TYPE OF TELEMETRY: %s\n",
                data.getDcp().getGoes(), " ", data.getTelem().getType());
        String tmp2 = String.format(
                "          OWNER: %-11s %12s            OWNER: %s\n",
                data.getDcp().getOwner(), " ", data.getTelem().getOwner());
        String tmp3 = String.format(
                "    REPORT TIME: %-9s %12s       PHONE NUMBER: %s\n",
                data.getDcp().getRptime(), " ", data.getTelem().getPhone());
        String tmp4 = String.format(
                "       INTERVAL: %-5s %12s               INTERVAL: %s\n",
                data.getDcp().getRptfreq(), " ", data.getTelem().getRptFreq());
        buffer.append(tmp1).append(tmp2).append(tmp3).append(tmp4);

        // Create the first criteria
        int dcpNumCols = 20;
        int dcpLeftMargin = 0;

        String[] crit1 = {};
        if (data.getDcp().getCriteria() != null) {
            crit1 = TextUtil.wordWrap(data.getDcp().getCriteria(), dcpNumCols,
                    dcpLeftMargin);
        }

        // Create the second criteria
        int telmNumCols = 15;
        int telmLeftMargin = 0;

        String[] crit2 = {};
        if (data.getTelem().getCriteria() != null) {
            crit2 = TextUtil.wordWrap(data.getTelem().getCriteria(),
                    telmNumCols, telmLeftMargin);
        }

        // formatting for line 1
        if (!CollectionUtil.isNullOrEmpty(crit1)) {
            String tmp6 = String.format("       CRITERIA: %-20s %12s", crit1[0],
                    " ");
            buffer.append(tmp6);
        }

        if (!CollectionUtil.isNullOrEmpty(crit2)) {
            String tmp6 = String.format("CRITERIA: %-20s\n", crit2[0]);
            buffer.append(tmp6);
        }

        // Formatting for all additional lines.
        for (int i = 1; i < Math.max(crit1.length, crit2.length); i++) {
            if (i < crit1.length) {
                String tmp6 = String.format("                 %-20s %12s",
                        crit1[i], " ");
                buffer.append(tmp6);
            }

            if (i < crit2.length) {
                String tmp6 = String.format("          %-20s", crit2[i], " ");
                buffer.append(tmp6).append('\n');
            }
        }

        String tmp5 = "       ";
        if (data.getTelem().getCost() > 0) {
            tmp5 = String.format("%-7.2f", data.getTelem().getCost());
        }

        buffer.append(String.format(
                "                 %18s     PAYOR/COST OF LINE: %s / $ %s\n\n",
                " ", data.getTelem().getPayor(), tmp5));

        int count1 = countNewlines(buffer.toString());

        // Do column header.
        buffer.append(
                "          GAGE TYPE   OWNER       MAINTENANCE BEGAN      ENDED     ")
                .append(" GAGE LOCATION/REMARKS\n")
                .append("          ----------- ----------- ----------- ---------- ----------")
                .append(" --------------------------------------------------------")
                .append("----\n");

        int count2 = countNewlines(buffer.toString()) - count1;

        int available = getLinesPerPage() - count1 - count2 - 5;
        int avail = available;

        TextReportData dataGage = TextReportDataManager.getInstance()
                .getGageQueryData(lid);
        List<Gage> gageList = dataGage.getGageList();

        int numCols = 60;
        int leftMargin = 68;
        int loop = 0;
        for (Gage gage : gageList) {
            String[] remark = TextUtil.wordWrap(gage.getRemark(), numCols,
                    leftMargin);
            int needed = remark.length + 2;

            if (needed > avail) {
                // try to place FOOTER at the bottom
                buffer.append(advanceToFooter(loop, buffer.toString()));

                String footer = createFooter(dataGage, E19_RREVISE_TYPE,
                        getDate(), "NWS FORM E-19", E19_GAGES, "GAGES", null,
                        E19_STANDARD_LEFT_MARGIN);
                buffer.append(footer).append('\f');

                // Do column header.
                buffer.append(
                        "          GAGE TYPE   OWNER       MAINTENANCE BEGAN      ENDED     ")
                        .append(" GAGE LOCATION/REMARKS\n")
                        .append("          ----------- ----------- ----------- ---------- ----------")
                        .append(" --------------------------------------------------------")
                        .append("----\n");

                avail = available + count1;
                loop++;
            }

            // Formatting for line 1
            tmp1 = StringUtils.EMPTY;
            if (!CollectionUtil.isNullOrEmpty(remark)) {
                tmp1 = remark[0].trim();
            }

            tmp3 = " ";
            if (gage.getBegin() != null) {

                tmp3 = sdf.format(gage.getBegin());
            }
            tmp4 = " ";
            if (gage.getEnd() != null) {
                tmp4 = sdf.format(gage.getEnd());

            }

            tmp2 = String.format(
                    "          %-11s %-11s %-11s %10s %10s %-23s\n",
                    gage.getType(), gage.getOwner(), gage.getMaint(), tmp3,
                    tmp4, tmp1);
            buffer.append(tmp2);

            // Formatting for all additional lines
            for (int i = 1; i < remark.length; i++) {
                buffer.append(remark[i]).append('\n');
            }

            avail -= needed;
        }
        buffer.append('\n');

        // try to place FOOTER at the bottom
        buffer.append(advanceToFooter(loop, buffer.toString()));
        String footer = createFooter(dataGage, E19_RREVISE_TYPE, getDate(),
                "NWS FORM E-19", E19_GAGES, "GAGES", null,
                E19_STANDARD_LEFT_MARGIN);

        buffer.append(footer).append('\f');

        return buffer.toString();
    }

    /**
     * Get the history page
     *
     * @return The history page text
     */
    private String E19History() {
        StringBuilder buffer = new StringBuilder(
                TextReportConstants.E19_HDR_HISTORY).append("\n\n");

        int count1 = countNewlines(buffer.toString());

        buffer.append(
                "      PUBLICATION/LOCATION OF RECORDS       STARTING DATE    ")
                .append("ENDING DATE\n")
                .append("      -------------------------------       -------------    ")
                .append("-----------\n");

        int count2 = countNewlines(buffer.toString()) - count1;

        int available = getLinesPerPage() - count1 - count2 - 5;
        int avail = available;

        TextReportData data = TextReportDataManager.getInstance()
                .getDataForReports(lid, 3);
        int loop = 0;
        for (Pub pub : data.getPubList()) {
            String tmp2 = sdf.format(pub.getBegin());
            String tmp3 = " ";
            if (pub.getEnd() != null) {
                tmp3 = sdf.format(pub.getEnd());
            }
            String tmp1 = String.format("      %-25s      %7s%10s       %10s\n",
                    pub.getPpub(), " ", tmp2, tmp3);
            int needed = 1;

            if (needed > avail) {
                // try to place FOOTER at the bottom
                buffer.append(advanceToFooter(loop, buffer.toString()));

                // Do footer
                String footer = createFooter(data, E19_RREVISE_TYPE,
                        getDate(), "NWS FORM E-19", E19_HISTORY, "HISTORY",
                        null, E19_STANDARD_LEFT_MARGIN);

                buffer.append(footer).append('\f');

                // Do column header
                buffer.append(
                        "      PUBLICATION/LOCATION OF RECORDS       STARTING DATE    ")
                        .append("ENDING DATE\n")
                        .append("      -------------------------------       -------------    ")
                        .append("-----------\n");

                avail = available + count1;
                loop++;
            }

            buffer.append(tmp1);
            avail = avail - needed;
        }
        buffer.append("\n\n");

        buffer.append(
                "      TYPE OF GAGE              OWNER       STARTING DATE    ")
                .append("ENDING DATE\n")
                .append("      ------------              -----       -------------    ")
                .append("-----------\n");

        available = getLinesPerPage() - count1 - count2 - 5;
        avail = available;

        loop = 0;
        TextReportData dataGage = TextReportDataManager.getInstance()
                .getGageQueryData(lid);
        List<Gage> gageList = dataGage.getGageList();
        for (Gage gage : gageList) {
            String tmp2 = sdf.format(gage.getBegin());
            String tmp3 = " ";
            if (gage.getEnd() != null) {
                tmp3 = sdf.format(gage.getEnd());
            }

            String tmp1 = String.format(
                    "      %-11s %14s%-11s %10s       %10s\n", gage.getType(),
                    " ", gage.getOwner(), tmp2, tmp3);
            int needed = 1;

            if (needed > avail) {
                // try to place FOOTER at bottom
                buffer.append(advanceToFooter(loop, buffer.toString()));

                // do footer
                String footer = createFooter(dataGage, E19_RREVISE_TYPE,
                        getDate(), "NWS FORM E-19", E19_HISTORY, "HISTORY",
                        null, E19_STANDARD_LEFT_MARGIN);

                buffer.append(footer).append('\f');

                // Do column header
                buffer.append(
                        "      TYPE OF GAGE              OWNER       STARTING DATE    ")
                        .append("ENDING DATE\n")
                        .append("      ------------              -----       -------------    ")
                        .append("-----------\n");

                avail = available + count1;
                loop++;
            }

            buffer.append(tmp1);
            avail -= needed;
        }
        buffer.append("\n\n");


        buffer.append(
                "      ZERO ELEVATION                        STARTING DATE\n")
                .append("      --------------                        -------------\n");

        available = getLinesPerPage() - count1 - count2 - 5;
        avail = available;
        loop = 0;

        for (Datum datum : data.getDatumList()) {
            String tmp2 = "        ";
            if (datum.getElevation() != -999) {
                tmp2 = String.format("%8.3f", datum.getElevation());
            }
            String tmp3 = sdf.format(datum.getDate());

            String tmp1 = String.format("      %s      %24s%10s\n", tmp2, " ",
                    tmp3);

            int needed = 1;

            if (needed > avail) {
                // try to place FOOTER at the bottom
                buffer.append(advanceToFooter(loop, buffer.toString()));

                // Do footer.
                String footer = createFooter(dataGage, E19_RREVISE_TYPE,
                        getDate(), "NWS FORM E-19", E19_HISTORY, "HISTORY",
                        null, E19_STANDARD_LEFT_MARGIN);

                buffer.append(footer).append('\f');

                // Do column header.
                buffer.append(
                        "      ZERO ELEVATION                        STARTING DATE\n")
                        .append("      --------------                        -------------\n");

                avail = available + count1;
            }

            buffer.append(tmp1);
            avail -= needed;
        }
        buffer.append("\n\n");

        // try to place FOOTER at the bottom
        buffer.append(advanceToFooter(loop, buffer.toString()));

        // Do footer.
        String footer = createFooter(dataGage, E19_RREVISE_TYPE,
                getDate(), "NWS FORM E-19", E19_HISTORY, "HISTORY",
                null, E19_STANDARD_LEFT_MARGIN);

        buffer.append(footer).append('\f');

        return buffer.toString();
    }

    /**
     * Get the crests page
     *
     * @return The crests page text
     */
    private String E19Crests() {
        int numCols = 50;
        int leftMargin = 73;

        StringBuilder buffer = new StringBuilder(
                TextReportConstants.E19_HDR_CRESTS).append("\n\n");

        TextReportData data = TextReportDataManager.getInstance()
                .getDataForReports(lid, 0);
        String tmp0 = StringUtils.EMPTY;
        String tmp2 = StringUtils.EMPTY;
        String tmp3 = StringUtils.EMPTY;
        String tmp4 = StringUtils.EMPTY;
        String tmp5 = StringUtils.EMPTY;
        if (data.getRiverstat() != null) {
            if (data.getRiverstat().getFs() != HydroConstants.MISSING_VALUE) {
                tmp2 = String.format("%-6.2f", data.getRiverstat().getFs());
            } else {
                tmp2 = "      ";
            }
            if (data.getRiverstat().getWstg() != HydroConstants.MISSING_VALUE) {
                tmp3 = String.format("%-6.2f", data.getRiverstat().getWstg());
            } else {
                tmp3 = "      ";
            }
            if (data.getRiverstat().getBf() != HydroConstants.MISSING_VALUE) {
                tmp4 = String.format("%-6.2f", data.getRiverstat().getBf());
            } else {
                tmp4 = "      ";
            }

            if (data.getRiverstat().getFq() != HydroConstants.MISSING_VALUE) {
                tmp5 = String.format("%-8.0f", data.getRiverstat().getFq());
            } else {
                tmp5 = "      ";
            }
            if (data.getRiverstat()
                    .getActionFlow() != HydroConstants.MISSING_VALUE) {
                tmp0 = String.format("%-8.0f",
                        data.getRiverstat().getActionFlow());
            } else {
                tmp0 = "      ";
            }
        } else {
            tmp0 = "      ";
            tmp2 = "      ";
            tmp3 = "      ";
            tmp4 = "      ";
            tmp5 = "      ";
        }

        String tmp1 = String.format(
                "      FLOOD STAGE: %s   ACTION STAGE: %s    BANKFULL STAGE: %s\n",
                tmp2, tmp3, tmp4);
        tmp2 = String.format("       FLOOD FLOW: %s    ACTION FLOW: %s\n\n",
                tmp5, tmp0);
        buffer = buffer.append(tmp1).append(tmp2);

        int count1 = countNewlines(buffer.toString());

        buffer = buffer
                .append("          DATE OF    TIME   CREST  FLOW   FROM HIGH  BASED ON  ")
                .append("CAUSED BY\n")
                .append("          CREST      LST    (ft)   (CFS)  WATERMARKS OLD DATUM ")
                .append("ICE JAM   REMARKS\n")
                .append("          ---------- ------ ------ ------ ---------- --------- ")
                .append("--------- --------------------------------------------------\n");

        int count2 = countNewlines(buffer.toString()) - count1;

        int available = getLinesPerPage() - count1 - count2 - 5;

        int avail = available - 2;
        int loop = 0;

        List<Crest> crestList = Collections.emptyList();
        TextReportData dataCrest = TextReportDataManager.getInstance()
                .getCrestData(lid);
        if (!CollectionUtil.isNullOrEmpty(dataCrest.getCrestList())) {
            crestList = dataCrest.getCrestList();
        }

        for (Crest crest : crestList) {
            String[] remark = TextUtil.wordWrap(crest.getCremark(), numCols,
                    leftMargin);
            int needed = remark.length + 1;

            if (needed > avail) {
                // try to place FOOTER at the bottom
                buffer.append(advanceToFooter(loop, buffer.toString()));

                // Do footer.
                String footer = createFooter(dataCrest, E19_RREVISE_TYPE,
                        getDate(), "NWS FORM E-19", E19_CRESTS, "CRESTS",
                        null, E19_STANDARD_LEFT_MARGIN);
                buffer.append(footer).append('\f');

                // Do column header.
                buffer.append("\n\n")
                        .append("          DATE OF    TIME   CREST  FLOW   FROM HIGH  BASED ON  ")
                        .append("CAUSED BY\n")
                        .append("          CREST      LST    (ft)   (CFS)  WATERMARKS OLD DATUM ")
                        .append("ICE JAM   REMARKS\n")
                        .append("          ---------- ------ ------ ------ ---------- --------- ")
                        .append("--------- --------------------------------------------------\n");

                avail = available + count1;
                loop++;
            }

            // Formatting for Line 1.
            tmp1 = (!CollectionUtil.isNullOrEmpty(remark)) ? remark[0].trim()
                    : " ";

            if (crest.getDatcrst() != null) {
                tmp3 = sdf.format(crest.getDatcrst());
            } else {
                tmp3 = " ";
            }

            if (crest.getStage() != HydroConstants.MISSING_VALUE) {
                tmp4 = String.format("%6.2f", crest.getStage());
            } else {
                tmp4 = "      ";
            }

            if (crest.getQ() != HydroConstants.MISSING_VALUE) {
                tmp5 = String.format("%6d", crest.getQ());
            } else {
                tmp5 = "      ";
            }

            String tmp6 = " ";
            if (crest.getHw() != null) {
                tmp6 = crest.getHw();
            }
            String tmp7 = " ";
            if (crest.getOldDatum() != null) {
                tmp7 = crest.getOldDatum();
            }
            String tmp8 = " ";
            if (crest.getJam() != null) {
                tmp8 = crest.getJam();
            }

            tmp2 = String.format(
                    "          %10s %-6s %s %s     %s %7s  %s %7s %s     %-17s\n",
                    tmp3, crest.getTimcrst(), tmp4, tmp5, tmp6, " ", tmp7, " ",
                    tmp8, tmp1);
            buffer.append(tmp2);

            // Formatting for all additional lines.
            if (remark != null) {
                for (int i = 1; i < remark.length; i++) {
                    if ((i == remark.length - 1)
                            && (remark[i].trim().isEmpty())) {
                        continue;
                    }

                    buffer.append(remark[i]).append('\n');
                }
            }

            avail -= needed;
        }

        // try to place FOOTER at the bottom
        buffer.append(advanceToFooter(loop, buffer.toString()));
        String footer = createFooter(dataCrest, E19_RREVISE_TYPE,
                getDate(), "NWS FORM E-19", E19_CRESTS, "CRESTS",
                null, E19_STANDARD_LEFT_MARGIN);
        buffer.append(footer).append('\f');

        return buffer.toString();
    }

    /**
     * Get the low water page
     *
     * @return The low water page text
     */
    private String E19LowWater() {
        StringBuilder buffer = new StringBuilder(
                TextReportConstants.E19_HDR_LOWWATER).append("\n\n");

        int count1 = countNewlines(buffer.toString());

        buffer = buffer.append("     DATE OF     STAGE    FLOW\n")
                .append("     LOW WATER   (ft)     (CFS)   REMARKS\n")
                .append("     ----------  -------  ------  -------------------------")
                .append("--------------------\n");

        int count2 = countNewlines(buffer.toString()) - count1;

        int available = getLinesPerPage() - count1 - count2 - 5;
        int avail = available;

        TextReportData data = TextReportDataManager.getInstance()
                .getDataForReports(lid, 4);
        List<LowWater> lowWaterList = Collections.emptyList();
        if (!CollectionUtil.isNullOrEmpty(data.getLowWaterList())) {
            lowWaterList = data.getLowWaterList();
        }

        int numCols = 45;
        int leftMargin = 34;
        int loop = 0;
        for (LowWater lowWater : lowWaterList) {
            String[] remark = TextUtil.wordWrap(lowWater.getRemarks(),
                    numCols,
                    leftMargin);
            int needed = remark.length + 1;

            if (needed > avail) {
                // try to place FOOTER at the bottom
                buffer.append(advanceToFooter(loop, buffer.toString()));

                // Do footer.
                String footer = createFooter(data, E19_RREVISE_TYPE,
                        getDate(), "NWS FORM E-19", E19_LOWWATER,
                        "LOW WATER", null, E19_STANDARD_LEFT_MARGIN);
                buffer.append(footer).append('\f');

                // Do column header.
                buffer.append("     DATE OF     STAGE    FLOW\n")
                        .append("     LOW WATER   (ft)     (CFS)   REMARKS\n")
                        .append("     ----------  -------  ------  -------------------------")
                        .append("--------------------\n");

                avail = available + count1;
                loop++;
            }

            // Formatting for Line 1.
            String tmp1 = StringUtils.EMPTY;
            if (!CollectionUtil.isNullOrEmpty(remark)) {
                tmp1 = remark[0].trim();
            }

            String tmp3 = StringUtils.EMPTY;
            if (lowWater.getDate() != null) {
                tmp3 = sdf.format(lowWater.getDate());
            }

            String tmp4 = "       ";
            if (lowWater.getStage() != HydroConstants.MISSING_VALUE) {
                tmp4 = String.format("%7.2f", lowWater.getStage());
            }

            String tmp5 = "      ";
            if (lowWater.getQ() != HydroConstants.MISSING_VALUE) {
                tmp5 = String.format("%6d", lowWater.getQ());
            }

            String tmp2 = String.format("     %10s  %s  %s  %-45s\n", tmp3,
                    tmp4, tmp5, tmp1);
            buffer.append(tmp2);

            // Formatting for all additional lines.
            if (remark != null) {
                for (int i = 1; i < remark.length; i++) {
                    if (!remark[i].trim().isEmpty()) {
                        buffer.append(remark[i]).append('\n');
                    }
                }
            }

            avail -= needed;
        }
        buffer.append("\n\n");

        // try to place FOOTER at the bottom
        buffer.append(advanceToFooter(loop, buffer.toString()));

        String footer = createFooter(data, E19_RREVISE_TYPE, getDate(),
                "NWS FORM E-19", E19_LOWWATER, "LOW WATER", null,
                E19_STANDARD_LEFT_MARGIN);
        buffer.append(footer).append('\f');

        return buffer.toString();
    }

    /**
     * Get the conditions page
     *
     * @return The conditions page text
     */
    private String E19Conditions() {
        StringBuilder buffer = new StringBuilder();
        int numCols = 60;
        int leftMargin = 17;
        String tmp1 = "      ";
        String tmp2 = "      ";
        String tmp3 = "      ";
        String indent = "";
        for (int i = 0; i < leftMargin; i++) {
            indent = indent.concat(" ");
        }
        buffer.append(TextReportConstants.E19_HDR_CONDITIONS + "\n\n");

        TextReportData data = TextReportDataManager.getInstance()
                .getDataForReports(lid, 0);

        if (data.getRiverstat().getMile() != HydroConstants.MISSING_VALUE) {
            tmp1 = String.format("%-6.1f", data.getRiverstat().getMile());
        }
        if (data.getRiverstat().getDa() != HydroConstants.MISSING_VALUE) {
            tmp2 = String.format("%-6.1f", data.getRiverstat().getDa());
        }
        if (data.getRiverstat().getPool() != HydroConstants.MISSING_VALUE) {
            tmp3 = String.format("%-6.1f", data.getRiverstat().getPool());
        }

        buffer.append(String.format(
                "     MILES ABOVE MOUTH: %s%sDRAINAGE AREA: %s%sPOOL STAGE: %s",
                tmp1, "     ", tmp2, "     ", tmp3));
        buffer.append("\n\n\n");

        buffer.append("     STREAM BED: ");

        if (data.getDescrip().getBed() != null) {
            String[] lines = TextUtil.wordWrap(data.getDescrip().getBed(),
                    numCols, 0);
            buffer.append(lines[0] + "\n");
            for (int i = 1; i < lines.length; i++) {
                if ((i != (lines.length - 1))
                        && (!lines[i].trim().isEmpty())) {
                    buffer.append("                 " + lines[i] + "\n");
                }
            }
        }
        buffer.append("\n");

        buffer.append("          REACH: ");
        if (data.getDescrip().getReach() != null) {
            String[] lines = TextUtil.wordWrap(data.getDescrip().getReach(),
                    numCols, 0);
            buffer.append(lines[0] + "\n");
            for (int i = 1; i < lines.length; i++) {
                if ((i != (lines.length - 1))
                        && (!lines[i].trim().isEmpty())) {
                    buffer.append("                 " + lines[i] + "\n");
                }
            }
        } else {
            buffer.append("\n");
        }
        buffer.append("\n");

        buffer.append("     REGULATION: ");
        if (data.getDescrip().getRes() != null) {
            String[] lines = TextUtil.wordWrap(data.getDescrip().getRes(),
                    numCols, 0);
            buffer.append(lines[0] + "\n");
            for (int i = 1; i < lines.length; i++) {
                if ((i != (lines.length - 1))
                        && (!lines[i].trim().isEmpty())) {
                    buffer.append("                 " + lines[i] + "\n");
                }
            }
        } else {
            buffer.append("\n");
        }
        buffer.append("\n");

        buffer.append("      DIVERSION: ");
        if (data.getDescrip().getDivert() != null) {
            String[] lines = TextUtil.wordWrap(data.getDescrip().getDivert(),
                    numCols, 0);
            buffer.append(lines[0] + "\n");
            for (int i = 1; i < lines.length; i++) {
                if ((i != (lines.length - 1))
                        && (!lines[i].trim().isEmpty())) {
                    buffer.append("                 " + lines[i] + "\n");
                }
            }
        } else {
            buffer.append("\n");
        }
        buffer.append("\n");

        buffer.append("         WINTER: ");
        if (data.getDescrip().getIce() != null) {
            String[] lines = TextUtil.wordWrap(data.getDescrip().getIce(),
                    numCols, 0);
            buffer.append(lines[0] + "\n");
            for (int i = 1; i < lines.length; i++) {
                if ((i != (lines.length - 1))
                        && (!lines[i].trim().isEmpty())) {
                    buffer.append("                 " + lines[i] + "\n");
                }
            }
        } else {
            buffer.append("\n");
        }
        buffer.append("\n");

        buffer.append("     TOPOGRAPHY: ");
        if (data.getDescrip().getTopo() != null) {
            String[] lines = TextUtil.wordWrap(data.getDescrip().getTopo(),
                    numCols, 0);
            buffer.append(lines[0] + "\n");
            for (int i = 1; i < lines.length; i++) {
                if ((i != (lines.length - 1))
                        && (!lines[i].trim().isEmpty())) {
                    buffer.append("                 " + lines[i] + "\n");
                }
            }
        } else {
            buffer.append("\n");
        }
        buffer.append("\n");

        buffer.append("        REMARKS: ");
        if (data.getDescrip().getRemark() != null) {
            String[] lines = TextUtil.wordWrap(data.getDescrip().getRemark(),
                    numCols, 0);
            buffer.append(lines[0] + "\n");
            for (int i = 1; i < lines.length; i++) {
                if ((i != (lines.length - 1))
                        && (!lines[i].trim().isEmpty())) {
                    buffer.append("                 " + lines[i] + "\n");
                }
            }
        } else {
            buffer.append("\n");
        }
        buffer.append("\n");

        // try to place FOOTER at the bottom
        buffer.append(advanceToFooter(0, buffer.toString()));

        Date d = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        String footer = createFooter(data, E19_RREVISE_TYPE, sdf.format(d),
                "NWS FORM E-19", E19_CONDITIONS, "CONDITIONS", null,
                E19_STANDARD_LEFT_MARGIN);
        buffer.append(footer).append('\f');

        return buffer.toString();
    }

    /**
     * Get the damage page
     *
     * @return The map damage text
     */
    private String E19Damage() {
        StringBuilder buffer = new StringBuilder(
                TextReportConstants.E19_HDR_DAMAGE).append("\n\n");

        int count1 = countNewlines(buffer.toString());

        buffer = buffer.append("              STAGE    AREAS AFFECTED\n")
                .append("              -------  ---------------------------------------------")
                .append("-------------------------------------------------------\n");

        int count2 = countNewlines(buffer.toString()) - count1;

        int available = getLinesPerPage() - count1 - count2 - 5;
        int avail = available;

        List<Flood> floodList = Collections.emptyList();
        TextReportData data = TextReportDataManager.getInstance()
                .getCrestData(lid);
        if (!CollectionUtil.isNullOrEmpty(data.getFloodList())) {
            floodList = data.getFloodList();
        }

        int loop = 0;
        int numCols = 100;
        int leftMargin = 23;
        for (Flood flood : floodList) {
            String tmp2 = "       ";
            if (flood.getStage() != HydroConstants.MISSING_VALUE) {
                tmp2 = String.format("%7.2f", flood.getStage());
            }
            StringBuilder tmp1 = new StringBuilder();
            if (flood.getDamage() != null) {
                String[] lines = TextUtil.wordWrap(flood.getDamage(), numCols,
                        leftMargin);
                for (int i = 0; i < lines.length; i++) {
                    String s = lines[i];

                    if (i == 0) {
                        tmp1.append(String.format("              %s  %s\n",
                                tmp2, s.trim()));
                    } else if ((i == lines.length - 1)
                            && (s.trim().isEmpty())) {
                        continue;
                    } else {
                        tmp1.append(s).append('\n');
                    }
                }
                tmp1.append('\n');
            } else {
                tmp1.append(String.format("              %s  %s\n\n", tmp2,
                        StringUtils.EMPTY));
            }
            int needed = countNewlines(tmp1.toString());

            if (needed > avail) {
                // try to place FOOTER at the bottom
                buffer.append(advanceToFooter(loop, buffer.toString()));

                String footer = createFooter(data, E19_RREVISE_TYPE,
                        getDate(), "NWS FORM E-19", E19_DAMAGE, "DAMAGE",
                        null, E19_STANDARD_LEFT_MARGIN);
                buffer.append(footer).append('\f');

                // Do column header.
                buffer.append("              STAGE    AREAS AFFECTED\n")
                        .append("              -------  ---------------------------------------------")
                        .append("-------------------------------------------------------\n");

                avail = available + count1;
                loop++;
            }

            buffer.append(tmp1);
        }
        buffer.append("\n\n");

        // try to place FOOTER at the bottom
        buffer.append(advanceToFooter(loop, buffer.toString()));

        String footer = createFooter(data, E19_RREVISE_TYPE, getDate(),
                "NWS FORM E-19", E19_DAMAGE, "DAMAGE", null,
                E19_STANDARD_LEFT_MARGIN);
        buffer.append(footer).append('\f');

        return buffer.toString();
    }

    /**
     * Get the staff gage page
     *
     * @return The map staff gage text
     */
    private String E19StaffGage() {
        StringBuilder buffer = new StringBuilder();
        StringBuilder record = new StringBuilder();
        // width of flood text
        int numCols = 85;
        int leftMargin = 12;
        String tmp1 = "      ";
        String tmp2 = "      ";
        String tmp3 = "      ";

        // so flood section is space-buffered
        int flood_filler = 100;
        // so crest section is space-buffered
        int crest_filler = 20;

        // ensures correct num of lines/page
        int linesAvailable = 0;
        // ensures lines will fit on page
        int linesNeeded = 0;

        // maximum of flood and crest stage
        int maxStage = Integer.MIN_VALUE;
        // minimum of flood and crese stage
        int minStage = Integer.MAX_VALUE;
        int scaleDiff = 0;
        // maxstage - minstage
        double range;
        // (range / lines_per_page)
        double feetStagePerLine = 0.0;

        double currentStage;

        // max printable lines per page
        int lines_per_page = 51;
        // distance between tickmarks
        int tickmark_gap = 5;
        // loop control variable (for tickmarks)
        int currentTick = 0;

        String indent = "";
        for (int i = 0; i < leftMargin; i++) {
            indent = indent.concat(" ");
        }
        buffer.append(TextReportConstants.E19_HDR_STAFFGAGE + "\n\n");

        TextReportData data = TextReportDataManager.getInstance()
                .getStaffGageData(lid);

        int count1 = countNewlines(buffer.toString());

        linesAvailable = getLinesPerPage() - count1 - 7;

        ArrayList<Flood> floodList = null;
        if (data.getStaffData().getFloodList() != null) {
            floodList = data.getStaffData().getFloodList();
        }
        ArrayList<Crest> crestList = null;
        if (data.getStaffData().getCrestList() != null) {
            crestList = data.getStaffData().getCrestList();
        }

        if ((crestList != null && !crestList.isEmpty())
                || (floodList != null && !floodList.isEmpty())) {
            /*
             * find the min and max stage values found in the Flood and Crest
             * tables
             */
            if (floodList != null) {
                for (Flood flood : floodList) {
                    if (flood.getStage() < minStage) {
                        minStage = (int) flood.getStage();
                    }
                    if (flood.getStage() > maxStage) {
                        maxStage = (int) flood.getStage();
                    }
                }
            }
            if (crestList != null) {
                for (Crest crest : crestList) {
                    if (crest.getStage() < minStage) {
                        minStage = (int) crest.getStage();
                    }
                    if (crest.getStage() > maxStage) {
                        maxStage = (int) crest.getStage();
                    }
                }
            }

            maxStage++;

            // determine the range & feetStagePerLine (feet per line)
            range = maxStage - minStage;
            feetStagePerLine = (range / lines_per_page);

            if (feetStagePerLine < 0.2) {
                feetStagePerLine = 0.2;
            } else if (feetStagePerLine < 0.4) {
                feetStagePerLine = 0.4;
            } else if (feetStagePerLine < 1.0) {
                feetStagePerLine = 1.0;
            } else if (feetStagePerLine < 2.0) {
                feetStagePerLine = 2.0;
            } else if (feetStagePerLine < 5.0) {
                feetStagePerLine = 5.0;
            }

            // set the maxstage based on the minstage + lines per page and
            // feet/line
            scaleDiff = (int) (((lines_per_page - 1) * feetStagePerLine)
                    - range);

            maxStage += scaleDiff / 2;

            if (floodList != null) {
                // Compute the number of lines needed for first flood record.
                if ((!floodList.isEmpty()) && (floodList.get(0)
                        .getStage() != HydroConstants.MISSING_VALUE)) {
                    tmp1 = String.format("%7.2f", floodList.get(0).getStage());
                    String[] lines = TextUtil
                            .wordWrap(floodList.get(0).getDamage(), numCols, 0);
                    record.append(indent + tmp1);

                    if (lines != null && lines.length > 0) {
                        record.append(" - " + lines[0]);

                        for (int i = 1; i < lines.length; i++) {
                            if (lines[i].length() > 1) {
                                record.append(indent + lines[i]);
                            }
                        }
                    }

                    linesNeeded = countNewlines(record.toString());
                }
            }

            // currentStage is decremented by feetStagePerLine
            currentStage = maxStage;
            int floodIndex = 0;
            int crestIndex = 0;
            int lineIndex = 0;
            String[] lines = null;
            String thisLineStr = null;

            for (currentTick = 0; currentTick < lines_per_page; currentTick++) {
                if (linesNeeded <= linesAvailable) {
                    /*
                     * List the flood events
                     */
                    if ((floodList.size() > floodIndex)
                            && (floodList.get(floodIndex) != null)
                            && (currentStage <= floodList.get(floodIndex)
                                    .getStage())) {
                        // get one line at a time from record
                        if (lines == null) {
                            lines = TextUtil.wordWrap(
                                    floodList.get(floodIndex).getDamage(),
                                    numCols, 0);
                            lineIndex = 0;

                            tmp1 = "       ";
                            if (floodList.get(floodIndex)
                                    .getStage() != HydroConstants.MISSING_VALUE) {
                                tmp1 = String.format("%7.2f",
                                        floodList.get(floodIndex).getStage());
                            }

                            if (lines == null || lines[lineIndex] == null) {
                                thisLineStr = "  " + tmp1 + " -";
                            } else {
                                thisLineStr = "  " + tmp1 + " - "
                                        + lines[lineIndex];
                            }
                        } else {
                            thisLineStr = "            ";
                        }

                        int spaces = flood_filler - thisLineStr.length();
                        record.setLength(0);

                        // Fill in the beginning of the line with spaces
                        StringBuilder remainSpace = new StringBuilder();
                        for (int i = 0; i < spaces; i++) {
                            remainSpace.append(" ");
                        }
                        buffer.append(thisLineStr);
                        buffer.append(remainSpace.toString());

                        lineIndex++;

                        // No more lines in this record
                        if (lines != null && lineIndex == lines.length - 1) {
                            // Get the next record
                            floodIndex++;

                            if ((floodList.size() > floodIndex)
                                    && (floodList.get(floodIndex) != null)) {

                                // Compute the number of lines needed for new
                                // flood record.
                                if (floodList.get(floodIndex)
                                        .getStage() != HydroConstants.MISSING_VALUE) {
                                    tmp1 = String.format("%7.2f", floodList
                                            .get(floodIndex).getStage());
                                    record.append(" " + tmp1 + " - " + floodList
                                            .get(floodIndex).getDamage());
                                    lines = TextUtil.wordWrap(record.toString(),
                                            numCols, 0);
                                    linesNeeded = countNewlines(
                                            record.toString());
                                }
                            }

                            lines = null;
                            lineIndex = 0;
                        }
                    } else {
                        // no flood information on this line
                        for (int i = 0; i < flood_filler; i++) {
                            buffer.append(" ");
                        }
                    }

                    /*
                     * Draw the gage
                     */
                    if ((currentTick % tickmark_gap) == 0) {
                        // then show gage with tickmark
                        String s = e19BuildStaffGageString(
                                String.format("%4.0f", currentStage));
                        buffer.append(s);
                    } else {
                        // then show gage with delete/rubout characters
                        String s = e19BuildStaffGageString(null);
                        buffer.append(s);
                    }

                    /*
                     * List the crest events
                     */
                    if ((crestList.size() > crestIndex)
                            && (crestList.get(crestIndex) != null)
                            && (currentStage <= crestList.get(crestIndex)
                                    .getStage())) {
                        tmp2 = String.format("%8.2f",
                                crestList.get(crestIndex).getStage());
                        if (crestList.get(crestIndex).getDatcrst() != null) {
                            tmp3 = sdf.format(
                                    crestList.get(crestIndex).getDatcrst());
                        }
                        buffer.append(String.format("%s  %s", tmp2, tmp3));

                        while ((crestList.size() > crestIndex)
                                && (crestList.get(crestIndex) != null)
                                && (crestList.get(crestIndex)
                                        .getStage() >= currentStage)) {
                            crestIndex++;
                        }
                    } else {
                        // no crest information on this line
                        for (int i = 0; i < crest_filler; i++) {
                            buffer.append(" ");
                        }
                    }

                    /*
                     * End of current line
                     */
                    buffer.append("\n");
                    currentStage -= feetStagePerLine;
                    linesAvailable--;
                }
            }
        }

        buffer.append(advanceToFooter(0, buffer.toString()));

        data = TextReportDataManager.getInstance().getDataForReports(lid, 0);
        String reach = (data.getDescrip() != null)
                ? data.getDescrip().getReach() : StringUtils.EMPTY;
        double elevation = (data.getRiverstat() != null)
                ? data.getRiverstat().getZd() : 0;
        tmp2 = String.format("REACH: %-80s  ELEVATION ZERO: %7.2f\n\n", reach,
                elevation);

        Date d = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        String footer = createFooter(data, E19_RREVISE_TYPE, sdf.format(d),
                "NWS FORM E-19", E19_STAFFGAGE, "STAFF", tmp2,
                E19_STANDARD_LEFT_MARGIN);
        buffer.append(footer).append('\f');

        return buffer.toString();
    }

    /**
     * Get the contacts page
     *
     * @return The contacts page text
     */
    private String E19Contacts() {
        StringBuilder buffer = new StringBuilder(
                TextReportConstants.E19_HDR_CONTACTS).append("\n\n");

        int count1 = countNewlines(buffer.toString());

        String tmp1 = "     SQ  CONTACT/REMARKS                                    PHONE\n"
                + "     --  -------------------------------------------------- ------------------\n";
        buffer = buffer.append(tmp1);

        int count2 = countNewlines(buffer.toString()) - count1;

        int available = getLinesPerPage() - count1 - count2 - 4;
        int avail = available;

        List<Contacts> contactList = Collections.emptyList();
        TextReportData data = TextReportDataManager.getInstance()
                .getDataForReports(lid, 6);
        if (!CollectionUtil.isNullOrEmpty(data.getContactList())) {
            contactList = data.getContactList();
        }

        int loop = 0;
        for (Contacts contact : contactList) {
            // Do contact & phone.
            String tmp4 = String.format("     %2d  %-51s%s\n",
                    contact.getPriority(), contact.getContact(),
                    contact.getPhone());

            // Do email address.
            String tmp6 = String.format("         %-60s\n", contact.getEmail());

            // Do remark.
            int numCols = 48;
            int leftMargin = 11;
            StringBuilder tmp5 = new StringBuilder();
            if (contact.getRemark() != null) {
                String[] lines = TextUtil.wordWrap(contact.getRemark(), numCols,
                        leftMargin);
                for (int i = 0; i < lines.length; i++) {
                    String s = lines[i];

                    if (i == 0) {
                        tmp5.append(
                                String.format("           %-48s\n", s.trim()));
                    } else if ((i == lines.length - 1)
                            && (s.trim().isEmpty())) {
                        continue;
                    } else {
                        tmp5.append(s).append('\n');
                    }
                }
                tmp5.append('\n');
            }

            int needed = countNewlines(tmp5.toString()) + 1 + 1 + 1;

            if (needed > avail) {
                // try to place FOOTER at the bottom
                buffer.append(advanceToFooter(loop, buffer.toString()));

                // Do footer.
                String footer = createFooter(data, E19_RREVISE_TYPE,
                        getDate(), "NWS FORM E-19", E19_CONTACTS,
                        "CONTACTS", null, E19_STANDARD_LEFT_MARGIN);
                buffer.append(footer).append('\f');

                // Do column header.
                String tmp2 = String.format("     %-55s  PHONE\n",
                        "CONTACT/REMARKS");
                String tmp3 = String.format("     %-20s%-20s%-15s  %-18s\n",
                        "--------------------", "--------------------",
                        "---------------", "------------------");
                buffer = buffer.append(tmp1).append(tmp2).append(tmp3);

                avail = available + count1;
                loop++;
            }

            buffer = buffer.append(tmp4).append(tmp6).append(tmp5);
            avail -= needed;
        }

        // try to place FOOTER at the bottom
        buffer.append(advanceToFooter(loop, buffer.toString()));

        String footer = createFooter(data, E19_RREVISE_TYPE, getDate(),
                "NWS FORM E-19", E19_CONTACTS, "CONTACTS", null,
                E19_STANDARD_LEFT_MARGIN);
        buffer.append(footer);

        return buffer.toString();
    }

    /**
     * Get the highest crest
     *
     * @return The highest crest
     */
    protected String getHighestCrest(int option, String date) {
        String buffer = null;
        String where = null;

        /* Create the query */
        String query = "select stage, datcrst from crest";

        /* Create the where clause from the desired option. */
        if (option == E19_GAGE_READING_CREST) {
            where = " where lid = '" + lid + "' and hw is null ";
        } else if (option == E19_HIGH_WATERMARKS_CREST) {
            where = " where lid = '" + lid + "' and hw = 'X' ";
        } else if (option == E19_TIME_CREST) {
            where = " where lid = '" + lid + "' and datcrst >= '" + date + "'";
        }

        where += " order by stage desc ";

        /* Create the return string and return. */
        ArrayList<Object[]> rs = TextReportDataManager.getInstance()
                .getCrest(query + where);

        if ((rs != null) && (!rs.isEmpty())) {
            // get the first record
            Object[] oa = rs.get(0);
            buffer = String.format("%s     %10s", oa[0],
                    sdf.format((Date) oa[1]));
        }

        return buffer;
    }

    /**
     * Create the footer.
     *
     * @param data
     *            report data
     * @param reviseType
     *            type
     * @param currDate
     *            the current date
     * @param formName
     *            form name
     * @param pageType
     *            type of page
     * @param pageTypeLabel
     *            page type label
     * @param optHeader
     *            optional header
     * @param leftMargin
     *
     * @return The footer text
     */
    private String createFooter(TextReportData data, int reviseType,
            String currDate, String formName, int pageType,
            String pageTypeLabel, String optHeader, int leftMargin) {
        if (initFooter) {
            initFooter = false;
            prevPageType = -1;
        }

        /*
         * Set up proper pageNum & subpageNum which is based on the
         * prevPageType.
         */
        /*
         * End result: all pages are numbered based on the pageType, however, if
         * a given pageType continues onto the next page, a subpageNum is used
         * as well (ex. "Page 5: CRESTS" -> "Page 5-2: CRESTS", etc).
         */
        int pageNum = pageType;
        if (pageType == prevPageType) {
            subpageNum++;
        } else {
            subpageNum = 1;
            prevPageType = pageType;
        }

        // Init buffer
        // Add left margin
        // Add (optional) header
        StringBuilder buffer = new StringBuilder();
        if (optHeader != null) {
            for (int i = 0; i < leftMargin; i++) {
                buffer.append(" ");
            }
            buffer.append(optHeader);
        }

        TextReportData dataRivStat = TextReportDataManager.getInstance()
                .getDataForReports(lid, 0);
        String tmp = String.format("%s %s %s, %s",
                dataRivStat.getRiverstat().getStream(),
                dataRivStat.getDescrip().getProximity(),
                locData.getLocation().getName(),
                locData.getLocation().getState());

        String date = null;
        if (reviseType == E19_LREVISE_TYPE) {
            if (locData.getLocation().getLrevise() != null) {
                date = sdf.format(locData.getLocation().getLrevise());
            }
        } else {
            /* revise_type == E19_RREVISE_TYPE */
            if (dataRivStat.getRiverstat().getRrevise() != null) {
                date = sdf.format(dataRivStat.getRiverstat().getRrevise());
            }
        }

        String tmp3 = String.format(
                "LOCATION: %-57s Revised, Printed Dates: %-10s, %-10s\n", tmp,
                date, currDate);

        for (int i = 0; i < leftMargin; i++) {
            buffer.append(" ");
        }

        buffer.append(tmp3);

        for (int i = 0; i < leftMargin; i++) {
            buffer.append(" ");
        }

        if (subpageNum > 1) {
            buffer.append(String.format(
                    "      ID: %-11s                    HSA: %-20s  %s PAGE %d-%d: %s\n",
                    lid, locData.getLocation().getHsa(), formName, pageNum,
                    subpageNum, pageTypeLabel));
        } else {
            buffer.append(String.format(
                    "      ID: %-11s                    HSA: %-20s  %s PAGE %d: %s\n",
                    lid, locData.getLocation().getHsa(), formName, pageNum,
                    pageTypeLabel));
        }

        return buffer.toString();
    }

    /**
     * Build the next line of the staff gage.
     *
     * @param s
     *            String to display
     *
     * @return The staff gage text
     */
    private String e19BuildStaffGageString(String s) {
        String tmp1 = null;
        if ((s != null) && (s.length() > 0)) {
            tmp1 = String.format("%c%s-%c", SEP_CHAR, s, SEP_CHAR);
        } else {
            tmp1 = String.format("%c-----%c", SEP_CHAR, SEP_CHAR);
        }

        return tmp1;
    }
}
