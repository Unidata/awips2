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

/**
 * Station Class Report.
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

public class StationClassReport extends TextReport {
    private static final int STNCLASS_STANDARD_LEFT_MARGIN = 7;

    private static final String stnclassHdrSummary = "STATION CLASS REPORT";
    private static final int stnclassFirstPage = STNCLASS_SUMMARY;
    private static final int stnclassLastPage = STNCLASS_SUMMARY;


    private boolean initHeader = true;
    private boolean initFooter = true;
    private int pageNum = 0;

    /**
     * Constructor.
     */
    public StationClassReport() {
        firstPage = 0;
        lastPage = 0;
    }
    
    /**
     * Get the text for the dialog.
     */
    @Override
    public String loadTextWidget() {
        String text = getText(STNCLASS_ALLPAGES);
        text = text.replace("null", "    ");
        return text;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrocommon.textreport.TextReport#getData(int, int)
     */
    @Override
    public String getText(int page) {
        String text = null;
        if (page == STNCLASS_ALLPAGES) {
            text = getAllPages();
        } else if (page == STNCLASS_SUMMARY) {
            text = getSummary();
        }

        return text;
    }
    
    /**
     * Get all the pages.
     * 
     * @return
     *      Text of all pages
     */
    private String getAllPages() {
        StringBuffer buffer = new StringBuffer();
        String text = null;
        
        /* Get the text for the given page(s). */
        for (int i = stnclassFirstPage; i <= stnclassLastPage; i++) {
            text = getText(i);
            buffer.append(paginate(text));
        }
        
        return buffer.toString();
    }

    /**
     * Get the summary
     * 
     * @return
     *      The summary
     */
    private String getSummary() {
        StringBuilder buffer = new StringBuilder();
        TextReportDataManager dman = TextReportDataManager.getInstance();
        int count1 = 0;
        int count2;
        int loop = 0;
        int available;
        int avail;
        int needed;

        buffer.append(createHeader());
        buffer.append("\n\n\n");
        buffer
                .append("       LID         STATION TYPE    DCP    OBSERVER    TELEMETRY DEVICE\n");
        buffer
                .append("       --------    ------------    ---    --------    ----------------\n");

        count2 = countNewlines(buffer.toString());

        available = getLinesPerPage() - count1 - count2 - 6;
        avail = available;

        ArrayList<String> lidList = dman.getLidList();

        for (String lid: lidList) {
            TextReportData.Stnclass stnclass = dman.getStnClass(lid);

            buffer.append(String.format(
                    "       %-8s    %-10s      %-3s       %-3s      %s\n",
                    lid, stnclass.getDisplayClass(), stnclass.getDcp(), 
                    stnclass.getObserver(), stnclass.getTelem_type()));
            
            needed = 1;
            
            if (needed <= avail) {
                avail = avail - needed;
            } else if (needed > avail) {
                /* try to place FOOTER at the bottom */
                buffer.append(advanceToFooter(loop, buffer.toString()));
                
                /* Do footer */
                buffer.append(createFooter());
                
                /* Do Column Header */
                buffer.append(createHeader());
                buffer.append("\n\n\n");
                buffer.append("       LID         STATION TYPE    DCP    OBSERVER    TELEMETRY DEVICE\n");
                buffer.append("       --------    ------------    ---    --------    ----------------\n");
                avail = available + count1;
                loop++;
            }
        }

        return buffer.toString();
    }

    /**
     * Create the header.
     * 
     * @return
     *      The header text
     */
    private String createHeader() {
        StringBuilder buffer = new StringBuilder();
        StringBuilder margin = new StringBuilder();

        if (initHeader) {
            pageNum = 0;
            initHeader = false;
        }
        pageNum += 1;

        for (int i = 0; i < STNCLASS_STANDARD_LEFT_MARGIN; i++) {
            margin.append(" ");
        }

        buffer.append(margin.toString());
        buffer.append(String.format("%-10s    %-9s\n", getDate(), " "));

        buffer.append(margin.toString());
        buffer.append(String.format("%-20s\n", stnclassHdrSummary));

        buffer.append(String.format("                     Page %2d", pageNum));

        return buffer.toString();
    }
    
    /**
     * Create the footer.
     * 
     * @return
     *      The footer text
     */
    private String createFooter() {
        StringBuilder buf = new StringBuilder();
        StringBuilder margin = new StringBuilder();
        
        if (initFooter) {
            
            initFooter = false;
        }
        
        for (int i = 0; i < STNCLASS_STANDARD_LEFT_MARGIN; i++) {
            margin.append(" ");
        }
        
        buf.append(margin.toString());
        buf.append("KEY:\n\n");
        buf.append(margin.toString());
        buf.append("F - Fcst Point   D - Reservoir       S - Snow   O - Other\n");
        buf.append(margin.toString());
        buf.append("R - River Data   P - Precipitation   T - Temp   U - Undefined\n");
        
        return buf.toString();
    }
}
