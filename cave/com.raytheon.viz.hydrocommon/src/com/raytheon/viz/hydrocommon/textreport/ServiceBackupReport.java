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

/**
 * Service Backup Report.
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

public class ServiceBackupReport extends TextReport {
    private static final int SERVBKUP_RECORD_LIMIT_PER_PAGE = 62;

    private static final String TITLE_LID = "LIST OF LOCATIONS SORTED BY STATION ID";

    private static final String TITLE_WFO = "LIST OF LOCATIONS SORTED BY WFO";

    private static final String TITLE_HSA = "LIST OF LOCATIONS SORTED BY HSA";

    private static final String HDR_FORMAT = "     %-10s     %-50s   Page %2d";

    /**
     * Constructor.
     * 
     * @param lid
     *      The location id
     */
    public ServiceBackupReport(String lid) {
        this.lid = lid;
    }

    /**
     * Get the text for the dialog.
     */
    @Override
    public String loadTextWidget() {
        return getText(0);
    }

    /**
     * Get the text of the report.
     * 
     * @param page
     *      The page number to generate
     */
    @Override
    public String getText(int sort) {
        /* Get the text . */
        String text = generateReport(sort);
        text = text.replace("null", "");

        return text;
    }

    /**
     * Generate the report.
     * 
     * @param sort
     *      Which column to sort on
     *      
     * @return
     *      The text
     */
    private String generateReport(int sort) {
        StringBuilder buffer = new StringBuilder();
        String hdrTitle = "LIST OF LOCATIONS";

        // Get the data for the report
        ServiceBackupData[] sbData = TextReportDataManager.getInstance()
                .getSvcBkupData(sort);

        int recordLimit; /* max num records per "8 1/2 x 11" page */
        int count = 0; /* for counting the num of records processed so far */
        int pageNum = 1;

        recordLimit = SERVBKUP_RECORD_LIMIT_PER_PAGE;

        /* Check page type. */
        switch (sort) {
        case TextReportConstants.SERVBKUP_SORTBY_LID:
            hdrTitle = TITLE_LID;
            break;
        case TextReportConstants.SERVBKUP_SORTBY_WFO:
            hdrTitle = TITLE_WFO;
            break;
        case TextReportConstants.SERVBKUP_SORTBY_HSA:
            hdrTitle = TITLE_HSA;
            break;
        }

        /* Build header line. */
        buffer.append(getHeaderLine(hdrTitle, pageNum));

        /*
         * Keep building the list of locations (& headers) for the given
         * pageType until there are no more records or until the record_limit is
         * reached.
         */
        for (int i = 0; i <sbData.length; i++) {
            /* Write out a single output line as specified by the order. */
            buffer.append("     ");
            buffer.append(String.format("%-10s    %-2s,%-20s    %-3s    %-7s\n",
                    sbData[i].getLid(), sbData[i].getState(), sbData[i].getCounty(),
                    sbData[i].getWfo(), sbData[i].getHsa()));
            count++;
            
            /* End of page reached, print header and reset count, increment pageNum */
            if (count == recordLimit) {
                pageNum++;
                buffer.append("\n");
                buffer.append(getHeaderLine(hdrTitle, pageNum));
                count = 0;
            }
        }
        
        /* Paginate */
        for (int i = count; i < 60; i++) {
            buffer.append("\n");
        }
        
        return buffer.toString();
    }

    /**
     * Build the header lines of the report.
     * 
     * @param title
     *            The page title
     * @param pageNum
     *            The page number
     * @return The header lines
     */
    private String getHeaderLine(String title, int pageNum) {
        StringBuffer hdr = new StringBuffer();

        hdr.append(String.format(HDR_FORMAT, getDate(), title, pageNum));
        hdr.append("\n\n");
        hdr
                .append("     STATION ID    ST,COUNTY                  WFO    HSA\n");
        hdr
                .append("     ----------    -----------------------    ---    ---\n");

        return hdr.toString();
    }
}
