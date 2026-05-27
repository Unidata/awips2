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
 * Sorted Station List Report.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009 2260       mpduff     Initial creation
 * Sep 11, 2012 13781      wkwock     add print menu
 *  
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StationListReport extends TextReport {
    private static final String HDR_FORMAT = "     %-10s    %-30s    %-70s   Page %2d";

    private static final String TITLE_LID = "LIST OF LOCATIONS SORTED BY LID";

    private static final String TITLE_NAME = "LIST OF LOCATIONS SORTED BY NAME";

    private static final String TITLE_COUNTY = "LIST OF LOCATIONS SORTED BY COUNTY";

    private static final String TITLE_BASIN = "LIST OF LOCATIONS SORTED BY BASIN";

    private static final String TITLE_OBSERVER = "LIST OF LOCATIONS SORTED BY OBSERVER";

    private static final int STALIST_RECORD_LIMIT_PER_PAGE = 54;

    /* Types used for ordering columns */
    protected static final int STALIST_LID = 1;

    protected static final int STALIST_NAM = 2;

    protected static final int STALIST_COU = 3;

    protected static final int STALIST_BAS = 4;

    protected static final int STALIST_WFO = 5;

    protected static final int STALIST_OBS = 6;

    private static final int[] sortedby_lid = { STALIST_LID, STALIST_NAM,
            STALIST_COU, STALIST_BAS, STALIST_WFO, STALIST_OBS, 0 };

    private static final int[] sortedby_name = { STALIST_NAM, STALIST_LID,
            STALIST_COU, STALIST_BAS, STALIST_WFO, STALIST_OBS, 0 };

    private static final int[] sortedby_county = { STALIST_COU, STALIST_LID,
            STALIST_NAM, STALIST_BAS, STALIST_WFO, STALIST_OBS, 0 };

    private static final int[] sortedby_basin = { STALIST_BAS, STALIST_LID,
            STALIST_NAM, STALIST_COU, STALIST_WFO, STALIST_OBS, 0 };

    private static final int[] sortedby_observer = { STALIST_OBS, STALIST_LID,
            STALIST_NAM, STALIST_COU, STALIST_BAS, STALIST_WFO, 0 };

    /**
     * Get the text of the report.
     * 
     * @param sort
     *      The column to sort on
     */
    @Override
    public String getText(int sort) {
        /* Get the text . */
        String text = generateReport(sort);
        text = text.replace("null", "");

        return text;
    }

    /**
     * Get the text for the dialog.
     */
    @Override
    public String loadTextWidget() {
        return getText(0);//, TEXTREPORTS_REASON_NORMAL);
    }
    
    
    public int[] getSortArray(int sort) {
    	int[] sortArray=null;
        switch (sort) {
        case TextReportConstants.STALIST_SORTBY_LID:
            sortArray = sortedby_lid;
            break;
        case TextReportConstants.STALIST_SORTBY_NAME:
            sortArray = sortedby_name;
            break;
        case TextReportConstants.STALIST_SORTBY_COUNTY:
            sortArray = sortedby_county;
            break;
        case TextReportConstants.STALIST_SORTBY_BASIN:
            sortArray = sortedby_basin;
            break;
        case TextReportConstants.STALIST_SORTBY_OBSERVER:
            sortArray = sortedby_observer;
            break;
        }

        return sortArray;
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
    public String generateSpecialReport(int sort, int[] sortArray) {
        StringBuilder buffer = new StringBuilder();
        String hdrTitle = "LIST OF LOCATIONS";

        // Get the data for the report
        StationList[] slData = TextReportDataManager.getInstance()
                .getStnListData(sort);

        int recordLimit; /* max num records per "8 1/2 x 11" page */
        int count = 0; /* for counting the num of records processed so far */
        int pageNum = 1;

        recordLimit = STALIST_RECORD_LIMIT_PER_PAGE;

        /* Check sort type. */
        switch (sort) {
        case TextReportConstants.STALIST_SORTBY_LID:
            hdrTitle = TITLE_LID;
            break;
        case TextReportConstants.STALIST_SORTBY_NAME:
            hdrTitle = TITLE_NAME;
            break;
        case TextReportConstants.STALIST_SORTBY_COUNTY:
            hdrTitle = TITLE_COUNTY;
            break;
        case TextReportConstants.STALIST_SORTBY_BASIN:
            hdrTitle = TITLE_BASIN;
            break;
        case TextReportConstants.STALIST_SORTBY_OBSERVER:
            hdrTitle = TITLE_OBSERVER;
            break;
        }

        /* Build header line. */
        buffer.append(getHeaderLine(hdrTitle, pageNum, sortArray));

        /*
         * Keep building the list of locations (& headers) for the given
         * pageType until there are no more records or until the record_limit is
         * reached.
         */
        for (int j = 0; j < slData.length; j++) {
            /* Write out a single output line as specified by the order. */
            buffer.append("     ");
            for (int i = 0; i < sortArray.length; i++) {
                switch (sortArray[i]) {
                case STALIST_LID:
                    buffer.append(String.format("%-8s", slData[j].getLid()));
                    break;
                case STALIST_NAM:
                    if (slData[j].getName() != null) {
                        if (slData[j].getName().length() > 20) {
                            buffer.append(String.format("%-20s", slData[j]
                                    .getName().substring(0, 20)));
                        } else {
                            buffer.append(String.format("%-20s", slData[j]
                                    .getName()));
                        }
                    } else {
                        buffer.append(String.format("%-8s", " "));
                    }
                    break;
                case STALIST_COU:
                    buffer
                            .append(String.format("%-20s", slData[j]
                                    .getCounty()));
                    break;
                case STALIST_BAS:
                    if (slData[j].getRb() != null) {
                        if (slData[j].getRb().length() > 20) {
                            buffer.append(String.format("%-20s", slData[j]
                                    .getRb().substring(0, 20)));
                        } else {
                            buffer.append(String.format("%-20s", slData[j]
                                    .getRb()));
                        }
                    } else {
                        buffer.append(String.format("%-20s", " "));
                    }
                    break;
                case STALIST_WFO:
                    buffer.append(String.format("%-3s", slData[j].getWfo()));
                    break;
                case STALIST_OBS:
                    if (slData[j].getLastname() != null) {
                        if (slData[j].getLastname().length() > 24) {
                            buffer.append(String.format("%-24s", slData[j]
                                    .getLastname().substring(0, 24)));
                        } else {
                            buffer.append(String.format("%-24s", slData[j]
                                    .getLastname()));
                        }
                    } else {
                        buffer.append(String.format("%-24s", " "));
                    }
                    break;
                }

                buffer.append(" ");
            }

            if (slData[j].getHPhone() != null) {
                if (slData[j].getHPhone().length() > 12) {
                    buffer.append(String.format("%-12s", slData[j].getHPhone()
                            .substring(0, 12)));
                } else {
                    buffer
                            .append(String.format("%-12s", slData[j]
                                    .getHPhone()));
                }
            } else {
                buffer.append(String.format("%-12s", " "));
            }
            buffer.append(" ");
            
            if (slData[j].getOPhone() != null) {
                if (slData[j].getOPhone().length() > 14) {
                    buffer.append(String.format("%-14s\n", slData[j]
                            .getOPhone().substring(0, 14)));
                } else {
                    buffer.append(String.format("%-14s\n", slData[j]
                            .getOPhone()));
                }
            } else {
                buffer.append(String.format("%-14s\n", " "));
            }
            count++;

            /*
             * End of page reached, print header and reset count, increment
             * pageNum
             */
            if (count == recordLimit) {
                pageNum++;
                buffer.append("\n");
                buffer.append(getHeaderLine(hdrTitle, pageNum, sortArray));
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
        StationList[] slData = TextReportDataManager.getInstance()
                .getStnListData(sort);

        int recordLimit; /* max num records per "8 1/2 x 11" page */
        int count = 0; /* for counting the num of records processed so far */
        int pageNum = 1;
        int[] sortArray = null;

        recordLimit = STALIST_RECORD_LIMIT_PER_PAGE;

        /* Check sort type. */
        switch (sort) {
        case TextReportConstants.STALIST_SORTBY_LID:
            hdrTitle = TITLE_LID;
            sortArray = sortedby_lid;
            break;
        case TextReportConstants.STALIST_SORTBY_NAME:
            hdrTitle = TITLE_NAME;
            sortArray = sortedby_name;
            break;
        case TextReportConstants.STALIST_SORTBY_COUNTY:
            hdrTitle = TITLE_COUNTY;
            sortArray = sortedby_county;
            break;
        case TextReportConstants.STALIST_SORTBY_BASIN:
            hdrTitle = TITLE_BASIN;
            sortArray = sortedby_basin;
            break;
        case TextReportConstants.STALIST_SORTBY_OBSERVER:
            hdrTitle = TITLE_OBSERVER;
            sortArray = sortedby_observer;
            break;
        }

        /* Build header line. */
        buffer.append(getHeaderLine(hdrTitle, pageNum, sortArray));

        /*
         * Keep building the list of locations (& headers) for the given
         * pageType until there are no more records or until the record_limit is
         * reached.
         */
        for (int j = 0; j < slData.length; j++) {
            /* Write out a single output line as specified by the order. */
            buffer.append("     ");
            for (int i = 0; i < sortArray.length; i++) {
                switch (sortArray[i]) {
                case STALIST_LID:
                    buffer.append(String.format("%-8s", slData[j].getLid()));
                    break;
                case STALIST_NAM:
                    if (slData[j].getName() != null) {
                        if (slData[j].getName().length() > 20) {
                            buffer.append(String.format("%-20s", slData[j]
                                    .getName().substring(0, 20)));
                        } else {
                            buffer.append(String.format("%-20s", slData[j]
                                    .getName()));
                        }
                    } else {
                        buffer.append(String.format("%-8s", " "));
                    }
                    break;
                case STALIST_COU:
                    buffer
                            .append(String.format("%-20s", slData[j]
                                    .getCounty()));
                    break;
                case STALIST_BAS:
                    if (slData[j].getRb() != null) {
                        if (slData[j].getRb().length() > 20) {
                            buffer.append(String.format("%-20s", slData[j]
                                    .getRb().substring(0, 20)));
                        } else {
                            buffer.append(String.format("%-20s", slData[j]
                                    .getRb()));
                        }
                    } else {
                        buffer.append(String.format("%-20s", " "));
                    }
                    break;
                case STALIST_WFO:
                    buffer.append(String.format("%-3s", slData[j].getWfo()));
                    break;
                case STALIST_OBS:
                    if (slData[j].getLastname() != null) {
                        if (slData[j].getLastname().length() > 24) {
                            buffer.append(String.format("%-24s", slData[j]
                                    .getLastname().substring(0, 24)));
                        } else {
                            buffer.append(String.format("%-24s", slData[j]
                                    .getLastname()));
                        }
                    } else {
                        buffer.append(String.format("%-24s", " "));
                    }
                    break;
                }

                buffer.append(" ");
            }

            if (slData[j].getHPhone() != null) {
                if (slData[j].getHPhone().length() > 12) {
                    buffer.append(String.format("%-12s", slData[j].getHPhone()
                            .substring(0, 12)));
                } else {
                    buffer
                            .append(String.format("%-12s", slData[j]
                                    .getHPhone()));
                }
            } else {
                buffer.append(String.format("%-12s", " "));
            }
            buffer.append(" ");
            
            if (slData[j].getOPhone() != null) {
                if (slData[j].getOPhone().length() > 14) {
                    buffer.append(String.format("%-14s\n", slData[j]
                            .getOPhone().substring(0, 14)));
                } else {
                    buffer.append(String.format("%-14s\n", slData[j]
                            .getOPhone()));
                }
            } else {
                buffer.append(String.format("%-14s\n", " "));
            }
            count++;

            /*
             * End of page reached, print header and reset count, increment
             * pageNum
             */
            if (count == recordLimit) {
                pageNum++;
                buffer.append("\n");
                buffer.append(getHeaderLine(hdrTitle, pageNum, sortArray));
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
    private String getHeaderLine(String title, int pageNum, int[] sortArray) {
        StringBuffer hdr = new StringBuffer();
        
        hdr.append("     ");
        hdr.append(String.format(HDR_FORMAT, getDate(), " ", title, pageNum));
        hdr.append("\n\n");
        hdr.append(String.format("%-105s %s\n", " ",
                "-------PHONE NUMBERS-------\n"));
        hdr.append("     ");
        for (int i = 0; i < sortArray.length; i++) {
            switch (sortArray[i]) {
            case STALIST_LID:
                hdr.append("LID     ");
                break;
            case STALIST_NAM:
                hdr.append("LOCATION            ");
                break;
            case STALIST_COU:
                hdr.append("COUNTY              ");
                break;
            case STALIST_BAS:
                hdr.append("BASIN               ");
                break;
            case STALIST_WFO:
                hdr.append("WFO");
                break;
            case STALIST_OBS:
                hdr.append("OBSERVER                ");
                break;
            }

            hdr.append(" ");
        }
        hdr.append("HOME         WORK          \n");

        hdr.append("     ");
        for (int i = 0; i < sortArray.length; i++) {
            switch (sortArray[i]) {
            case STALIST_LID:
                hdr.append("--------");
                break;
            case STALIST_NAM:
                hdr.append("--------------------");
                break;
            case STALIST_COU:
                hdr.append("--------------------");
                break;
            case STALIST_BAS:
                hdr.append("--------------------");
                break;
            case STALIST_WFO:
                hdr.append("---");
                break;
            case STALIST_OBS:
                hdr.append("------------------------");
                break;
            }

            hdr.append(" ");
        }
        hdr.append("------------ --------------\n");

        return hdr.toString();
    }

}
