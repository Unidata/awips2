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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * Base Report Class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009 2260       mpduff     Initial creation
 * Apr 10, 2012 14499      wkwock     correct algorithm in countNewlines
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public abstract class TextReport {
    protected SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy");
    
    protected SimpleDateFormat yearFormat = new SimpleDateFormat("yyyy");

    /** "StnClass Page" Types (for printing also) */
    protected static final int STNCLASS_SUMMARY = 0;

    /** (for initial purposes only) */
    protected static final int STNCLASS_ALLPAGES = 1;
    
    protected String lid = null;
    
    private int linesPerPage = -999;

    private int footerPosition = -999;

    protected String reportText;

    protected int firstPage;

    protected int lastPage;

    public abstract String loadTextWidget();

    public abstract String getText(int page);

    /**
     * Get the current date.
     * 
     * @return The date in mm/dd/yyyy format
     */
    public String getDate() {
        Date d = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();        

        return sdf.format(d);
    }

    /**
     * Get the number of lines in the String.
     * @param s
     *      The String
     * @return
     *      The number of lines in the String
     */
    protected int countNewlines(String s) {
        int newLineCount = 0;
        if ((s != null) && (s.length() > 0)) {
            byte bStr[]=s.getBytes();
            for (byte b : bStr) {
            	if (b=='\n') {
            		newLineCount++;
            	}
            }
        } else {
            return 0;
        }

        return newLineCount;
    }

    /**
     * Get the number of lines per page.
     * 
     * @return
     *      The number of lines per page
     */
    protected int getLinesPerPage() {
        if (linesPerPage == -999) {
            AppsDefaults apps = AppsDefaults.getInstance();

            // Get the lines per page, default is 60
            String lpp = apps.getToken("whfs_lines_per_page", "60");
            linesPerPage = Integer.parseInt(lpp);
        }

        return linesPerPage;
    }

    /**
     * Get the footer position on the page.
     * 
     * @return The footer position
     */
    protected int getFooterPosition() {
        if (footerPosition == -999) {
            footerPosition = getLinesPerPage() - 4;
        }

        return footerPosition;
    }

    /**
     * Break text up into pages.
     * 
     * @param text
     *      The text to paginate
     * @return
     *      The newly paginated text
     */
    protected String paginate(String text) {
        if (text == null) {
            return null;
        }

        int newLines = countNewlines(text);
        int nlCount = 0;

        /* Find number of "lines" leftover on last page. */
        int leftover = newLines;

        while (leftover > getLinesPerPage()) {
            leftover -= linesPerPage;
        }
        
        if (leftover == 0) {
            return text;
        }

        /*
         * Necessary to add newlines:
         * 
         * (Start with original text.) Create a string of newlines. Add string
         * of newlines to original text to get "new" text.
         */
        nlCount = linesPerPage - leftover;

        for (int i = 0; i < nlCount; i++) {
            text.concat("\n");
        }

        return text;
    }
    
    /**
     * Advance the cursor down to the location of the footer.
     * 
     * @param loop
     *      How many times through
     * @param str
     *      The page text
     * @return
     *      The number of "\n" to make it to the bottom of the page
     */
    protected String advanceToFooter(int loop, String str) {
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < ((getLinesPerPage() * loop) + getFooterPosition()) - countNewlines(str); i++) {
           buf.append("\n"); 
        }
        
        return buf.toString();
    }
    

}
