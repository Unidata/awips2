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
package com.raytheon.edex.plugin.sfcobs.decoder;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.core.DefaultParserStrategy;
import com.raytheon.uf.edex.decodertools.core.ReportParser;

/**
 * AbstractSfcObsDecoder is the base class for all surface observation decoders.
 * It contains common data items, observation time for example, and methods
 * common to most of the decoders.
 * 
 * <pre>
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 19, 2007  391      jkorman     Initial Coding.
 * Oct 29, 2013  2489     bsteffen    Add null check to matchElement.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class AbstractSfcObsDecoder implements ISfcObsDecoder {

    private Calendar obsTime = null;

    private String reportIdentifier = null;

    // Protected for implementing classes.
    protected String reportData = null;

    protected String reportPrefix = null;

    protected ReportParser reportParser = null;

    protected WMOHeader header = null;

    /**
     * Get the report data for this observation.
     * 
     * @return The reportData.
     */
    public String getReportData() {
        return reportData;
    }

    /**
     * Set the report data for this observation. The report parser for the data
     * is also created.
     * 
     * @param report
     *            The observation data for this report.
     */
    @Override
    public void setReportData(String report) {
        reportData = report;
        reportParser = new ReportParser(reportData, new DefaultParserStrategy());
    }

    /**
     * Get the WMO header associated with this report.
     * 
     * @return The WMO header.
     */
    public WMOHeader getHeader() {
        return header;
    }

    /**
     * Set the WMO header associated with this report.
     * 
     * @param header
     *            The WMO header.
     */
    public void setHeader(WMOHeader header) {
        this.header = header;
    }

    /**
     * Check if the observation data represents a NIL observation.
     * 
     * @return Is this observation a possible NIL?
     */
    public boolean isNILObs() {
        boolean isNIL = false;

        if (reportData != null) {
            isNIL = reportData.startsWith(reportPrefix)
                    & reportData.endsWith("NIL");
        }
        return isNIL;
    }

    /**
     * Get the observation time for this report.
     * 
     * @return The report observation time.
     * @see com.raytheon.edex.plugin.sfcobs.decoder.ISfcObsDecoder#getObsTime()
     */
    @Override
    public Calendar getObsTime() {
        return obsTime;
    }

    /**
     * Set the observation time for this report.
     * 
     * @param obsTime
     *            The observation time.
     */
    public void setObsTime(Calendar obsTime) {
        this.obsTime = obsTime;
    }

    /**
     * Get the identifier set for this report.
     * 
     * @return The report identifier.
     */
    public String getReportIdentifier() {
        return reportIdentifier;
    }

    /**
     * Set the identifier for this report.
     * 
     * @param reportIdentifier
     *            The report identifier.
     */
    public void setReportIdentifier(String reportIdentifier) {
        this.reportIdentifier = reportIdentifier;
    }

    /**
     * Determine if a given element matches a regular expression pattern.
     * 
     * @param element
     *            The string element to match.
     * @param pattern
     *            The regular expression pattern.
     * @return
     */
    public static final boolean matchElement(String element, String pattern) {
        boolean matches = false;
        if (pattern != null && element != null) {
            Pattern p = Pattern.compile(pattern);
            Matcher m = p.matcher(element);
            matches = m.find();
        }
        return matches;
    }

    /**
     * Does a value fall between two specified values, inclusive.
     * @param lo The range minimum value.
     * @param x The value to check.
     * @param hi The range maximum value.
     * @return Does a value fall between the min, max values, inclusive?
     */
    public static boolean checkRange(double lo, double x, double hi) {
        return ((x >= lo) && (x <= hi));
    }
    
    /**
     * Convert a defined substring within an element to an integer value. If the
     * substring equals a string of slashes a value of VAL_MISSING is returned.
     * If a number conversion error occurs a value of VAL_ERROR is returned.
     * 
     * @param element
     *            The text element to decode.
     * @param start
     *            The start position within the text element.
     * @param stop
     *            The stop position within the text element.
     * @return
     */
    public static final Integer getInt(String element, int start, int stop) {
        Integer retValue = null;

        if (element != null) {
            if ((start >= 0) && (stop <= element.length()) && (start < stop)) {
                // If the first character of the substring is a slash, then
                // assume that we are dealing with missing data.
                if (element.charAt(start) == '/') {
                    retValue = VAL_MISSING;
                    // Make sure all the specified data are slashes.
                    for (int i = start; i < stop; i++) {
                        // If not, then its an error.
                        if (element.charAt(i) != '/') {
                            retValue = VAL_ERROR;
                            break;
                        }
                    }
                } else {
                    try {
                        retValue = Integer.parseInt(element.substring(start,
                                stop));
                    } catch (NumberFormatException nfe) {
                        retValue = VAL_ERROR;
                    }
                }
            }
        }

        return retValue;
    }

}
