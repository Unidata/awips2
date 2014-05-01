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

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Mostly derived from the PVtecObject class in package
 * com.raytheon.edex.vtec.api to remove dependency.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2009            bwoodle     Initial creation
 * May 08, 2013  #1842     dgilling    Add getPhensig() method.
 * Feb 17, 2014  #2774     dgilling    Added toString() for debugging
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1.0
 */

public class VtecObject {

    /**
     * The format, used by {@link java.lang.String#format(String, Object...)},
     * that defines the P-VTEC string.
     */
    private static final String vtecCreateFormat = "/%s.%s.%s.%s.%s.%04d.%s-%s/";

    public static final String vtecParseFormat = "\\/([OTEX])\\.([A-Z]{3})\\.([A-Za-z0-9]{4})\\.([A-Z]{2})\\.([WAYSFON])\\.(\\d{4})\\.(\\d{6}T\\d{4}Z)-(\\d{6}T\\d{4}Z)\\/";

    private static final String PARTIAL_MATCH_FORMAT = "/[OTEX]\\.\\w{3}\\.(%s)\\.(%s)\\.(%s)\\.(\\d{4})\\.\\d{6}T\\d{4}Z-\\d{6}T\\d{4}Z/";

    /**
     * The P-VTEC Product Class token. Part of the P-VTEC Event Group. Initially
     * set to an underscore to simplify P-VTEC generation.
     */
    private String product = "_";

    /**
     * The P-VTEC Actions token. Part of the P-VTEC Event Group. Initially set
     * to an underscore to simplify P-VTEC generation.
     */
    private String action = "_";

    /**
     * The P-VTEC Office ID token. Part of the P-VTEC Event Group. Initially set
     * to an underscore to simplify P-VTEC generation.
     */
    private String office = "_";

    /**
     * The P-VTEC Phenomena token. Part of the P-VTEC Event Group. Initially set
     * to an underscore to simplify P-VTEC generation.
     */
    private String phenomena = "_";

    /**
     * The P-VTEC Significance token. Part of the P-VTEC Event Group. Initially
     * set to an underscore to simplify P-VTEC generation.
     */
    private String significance = "_";

    /**
     * The P-VTEC Event Tracking Number (ETN) token. Part of the P-VTEC Event
     * Group. Initially set to zero (0) to simplify P-VTEC generation.
     */
    private Integer sequence = 0;

    /**
     * The p-VTEC Event Beginning Date/Time value. Part of the P-VTEC Date/Time
     * Group.
     */
    private Calendar startTime = null;

    /**
     * The p-VTEC Event Ending Date/Time value. Part of the P-VTEC Date/Time
     * Group.
     */
    private Calendar endTime = null;

    /**
     * Constructor. Constructs an empty P-VTEC object.
     */
    public VtecObject() {

    }

    public VtecObject(String vtec) {
        Pattern p = Pattern.compile(vtecParseFormat);
        Matcher m = p.matcher(vtec);
        if (m.matches()) {
            // 1 - 8
            this.product = m.group(1);
            this.action = m.group(2);
            this.office = m.group(3);
            this.phenomena = m.group(4);
            this.significance = m.group(5);
            this.sequence = new Integer(m.group(6));
            this.startTime = VtecUtil.calendarFromVtec(m.group(7));
            this.endTime = VtecUtil.calendarFromVtec(m.group(8));
        }
    }

    /**
     * Constructor. Creates a P-VTEC object and populates it using the specified
     * parameters.
     * 
     * @param product
     *            the product classification.
     * @param action
     *            the action performed on the bulletin.
     * @param office
     *            the originating office.
     * @param phenomenon
     *            the phenomenon for the bulletin.
     * @param significance
     *            the significance of the event - essentially the bulletin type.
     * @param sequence
     *            the the VTN.
     * @param startTime
     *            the valid start time for the bulletin.
     * @param endTime
     *            the valid end time for the bulletin.
     */
    public VtecObject(String action, String office, String phenomenon,
            String significance, int sequence) {
        this.action = action;
        this.office = office;
        this.phenomena = phenomenon;
        this.significance = significance;
        this.sequence = sequence;
    }

    /**
     * Constructor. Creates a partially populated P-VTEC object.
     * 
     * @param office
     *            the originating office.
     * @param phenomenon
     *            the phenomenon for the bulletin.
     * @param significance
     *            the significance of the event - essentially the bulletin type.
     * @param sequence
     *            the the VTN.
     */
    public VtecObject(String office, String phenomenon, String significance,
            int sequence) {
        this.office = office;
        this.phenomena = phenomenon;
        this.significance = significance;
        this.sequence = sequence;
    }

    /**
     * Returns the P-VTEC Action token.
     * 
     * @return the action
     */
    public String getAction() {
        return action;
    }

    /**
     * Sets the P-VTEC Action token.
     * 
     * @param action
     *            the action to set
     */
    public void setAction(String action) {
        this.action = action;
    }

    /**
     * Returns the P-VTEC Office ID token.
     * 
     * @return the office
     */
    public String getOffice() {
        return office;
    }

    /**
     * Sets the P-VTEC Office ID token.
     * 
     * @param office
     *            the office to set
     */
    public void setOffice(String office) {
        this.office = office;
    }

    /**
     * Returns the P-VTEC Phenomena token.
     * 
     * @return the phenomenon
     */
    public String getPhenomena() {
        return phenomena;
    }

    /**
     * Sets the P-VTEC Phenomena token.
     * 
     * @param phenomenon
     *            the phenomenon to set
     */
    public void setPhenomena(String phenomenon) {
        this.phenomena = phenomenon;
    }

    /**
     * Returns the P-VTEC Significance token.
     * 
     * @return the significance
     */
    public String getSignificance() {
        return significance;
    }

    /**
     * Sets the P-VTEC Significance token.
     * 
     * @param significance
     *            the significance to set
     */
    public void setSignificance(String significance) {
        this.significance = significance;
    }

    /**
     * Returns the P-VTEC ETN Sequence value.
     * 
     * @return the sequence
     */
    public Integer getSequence() {
        return sequence;
    }

    /**
     * Sets the P-VTEC ETN Sequence value.
     * 
     * @param sequence
     *            the sequence to set
     */
    public void setSequence(Integer sequence) {
        this.sequence = sequence;
    }

    public String getProduct() {
        return product;
    }

    public void setProduct(String product) {
        this.product = product;
    }

    public Calendar getStartTime() {
        return startTime;
    }

    public void setStartTime(Calendar startTime) {
        this.startTime = startTime;
    }

    public Calendar getEndTime() {
        return endTime;
    }

    public void setEndTime(Calendar endTime) {
        this.endTime = endTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.vtec.db.IVtecObject#getVtecString()
     */
    public String getVtecString() {
        return generateVtec();
    }

    /**
     * Generates the P-VTEC String for the object. Missing fields are indicated
     * with an underscore.
     * 
     * @return the P-VTEC string for the object
     */
    private String generateVtec() {
        return String.format(vtecCreateFormat, this.product, this.action,
                this.office, this.phenomena, this.significance, this.sequence,
                VtecUtil.formatVtecTime(this.startTime),
                VtecUtil.formatVtecTime(this.endTime));
    }

    @Override
    public String toString() {
        return generateVtec();
    }

    /*
     * (non-Javadoc)
     */
    public String getMatchString() {
        return String.format(PARTIAL_MATCH_FORMAT, this.office, this.phenomena,
                this.significance);
    }

    public String getPhensig() {
        return phenomena + '.' + significance;
    }
}
