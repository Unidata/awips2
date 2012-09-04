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
package com.raytheon.edex.plugin.pirep.decoder;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Parse Pilot Report (PIREP) Text Element Indicators (TEIs) from potential
 * report text. This 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * AWIPS2 DR Work
 * Aug  7, 2012       1011 jkorman     Initial creation
 * Aug 23, 2012       1011 jkorman     Change control characters to spaces. 
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TEIInfo implements Comparable<TEIInfo> {
    // The start position of this TEI within the decoded data.
    private int start;

    // The stop position of this TEI's text within the decoded data.
    private int stop;

    // The TEI associated with the info.
    private TEI tei;

    // The extracted text associated with this TEI.
    private String teiText;

    /**
     * Construct a TEIInfo instance with a given TEI, position, and index.
     * 
     * @param tei
     * @param position
     * @param teiIndex
     */
    private TEIInfo(TEI tei, int position) {
        this.tei = tei;
        start = position;
    }

    /**
     *  
     */
    @Override
    public String toString() {
        return String.format("%s:%d:%d:%d", tei, start, stop);
    }

    /**
     * Calculate the hashCode for this instance.
     * 
     * @return The calculated hashCode.
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + start;
        result = prime * result + ((tei == null) ? 0 : tei.hashCode());
        result = prime * result + ((teiText == null) ? 0 : teiText.hashCode());
        return result;
    }

    /**
     * Is this instance equal to another object instance.
     * 
     * @return Is this instance equal to another object instance.
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        TEIInfo other = (TEIInfo) obj;
        if (start != other.start)
            return false;
        if (tei != other.tei)
            return false;
        if (teiText == null) {
            if (other.teiText != null)
                return false;
        } else if (!teiText.equals(other.teiText))
            return false;
        return true;
    }

    /**
     * Override compareTo for TEIInfo. This method uses the start position of
     * the TEI within the decoded data as the comparands.
     */
    @Override
    public int compareTo(TEIInfo t) {
        return (t.start == start) ? 0 : (t.start < start) ? 1 : -1;
    }

    /**
     * Get the TEI that was decoded.
     * 
     * @return The decoded TEI.
     */
    public TEI getTei() {
        return tei;
    }

    /**
     * Get the extracted text for the TEI.
     * 
     * @return The TEI extracted text.
     */
    public String getTeiText() {
        return teiText;
    }

    /**
     * Extract the TEI information as well as the text data associated with that
     * TEI into a collection of TEIInfo objects. Out of order data can be correctly
     * parsed and returned.
     * 
     * @param str
     *            Data containing a PIREP.
     * @return Returns a list of TEIs found in the input data. Returns a zero
     *         length list if the input was null or no TEIs could be found.
     */
    public static List<TEIInfo> findTEIs(String str) {
        List<TEIInfo> positions = new ArrayList<TEIInfo>();
        if (str != null) {
            StringBuilder sb = new StringBuilder(str);
            for(int i = 0;i < sb.length();i++) {
                char c = sb.charAt(i);
                if(c < ' ') {
                    sb.setCharAt(i, ' ');
                }
            }
            str = sb.toString();
            int teiIndex = 0;
            // loop over the valid TEIs
            for (TEI tei : TEI.PIREP) {
                int n = str.indexOf(tei.getId());
                if (n >= 0) {
                    TEIInfo info = new TEIInfo(tei, n);
                    positions.add(info);
                }
                teiIndex++;
            }
            Collections.sort(positions);
            if (positions.size() > 0) {
                TEIInfo tt = positions.get(0);
                // Ensure that the PIREP is starting correctly!
                if (TEI.OV.equals(tt.tei)) {
                    // Note that this will find both "UA" and "UUA"
                    if (str.substring(0, tt.start).indexOf("UA") > 0) {
                        // Insert the PIREP element at the 'head' of the list
                        positions.add(0, new TEIInfo(TEI.PIREP, 0));
                        // Now iterate the list and fixup the stop positions
                        // of each TEI
                        TEIInfo previous = null;
                        for (TEIInfo t : positions) {
                            if (previous == null) {
                                previous = t;
                            } else {
                                previous.stop = t.start;
                                previous = t;
                            }
                        }
                        // Set the last TEI stop position to the end of the
                        // report.
                        previous.stop = str.length();
                    }
                }
            }
            // Now come in and extract the text associated with each TEI.
            for (TEIInfo t : positions) {
                if (TEI.PIREP.equals(t.getTei())) {
                    // Pick up the leading portion of the PIREP
                    t.teiText = str.substring(t.start, t.stop).trim();
                } else {
                    t.teiText = str.substring(
                            t.start + t.getTei().getId().length(), t.stop)
                            .trim();
                }
            }
        }
        return positions;
    }
}
