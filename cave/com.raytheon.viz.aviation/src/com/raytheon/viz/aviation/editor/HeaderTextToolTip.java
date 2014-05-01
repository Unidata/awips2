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
package com.raytheon.viz.aviation.editor;

/**
 * This contains a tool tip and where in a text area it should be displayed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2010 3263       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class HeaderTextToolTip {
    /**
     * The starting offset in a text area where the tool tip should be
     * displayed.
     */
    private int start;

    /**
     * The ending offset in a text area for displaying the tool tip.
     */
    private int end;

    /**
     * The tool tip to display.
     */
    private String value;

    /**
     * The constructor; this assumes 0 <= start < end.
     * 
     * @param start
     *            - Offset to start displaying the tool tip
     * @param end
     *            - The Offset to stop displaying the tool tip
     * @param value
     *            - The tool tip to display
     */
    public HeaderTextToolTip(int start, int end, String value) {
        this.start = start;
        this.end = end;
        this.value = value;
    }

    /**
     * Determine position of offset relative to start and end.
     * 
     * @param offset
     * @return -1 offset < start, 0 start <= offset < end, +1 offset > end
     */
    private int comp(int offset) {
        if (offset < start) {
            return -1;
        }
        if (offset >= end) {
            return 1;
        }
        return 0;
    }

    /**
     * Get tip for the desired offset. This assumes the array is well ordered: <br>
     * (1) tip[0].start >= 0 <br>
     * (2) tips[i].start < tips[i].end where 0 <= i < tips.length <br>
     * (3) tips[k].end < tips[k+1].start where 0<= k < tips.length-2
     * 
     * @param tips
     *            - Ordered array of tips
     * @param offset
     *            - The offset for desired tip
     * @return - the tip associated with offset or null if no tip found, tips is
     *         null or tips.length is 0
     */
    static String findTip(HeaderTextToolTip[] tips, int offset) {
        if (tips == null || tips.length == 0) {
            return null;
        }
        int top = 0;
        int bottom = tips.length;
        while (top < bottom) {
            int mid = (top + bottom) >> 1;
            switch (tips[mid].comp(offset)) {
            case -1:
                bottom = mid;
                break;
            case 0:
                return tips[mid].value;
            case 1:
                top = mid + 1;
                break;
            default:
                System.err
                        .println("Program error unexpected value from comp method.");
            }
        }
        return null;
    }
}
