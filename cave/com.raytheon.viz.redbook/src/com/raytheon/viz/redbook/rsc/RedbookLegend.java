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
package com.raytheon.viz.redbook.rsc;

import java.util.HashSet;

/**
 * Represents the D2D Redbook Legend
 * 
 * The redbook legend is text in redbook that is placed in the upper left corner
 * of the display. Unfortunately, there is no flag in the data to indicate
 * whether a piece of data is legend or if it is regularly plotted text, so it
 * must be inferred.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 2, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class RedbookLegend {

    private int minX = Integer.MAX_VALUE;
    private int maxX = Integer.MIN_VALUE;
    private int minY = Integer.MAX_VALUE;
    private int maxY = Integer.MIN_VALUE;
    
    private boolean testAreaValid = false;
    private float testMinX = Integer.MAX_VALUE;
    private float testMaxX = Integer.MAX_VALUE;
    private float testMinY = Integer.MAX_VALUE;
    private float testMaxY = Integer.MAX_VALUE;
    
    private float DMNMX = 0;

    private HashSet<String> legendStrings = new HashSet<String>();

    public enum Type {
        GRAPHIC, LEGEND, IGNORE
    }

    public RedbookLegend() {
        
    }
    
    public void setMaxRefY(int maxRefY) {
        DMNMX = maxRefY / 30.0f;
    }

    /**
     * Make a note that graphics have been drawn at the specified location. This
     * affects the classification of text blocks as legends or graphics.
     * 
     * @param x
     * @param y
     */
    public void addCoordinate(int x, int y) {
        if (x < minX)
            minX = x;
        if (x > maxX)
            maxX = x;
        if (y < minY)
            minY = y;
        if (y > maxY)
            maxY = y;
        testAreaValid = false;
    }

    /**
     * This method determines whether a block of text should be plotted on the
     * legend or not.
     * <p>
     * Delegates most work to {@code isLegend1}. Checks for duplicate legend
     * strings that should be ignored.
     * <p>
     * Based on gisleg.f.
     * 
     * @param text
     *            text content of the text block
     * @param x
     *            x-coordinate of the text block
     * @param y
     *            y-coordinate of the text block
     * @return
     */
    public Type isLegend(String text, int x, int y) {
        Type type = isLegend1(text, x, y);
        if (type == Type.LEGEND) {
            if (!legendStrings.contains(text)) {
                legendStrings.add(text);
                return Type.LEGEND;
            } else
                return Type.IGNORE;
        }
        return type;
    }
    
    /**
     * This method determines whether a block of text should be plotted on the
     * legend or not.
     * <p>
     * Based on gisleg.f.
     * 
     * @param text
     *            text content of the text block
     * @param x
     *            x-coordinate of the text block
     * @param y
     *            y-coordinate of the text block
     * @return
     */
    private Type isLegend1(String text, int x, int y) {
        int nAscii = 0; // Excluding leading spaces
        int nWx = 0; // Number of weather symbols
        int ncWrk = 0; // Number of ASCII chars excluding leading and trailing
        // spaces
        int nDspc = 0;
        int nSpc = 0; // Number of internal spaces
        int nAlpha = 0; // Number of letters
        int nNum = 0; // number of digits
        int nCurFirst = 0; // Number of cursor movements before first printable
        // char
        int nCur = 0; // Total number of cursor movements
        int nFont = 0; // Number of font switches
        boolean number = true; // True if text could represent a number
        boolean period = false;
        boolean wxChars = false;

        StringBuilder printStrBuf = new StringBuilder(text.length());

        for (int i = 0; i < text.length(); ++i) {
            if (ncWrk > maxNChars)
                break;
            char c = text.charAt(i);
            if (c == 18) {
                wxChars = true;
                ++nFont;
                nDspc = 0;
                continue;
            } else if (c == 17) {
                wxChars = false;
                ++nFont;
                nDspc = 0;
                continue;
            } else if (c >= 8 && c <= 13) {
                ++nCur;
                if (nAscii == 0)
                    ++nCurFirst;
                continue;
            } else if (wxChars) {
                ++nWx;
                number = false;
                continue;
            } else if (c == ' ') {
                if (ncWrk != 0)
                    ++nDspc;
            } else if (c < 33 || c > 126) {
                number = false;
                continue;
            } else if (c >= '0' && c <= '9') {
                ++nNum;
            } else if ((c >= 64 && c <= 90) || (c >= 97 && c <= 122)) {
                c = Character.toUpperCase(c);
                ++nAlpha;
                number = false;
            } else if (c == '+' || c == '-') {
                if (nAscii != 0)
                    number = false;
            } else if (c == '.') {
                if (period)
                    number = false;
                period = true;
            } else
                number = false;
            if (c == ' ' && nAscii == 0 && nWx == 0)
                continue;
            ++nAscii;
            printStrBuf.append(c);
            if (c != ' ') {
                nSpc = nSpc + nDspc;
                nDspc = 0;
                ncWrk = nAscii;
                // printStrBuf.insert(ncWrk - 1, c);
                // printStrBuf.append(c);
            }
        }

        // If we have no printable characters at all, return as duplicate legend
        if (ncWrk == 0 && nWx == 0)
            return Type.IGNORE;

        // TODO: state via ===,==+,==-

        // ASCII numbers are usually in the graphic
        if (ncWrk < NUNC && number)
            return Type.GRAPHIC;

        String printStr = printStrBuf.toString().trim();

        for (String s : exactLegendStrings)
            if (s.equalsIgnoreCase(printStr))
                return Type.LEGEND;

        if ("SEE TEXT".equals(printStr))
            return Type.GRAPHIC;

        // Very short strings are in the graphic
        if (ncWrk + nWx <= minNChars)
            return Type.GRAPHIC;

        // Position of colon comparison
        if (minColonPos < ncWrk && nWx == 0
                && printStr.indexOf(':') + 1 >= minColonPos)
            return Type.LEGEND;

        if (ncWrk > 13 && printStr.indexOf("CRITICAL FIRE") != -1)
            return Type.GRAPHIC;

        if (ncWrk >= maxNChars)
            return Type.LEGEND;

        for (String s : containsLegendStrings)
            if (printStr.indexOf(s) != -1)
                return Type.LEGEND;

        // If a string has a lot of cursor movements at the front, it is in the
        // graphic.
        if (nCurFirst * 100 >= ncWrk * t0cp)
            return Type.GRAPHIC;

        if (ncWrk >= TMLG && nNum >= 2 && nSpc > 0)
            for (String s : dayMonAbbrev) {
                int i = printStr.indexOf(s);
                if (i != -1)
                    if ((i == 0 || !Character.isLetter(printStr.charAt(i - 1)))
                            && ((i + s.length()) >= printStr.length() || !Character
                                    .isLetter(printStr.charAt(i + s.length()))))
                        return Type.LEGEND;
            }

        for (String s : prefixLegendStrings)
            if (printStr.indexOf(s) == 0)
                return Type.LEGEND;

        updateTestArea();
        
        if (x > testMinX && x < testMaxX && y > testMinY && y < testMaxY) {
            boolean plain = nFont == 0 && ! number;
            if (ncWrk >= GNC && plain && (nCurFirst * 100) < (ncWrk * GCP))
                return Type.LEGEND;
            else
                return Type.GRAPHIC;
        } else if (nSpc > T2NS && nAlpha > T2NA)
            return Type.LEGEND;
        else if (nSpc == 0 && nFont > 0)
            return Type.GRAPHIC;
        else
            return Type.LEGEND;
    }

    private void updateTestArea() {
        if (! testAreaValid) {
            float x0 = (float)(minX + maxX) / 2; 
            float dx = (float)(maxX - minX) / 2;
            float y0 = (float)(minY + maxY) / 2; 
            float dy = (float)(maxY - minY) / 2;
            if (dx < dy)
                dx = dy;
            else if (dy < dx * 0.75)
                dy = (float)(dx * 0.75);
            dx += DMNMX;
            testMinX = x0 - dx;
            testMaxX = x0 + dx;
            dy += DMNMX;
            testMinY = y0 - dy;
            testMaxY = y0 + dy;

            testAreaValid = true;
        }
    }

    // Parameters and data used by isLegend
    private static final int NUNC = 6;

    private static final int maxNChars = 25;

    private static final int minNChars = 3;

    private static final int minColonPos = 3;

    private static final int t0cp = 50;

    private static final int TMLG = 8;

    private static final int T2NS = 0;

    private static final int T2NC = 5;

    private static final int T2NA = 0; // AKA T2NC?
    
    private static final int GNC = 8;
    
    private static final int GCP = 15;

    private static final String[] exactLegendStrings = { "NWS", "A POINT",
            "SOILS" };

    private static final String[] containsLegendStrings = { "NOAA", "(FT)",
            "ANALY", "NOCAST", "FORECAST" };

    private static final String[] prefixLegendStrings = { "ISSUED", "VALID",
            "THRU" };

    private static final String[] dayMonAbbrev = { "JAN", "FEB", "MAR", "APR",
            "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "SUN",
            "MON", "TUE", "WED", "THU", "FRI", "SAT" };
}
