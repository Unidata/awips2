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
package com.raytheon.uf.common.dataplugin.warning.portions;

/**
 * Port of A1 constants applied to a grid to determine county or zone portions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2013  2177       jsanchez     Initial creation
 * Sep 22, 2013 2177       jsanchez     Updated EW_MASK.
 * Dec  4, 2013 2604       jsanchez     Moved out of viz.warngen.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class CoverageConstants {

    public static final int XSOUTH = 0x0001;

    public static final int SOUTH = 0x0002;

    public static final int NORTH = 0x0040;

    public static final int XNORTH = 0x0080;

    public static final int CENTER_NS = 0x0018;

    public static final int CENTRAL_NS = 0x0024;

    public static final int XWEST = 0x0100;

    public static final int WEST = 0x0200;

    public static final int EAST = 0x4000;

    public static final int XEAST = 0x8000;

    public static final int CENTER_EW = 0x1800;

    public static final int CENTRAL_EW = 0x2400;

    public static final int SOUTHERN = 0x0003;

    public static final int NORTHERN = 0x00C0;

    public static final int WESTERN = 0x0300;

    public static final int EASTERN = 0xC000;

    public static final int SOUTHSIDE = 0x000F;

    public static final int NORTHSIDE = 0x00F0;

    public static final int WESTSIDE = 0x0F00;

    public static final int EASTSIDE = 0xF000;

    public static final int EXTREME = 0x8181;

    public static final int NOT_EXTREME = 0x7E7E;

    public static final int EXTREME_NS = 0x0081;

    public static final int EXTREME_EW = 0x8100;

    public static final int CENTRAL = 0x2424;

    public static final int CENTER = 0x1818;

    public static final int NOT_CENTRAL = 0xC3C3;

    public static final int NORTH_SOUTH = 0x00FF;

    public static final int EAST_WEST = 0xFF00;

    public static final int NNE = 0x0001;

    public static final int ENE = 0x0002;

    public static final int ESE = 0x0004;

    public static final int SSE = 0x0008;

    public static final int SSW = 0x0010;

    public static final int WSW = 0x0020;

    public static final int WNW = 0x0040;

    public static final int NNW = 0x0080;

    public static final int EXTREME_YES = 0xFFFF00;

    public static final int EXTREME_NO = 0x00FF;

    public static final int EXTREME_CORNER = 0xFF0000;

    public static int[] NS_MASK = new int[256];

    public static int[] EW_MASK = new int[256];

    static {
        int i;

        NS_MASK[0] = 0;
        for (i = 1; i < 256; i++) {
            if (i < 87) {
                NS_MASK[i] = XSOUTH | SOUTH;
            } else if (i > 167) {
                NS_MASK[i] = XNORTH | NORTH;
            } else if (i < 106) {
                NS_MASK[i] = SOUTH;
            } else if (i > 148) {
                NS_MASK[i] = NORTH;
            } else if (i < 118) {
                NS_MASK[i] = CENTRAL_NS | SOUTH;
            } else if (i > 138) {
                NS_MASK[i] = CENTRAL_NS | NORTH;
            } else if (i < 127) {
                NS_MASK[i] = CENTER_NS | CENTRAL_NS | SOUTH;
            } else if (i > 127) {
                NS_MASK[i] = CENTER_NS | CENTRAL_NS | NORTH;
            } else {
                NS_MASK[i] = CENTER_NS | CENTRAL_NS;
            }
        }

        EW_MASK[0] = 0;
        for (i = 1; i < 256; i++) {
            if (i < 87) {
                EW_MASK[i] = XWEST | WEST;
            } else if (i > 167) {
                EW_MASK[i] = XEAST | EAST;
            } else if (i < 106) {
                EW_MASK[i] = WEST;
            } else if (i > 148) {
                EW_MASK[i] = EAST;
            } else if (i < 118) {
                EW_MASK[i] = CENTRAL_EW | WEST;
            } else if (i > 138) {
                EW_MASK[i] = CENTRAL_EW | EAST;
            } else if (i < 127) {
                EW_MASK[i] = CENTER_EW | CENTRAL_EW | WEST;
            } else if (i > 127) {
                EW_MASK[i] = CENTER_EW | CENTRAL_EW | EAST;
            } else {
                EW_MASK[i] = CENTER_EW | CENTRAL_EW;
            }
        }
    }

    private CoverageConstants() {

    }
}
