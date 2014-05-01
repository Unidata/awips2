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
package com.raytheon.uf.viz.core.rsc;

/**
 * Different display types(Image, Contour, Streamline etc..).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           Ticket#    Engineer    Description
 * ------------   ---------- ----------- --------------------------
 * July 17, 2008   #1280          S. Manoj     Initial creation
 * 
 * </pre>
 * 
 * @author smanoj
 * @version 1.0
 */
public enum DisplayType {
    IMAGE, CONTOUR, STREAMLINE, BARB, ARROW, ICON, DUALARROW;

    /**
     * 
     * @param displayType
     * @return the abbreviation string for the given displayType
     */
    public static String getAbbreviation(DisplayType displayType) {
        switch (displayType) {
        case IMAGE:
            return "Img";
        case CONTOUR:
        case BARB:
            // default display types have no abbreviation
            return "";
        case STREAMLINE:
            return "Strmlns";
        case ARROW:
            return "Arrows";
        }
        return null;
    }

    /**
     * 
     * @param displayType
     * @return true if the given display type is the default display Type
     */
    public static boolean isDefault(DisplayType displayType) {
        switch (displayType) {
        case CONTOUR:
        case BARB:
            return true;
        }
        return false;
    }

    /**
     * 
     * @param displayType
     * @return true if the given displayType is a vector DisplayType, false
     *         otherwise.
     */
    public static boolean isVector(DisplayType displayType) {
        switch (displayType) {
        case BARB:
        case STREAMLINE:
        case ARROW:
            return true;
        }
        return false;
    }
}
