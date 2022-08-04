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
package com.raytheon.uf.viz.stats.ui;

/**
 * Color Manager
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2012            mpduff      Initial creation.
 *
 * </pre>
 *
 * @version 1.0
 */
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

public class ColorManager {

    /** Color list */
    private List<RGB> colorRGBs;

    /**
     * Constructor.
     */
    public ColorManager() {
        setupColors();
    }

    /**
     * Initialize the colors
     */
    private void setupColors() {
        List<RGB> tempColorRGBs = new ArrayList<RGB>();

        tempColorRGBs.add(new RGB(255, 0, 0)); // RED
        tempColorRGBs.add(new RGB(255, 165, 0)); // ORANGE
        tempColorRGBs.add(new RGB(0, 255, 0)); // GREEN
        tempColorRGBs.add(new RGB(255, 255, 0)); // YELLOW
        tempColorRGBs.add(new RGB(165, 42, 42)); // BROWN
        tempColorRGBs.add(new RGB(0, 255, 255)); // CYAN
        tempColorRGBs.add(new RGB(255, 0, 255)); // MAGENTA
        tempColorRGBs.add(new RGB(0, 0, 255)); // BLUE
        tempColorRGBs.add(new RGB(138, 43, 226)); // BLUEVIOLET
        tempColorRGBs.add(new RGB(238, 130, 238)); // VIOLET
        tempColorRGBs.add(new RGB(102, 205, 170)); // AQUAMARINE3
        tempColorRGBs.add(new RGB(139, 125, 107)); // BISQUE4
        tempColorRGBs.add(new RGB(238, 197, 145)); // BURLYWOOD2
        tempColorRGBs.add(new RGB(205, 205, 0)); // YELLOW3
        tempColorRGBs.add(new RGB(69, 139, 0)); // CHARTREUSE4
        tempColorRGBs.add(new RGB(255, 127, 80)); // CORAL
        tempColorRGBs.add(new RGB(100, 149, 237)); // CORNFLOWERBLUE
        tempColorRGBs.add(new RGB(0, 139, 139)); // CYAN4
        tempColorRGBs.add(new RGB(169, 169, 169)); // DARKGRAY
        tempColorRGBs.add(new RGB(139, 0, 139)); // DARKMAGENTA
        tempColorRGBs.add(new RGB(238, 118, 0)); // DARKORANGE2
        tempColorRGBs.add(new RGB(139, 0, 0)); // DARKRED
        tempColorRGBs.add(new RGB(233, 150, 122)); // DARKSALMON
        tempColorRGBs.add(new RGB(255, 20, 147)); // DEEPPINK
        tempColorRGBs.add(new RGB(0, 191, 255)); // DEEPSKYBLUE
        tempColorRGBs.add(new RGB(255, 255, 255)); // WHITE
        tempColorRGBs.add(new RGB(193, 205, 193)); // HONEYDEW3
        tempColorRGBs.add(new RGB(139, 58, 98)); // HOTPINK4
        tempColorRGBs.add(new RGB(144, 238, 144)); // LIGHTGREEN
        tempColorRGBs.add(new RGB(255, 182, 193)); // LIGHTPINK
        tempColorRGBs.add(new RGB(176, 196, 222)); // LIGHTSTEELBLUE
        tempColorRGBs.add(new RGB(205, 133, 0)); // ORANGE3
        tempColorRGBs.add(new RGB(139, 105, 105)); // ROSYBROWN4
        tempColorRGBs.add(new RGB(210, 180, 140)); // TAN
        tempColorRGBs.add(new RGB(0, 0, 0)); // BLACK

        colorRGBs = Collections.unmodifiableList(tempColorRGBs);

    }

    /**
     * Get the list of colors.
     *
     * @return the list of colors
     */
    public List<RGB> getColorRGBs() {
        return colorRGBs;
    }

    /**
     * Get the color at the provided index.
     *
     * @param index
     *            The index
     * @return the color for the index
     */
    public RGB getColorAtIndex(int index) {
        int indexRV = index % colorRGBs.size();

        RGB returnColor = colorRGBs.get(indexRV);

        if (returnColor == null) {
            return new RGB(0, 0, 0);
        }

        return returnColor;
    }

    // for testing
    public static void main(String[] args) {
        ColorManager cm = new ColorManager();

        List<RGB> colorRGBArray = cm.getColorRGBs();

        for (RGB rgb : colorRGBArray) {
            // String s = RGBColors.getColorName(rgb);
            // System.out.println(String.format("%20S", s) + "\t" +
            // rgb.toString());
            System.out.println(rgb.toString());
        }
    }
}
