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

package com.raytheon.uf.viz.core;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashMap;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.status.StatusConstants;

/**
 * This class reads in the rgb.txt file and creates a map of color names and the
 * associated RGB value.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 14 JUL 2008  1223       randerso    reworked for common access from all of CAVE
 * Apr 7, 2009             njensen     Added RGB to string features
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class RGBColors {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RGBColors.class);
    /**
     * rgb.txt file.
     */
    private static final String RGB_FILE_NAME = "colorfile/rgb.txt";

    private static class RGBEntry {
        RGB color;

        String name;
    }

    /**
     * Map of color names (key) and RGB color (value).
     */
    private static HashMap<String, RGBEntry> colorMap;

    /**
     * Maps of RGB colors to color names
     */
    private static HashMap<RGB, String> reverseMap;

    /**
     * Constructor.
     */
    private RGBColors() {
    }

    private static void init() {
        colorMap = new HashMap<String, RGBEntry>();
        reverseMap = new HashMap<RGB, String>();

        try {
            File file = PathManagerFactory.getPathManager().getStaticFile(
                    RGB_FILE_NAME);
            parseFile(file);
        } catch (Exception e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error loading rgbcolors", e);
        }
    }

    /**
     * Parse the RGB file. Put the color names and the RGB values in a map.
     */
    private static void parseFile(File file) {
        String fileLine;

        if (!file.exists()) {
            System.out.println("Cannot find file - " + file.getAbsolutePath());
            return;
        }

        try {
            FileReader fr = new FileReader(file);
            BufferedReader bufRead = new BufferedReader(fr);

            while ((fileLine = bufRead.readLine()) != null) {

                RGBEntry entry = new RGBEntry();
                String[] strArray = fileLine.trim().split("\\s+", 4);
                entry.name = strArray[3].trim();
                entry.color = new RGB(Integer.valueOf(strArray[0]), Integer
                        .valueOf(strArray[1]), Integer.valueOf(strArray[2]));
                colorMap.put(entry.name.toUpperCase(), entry);

                /*
                 * Check if the RGB key exists the text file contains several
                 * colors that have the same RGB value (for example: black &
                 * grey0 are the same RGB).
                 */
                if (reverseMap.containsKey(entry.color) == false) {
                    reverseMap.put(entry.color, entry.name);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static synchronized HashMap<String, RGBEntry> getColorMap() {
        if (colorMap == null) {
            init();
        }
        return colorMap;
    }

    private static synchronized HashMap<RGB, String> getReverseMap() {
        if (reverseMap == null) {
            init();
        }
        return reverseMap;
    }

    /**
     * Get the RGB value of the color name passed in.
     * 
     * @param colorName
     *            Name of the color.
     * @return The associated RGB value.
     */
    public static RGB getRGBColor(String colorName) {
        if (colorName.startsWith("#")) {
            return parseHexString(colorName);
        }

        RGBEntry entry = getColorMap().get(colorName.toUpperCase());
        if (entry == null) {
            return null;
        }
        return entry.color;
    }

    /**
     * Returns the name of a color mapped to the RGB values. If the name is not
     * found, returns the hex string of the color.
     * 
     * @param aColor
     *            an RGB
     * @return
     */
    public static String getColorName(RGB aColor) {

        String name = getReverseMap().get(aColor);
        if (name == null) {
            name = getHexString(aColor);
        }
        return name;
    }

    /**
     * Get the color name of the hex string passed in. If there is not a color
     * name associated then the hex string is returned.
     * 
     * @param hexColor
     *            Color as a hex string.
     * @return Color name.
     */
    public static String getColorName(String hexColor) {

        RGB rgb = parseHexString(hexColor);

        String name = getReverseMap().get(rgb);
        if (name != null) {
            return name;
        }

        return hexColor;
    }

    /**
     * Returns a hex string representing the color
     * 
     * @param aColor
     *            the hex string of the color
     * @return
     */
    public static String getHexString(RGB aColor) {
        return String.format("#%02x%02x%02x", aColor.red, aColor.green,
                aColor.blue);
    }

    private static RGB parseHexString(String s) {
        if (s.startsWith("#") && s.length() == 7) {
            try {
                int red = Integer.parseInt(s.substring(1, 3), 16);
                int green = Integer.parseInt(s.substring(3, 5), 16);
                int blue = Integer.parseInt(s.substring(5, 7), 16);
                return new RGB(red, green, blue);
            } catch (NumberFormatException e) {
                // fall through to throw
            }
        }

        throw new IllegalArgumentException(
                "\""
                        + s
                        + "\" is not a valid hexadecmial color string of the form (#rrggbb))");
    }
}
