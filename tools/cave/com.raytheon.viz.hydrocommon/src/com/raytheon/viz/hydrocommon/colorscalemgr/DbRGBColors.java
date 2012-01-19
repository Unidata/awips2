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
package com.raytheon.viz.hydrocommon.colorscalemgr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.ColorNameData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2008            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DbRGBColors {

    /**
     * Maps RGB color values to string name
     */
    private Map<RGB, String> colorToName;

    /**
     * list of all color names
     */
    private List<ColorNameData> colorNames;

    /**
     * instance of class
     */
    private static DbRGBColors instance;

    /**
     * mutex used to make sure two threads don't call init() at the same time,
     * may not be needed
     */
    private static String mutex = "";

    /**
     * Random number used to get random color
     */
    private Random r;

    private DbRGBColors() {
        try {
            List<ColorNameData> remove = new ArrayList<ColorNameData>();
            colorNames = HydroDBDataManager.getInstance().getData(
                    ColorNameData.class);
            colorToName = new HashMap<RGB, String>();
            r = new Random();
            for (ColorNameData colorName : colorNames) {
                RGB rgb = colorName.getColorValue();
                if (rgb != null) {
                    colorToName.put(rgb, colorName.getColorName());
                } else {
                    System.out
                            .println("No RGB for " + colorName.getColorName());
                    remove.add(colorName);
                }
            }

            for (ColorNameData colorName : remove) {
                colorNames.remove(colorName);
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    private static void init() {
        instance = new DbRGBColors();
    }

    /**
     * Returns the name of a color or null if none found
     * 
     * @param color
     * @return the name of the color
     */
    public static String getName(RGB color) {
        synchronized (mutex) {
            if (instance == null) {
                init();
            }
        }
        return instance.colorToName.get(color);
    }

    /**
     * get all colors from the database
     * 
     * @return list of all colors in database
     */
    public static List<ColorNameData> getAllColors() {
        synchronized (mutex) {
            if (instance == null) {
                init();
            }
        }

        return instance.colorNames;
    }

    /**
     * Use temporarily to populate dialog colors
     * 
     * @return a random RGB value from list of colors
     */
    public static RGB getRandomColor() {
        synchronized (mutex) {
            if (instance == null) {
                init();
            }
        }
        RGB rgb = null;
        while (rgb == null) {
            int index = Math.abs(instance.r.nextInt())
                    % instance.colorNames.size();
            rgb = instance.colorNames.get(index).getColorValue();
        }
        return rgb;
    }

    /**
     * get the index of a color in the list
     * 
     * @param color
     *            the color to be indexed
     * @return the index of the color
     */
    public static int getIndexOf(RGB color) {
        synchronized (mutex) {
            if (instance == null) {
                init();
            }
        }
        int rval = -1;
        for (int i = 0; i < instance.colorNames.size() && rval < 0; ++i) {
            if (instance.colorNames.get(i).getColorValue().equals(color)) {
                rval = i;
            }
        }
        return rval;
    }
}
