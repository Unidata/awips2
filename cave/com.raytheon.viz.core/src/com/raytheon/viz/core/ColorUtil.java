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
package com.raytheon.viz.core;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;

/**
 * Provides color utilities
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/2008   #878       chammack    Broke functionality away from GraphUtil
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ColorUtil {

    private static String COLOR_PREF = "defaultGraphicColor";

    public static final RGB DEFAULT_ITEM_COLOR = new RGB(255, 255, 255);

    public static final RGB GREY = new RGB(128, 128, 128);

    public static final RGB WHITE = new RGB(255, 255, 255);

    public static final RGB BLACK = new RGB(0, 0, 0);

    private static RGB colors[];

    /**
     * Adds a list of RGB colors to a color count map
     * 
     * @param colorList
     * @param colorCountMap
     */
    private static void addColorListColorMap(List<RGB> colorList,
            Map<RGB, Integer> colorCountMap) {
        for (RGB color : colorList) {
            int count = new Integer(0);
            if (colorCountMap.containsKey(color)) {
                count = colorCountMap.get(color);
            }
            colorCountMap.put(color, count + 1);
        }
    }

    private static synchronized void checkAndLoadColors() {
        if (colors != null) {
            return;
        }

        HierarchicalPreferenceStore prefs = (HierarchicalPreferenceStore) CorePlugin
                .getDefault().getPreferenceStore();

        String[] names = prefs.getStringArray(COLOR_PREF);
        colors = new RGB[names.length];
        int i = 0;
        for (String name : names) {
            colors[i++] = RGBColors.getRGBColor(name);
        }
    }

    /**
     * @return a map of preset colors all initialized to a count of zero.
     */
    private static Map<RGB, Integer> getColorCountMap() {
        checkAndLoadColors();

        // keep track of the number of times a specific color is used
        Map<RGB, Integer> colorCountMap = new LinkedHashMap<RGB, Integer>();

        for (RGB color : colors) {
            colorCountMap.put(color, 0);
        }
        return colorCountMap;
    }

    /**
     * @param colorCountMap
     * @return the least used color in the color count map.
     */
    private static RGB getLeastUsed(Map<RGB, Integer> colorCountMap) {
        // This algorithm borrowed from GFE
        // Find the "least used" color in the map
        int minCount = Integer.MAX_VALUE;
        RGB leastUsedColor = null;
        for (Map.Entry<RGB, Integer> entry : colorCountMap.entrySet()) {
            if (entry.getValue() < minCount) {
                minCount = entry.getValue();
                leastUsedColor = entry.getKey();
            }
        }

        return leastUsedColor;
    }

    /**
     * Return the next unused color in the preset list based on the resources in
     * a descriptor
     * 
     * If no unused color is available, return a "least used" color.
     * 
     * @param descriptor
     *            the descriptor to use
     * @return the color
     */
    public static RGB getNewColor(IDescriptor descriptor, ResourcePair pair) {

        Map<RGB, Integer> existingColorMap = getColorCountMap();

        addColorListColorMap(getUsedColors(descriptor), existingColorMap);
        if (pair != null
                && pair.getLoadProperties() != null
                && (pair.getLoadProperties().getCapabilities()
                        .hasCapability(ImagingCapability.class) || pair
                        .getLoadProperties().getCapabilities()
                        .hasCapability(ColorMapCapability.class))) {
            if (existingColorMap.containsKey(WHITE)) {
                return getLeastUsed(existingColorMap);
            } else {
                existingColorMap.put(WHITE, 1);
                return WHITE;
            }
        }

        return getLeastUsed(existingColorMap);
    }

    public static RGB getNewColor(IDescriptor descriptor) {
        return getNewColor(descriptor, null);
    }

    /**
     * @param displayPanes
     * @return a unique preset color not used in any of the given displayPanes.
     *         if all preset colors have been used the least used preset color
     *         will be returned.
     */
    public static RGB getNewColor(IDisplayPane[] displayPanes) {
        return getNewColor(displayPanes, null, null);

    }

    public static RGB getNewColor(IDisplayPane[] displayPanes,
            IDescriptor descriptor, ResourcePair pair) {
        Map<RGB, Integer> colorCountMap = getColorCountMap();

        for (IDisplayPane displayPane : displayPanes) {
            // Add this panes colors to the list
            addColorListColorMap(getUsedColors(displayPane.getDescriptor()),
                    colorCountMap);
            // Now try to find a matching resource to steal a color from
            int index = displayPane.getDescriptor().getResourceList()
                    .indexOf(pair);
            if (index < 0) {
                continue;
            }
            ResourcePair oldRp = displayPane.getDescriptor().getResourceList()
                    .get(index);
            if (!oldRp.getLoadProperties().getCapabilities()
                    .hasCapability(ColorableCapability.class)) {
                continue;
            }
            final ColorableCapability cap = oldRp
                    .getLoadProperties()
                    .getCapabilities()
                    .getCapability(oldRp.getResourceData(),
                            ColorableCapability.class);
            pair.getLoadProperties().getCapabilities().addCapability(cap);
            // Since they now share the capability we need to
            // forward the change event.
            final AbstractResourceData rd = pair.getResourceData();
            oldRp.getResourceData().addChangeListener(
                    new IResourceDataChanged() {
                        @Override
                        public void resourceChanged(ChangeType type,
                                Object object) {
                            if (object == cap) {
                                rd.fireChangeListeners(type, object);
                            }
                        }
                    });
            return cap.getColor();
        }

        // If this is the first image resource in the descriptor it should be
        // white
        if (pair != null
                && descriptor != null
                && pair.getLoadProperties() != null
                && (pair.getLoadProperties().getCapabilities()
                        .hasCapability(ImagingCapability.class) || pair
                        .getLoadProperties().getCapabilities()
                        .hasCapability(ColorMapCapability.class))
                && !getUsedColors(descriptor).contains(WHITE)) {
            return WHITE;
        }

        return getLeastUsed(colorCountMap);

    }

    /**
     * Return a new color based on an index
     * 
     * @param index
     *            the color index to return
     * @return
     */
    public static RGB getNewResourceColor(int index) {
        checkAndLoadColors();
        if (index >= colors.length) {
            index = index % colors.length;
        }

        return colors[index];
    }

    /**
     * Load and return the array of RGB color presets.
     * 
     * @return RGB array
     */
    public static RGB[] getResourceColorPresets() {
        checkAndLoadColors();
        return colors;
    }

    /**
     * @param descriptor
     * @return a list of all currently used colors in the given descriptor. Any
     *         colorable resource present in the given descript is added.
     */
    private static List<RGB> getUsedColors(IDescriptor descriptor) {

        List<RGB> usedColors = new ArrayList<RGB>();

        ResourceList rl = descriptor.getResourceList();
        synchronized (rl) {
            for (ResourcePair rp : rl) {
                if (rp.getLoadProperties().getCapabilities()
                        .hasCapability(ColorableCapability.class)) {
                    RGB color = rp
                            .getLoadProperties()
                            .getCapabilities()
                            .getCapability(rp.getResourceData(),
                                    ColorableCapability.class).getColor();
                    usedColors.add(color);
                }
            }
        }

        return usedColors;
    }

}
