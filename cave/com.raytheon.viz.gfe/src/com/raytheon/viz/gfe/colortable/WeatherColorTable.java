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
package com.raytheon.viz.gfe.colortable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WxComposite;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2010            randerso     Initial creation
 * Jan 27, 2016 14453      yteng        Make color for same weather type
 *                                      consistent in different grids
 *
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class WeatherColorTable extends ColorTable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WeatherColorTable.class);

    private static final List<String> grayColors = new ArrayList<String>(
            Arrays.asList("grey85", "gray75", "gray60", "gray40", "grey90",
                    "grey50"));

    private Map<String, String> covNamesToPatterns;

    private Map<String, String> typeNamesToColors;

    private Map<String, String> typeIntenNamesToColors;

    private Map<String, String> genericNamesToColors = new HashMap<String, String>();

    public WeatherColorTable() {
        super();

        covNamesToPatterns = loadMap("WeatherCoverage_names",
                "WeatherCoverage_fillPatterns");

        typeNamesToColors = loadMap("WeatherType_names", "WeatherType_colors");

        typeIntenNamesToColors = loadMap("WeatherTypeInten_names",
                "WeatherTypeInten_colors");
    }

    private Map<String, String> loadMap(String keyKey, String valueKey) {
        Map<String, String> map = new HashMap<String, String>();

        String[] keys = prefs.getStringArray(keyKey);
        String[] values = prefs.getStringArray(valueKey);
        if (keys.length == values.length) {
            for (int i = 0; i < keys.length; i++) {
                map.put(keys[i], values[i]);
            }
        } else {
            statusHandler.handle(Priority.PROBLEM, keyKey + " and " + valueKey
                    + " not parallel.");
        }

        return map;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.colortable.ColorTable#map(com.raytheon.viz.gfe.core
     * .wxvalue.WxValue)
     */
    @Override
    public List<ImageAttr> map(WxValue wxValue) {
        List<ImageAttr> result = mapWeatherValue(wxValue);
        if (NOT_IN_TABLE_ENTRY.equals(result)) {
            allocateWeatherCTEntry(wxValue);
            result = mapWeatherValue(wxValue);
        }
        return result;
    }

    /**
     * @param wxValue
     */
    private void allocateWeatherCTEntry(WxValue wxValue) {
        WeatherKey wxkey = ((WeatherWxValue) wxValue).getWeatherKey();
        if (!wxkey.isValid()) {
            statusHandler.handle(Priority.VERBOSE,
                    "Attempt to allocate color entry for invalid wxkey:"
                            + wxkey);
            return; // if invalid, then return and do nothing
        }

        // decompose the weather key into composite types
        List<WxComposite> comps = wxkey.getCompositeTypes();

        List<ImageAttr> imageAttr = new ArrayList<ImageAttr>();

        // not complex weather
        if (comps.size() < 3) {
            for (WxComposite comp : comps) {
                // find a fill pattern based upon the coverage.
                String fillName = "WHOLE";
                // special case for empty coverage or <NoCov>
                if (comp.coverage().isEmpty()) {
                    fillName = covNamesToPatterns.get("<NoCov>");
                } else {
                    fillName = covNamesToPatterns.get(comp.coverage());
                }

                // find a color
                // first try an exact type/intensity match.
                String color = typeIntenNamesToColors
                        .get(comp.typesWithInten());
                if (color == null) {
                    color = typeNamesToColors.get(comp.types());
                }

                if (color == null) {
                    color = genericNamesToColors.get(comp.types());
                }

                if (color == null) {
                    color = getUniqueColor();
                    genericNamesToColors.put(comp.types(), color);
                }

                imageAttr.add(new ImageAttr(color, fillName));
            }
        } else {
            // Complex weather is always solid and a unique color
            imageAttr.add(new ImageAttr(getUniqueColor(), "WHOLE"));
        }

        // now add the entry to the ColorTable
        addEntry(wxValue, imageAttr);

        return;
    }

    private String getUniqueColor() {
        // List<String> userColors, usedColors, possibleColors, ctUsedColors;
        List<String> ctUsedColors = new ArrayList<String>();

        // all of the used colors in color table
        for (List<ImageAttr> imageAttrs : getEntries().values()) {
            for (ImageAttr imageAttr : imageAttrs) {
                ctUsedColors.add(imageAttr.getColorName());
            }
        }

        // get a list of colors the user would like us to try.
        String[] userColors = prefs.getStringArray("WeatherGeneric_colors");

        // these are possible choices for the new color.
        List<String> possibleColors = new ArrayList<String>(
                Arrays.asList(userColors));

        // these can not be used for the new color.
        List<String> usedColors = new ArrayList<String>();
        usedColors.addAll(typeNamesToColors.values());
        usedColors.addAll(typeIntenNamesToColors.values());
        usedColors.addAll(ctUsedColors);

        String newColor = firstFreeColor(usedColors, possibleColors);

        // if we couldn't find any available colors, then use grays
        if (newColor.equals("<None>")) {
            // pick a shade of gray
            possibleColors.clear();
            possibleColors.addAll(grayColors);

            newColor = firstFreeColor(usedColors, possibleColors);
        }

        // all of the grays are used, so use white
        if (newColor.equals("<None>")) {
            statusHandler
                    .handle(Priority.EVENTB,
                            "WeatherColorTable::getUniqueColor() : out of colors using 'White'");
            newColor = "White";
        }

        return newColor;
    }

    private String firstFreeColor(List<String> used, List<String> possible) {
        for (String color : possible) {
            if (!used.contains(color)) {
                return color;
            }
        }

        return "<None>";
    }

    private List<ImageAttr> mapWeatherValue(WxValue key) {
        List<ImageAttr> retVal = getEntries().get(key);

        if (retVal == null) {
            retVal = NOT_IN_TABLE_ENTRY;
        }
        return retVal;
    }

}
