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
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.FIELDS;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.StyleRuleset;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * FFMPColor Utility
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/29/09      2152       D. Hladky   Initial release
 * 05/21/12     DR 14833    G. Zhang    Error handling for invalid cmap 
 * Apr 26, 2013 1954        bsteffen    Minor code cleanup throughout FFMP.
 * Jun 10, 2013 2075        njensen     Improved init time
 * Sep 05, 2013 2051        mnash       Moved style rule instantiation so that we don't get NPEs
 * Sep 28, 2015 4756        dhladky     Multiple guidance style rules for FFMP.
 * Dec 10, 2015 4834        njensen     Use non-deprecated ColorMapLoader
 * Mar 27, 2018 7029        njensen     Split constructor logic to separate methods
 * Apr 04, 2018 6889        njensen     Add ability to get style rule
 * Apr 11, 2018 6070        mduff       Clean up
 * 
 * </pre>
 * 
 * @author dhladky
 */

public class FFMPColorUtils {

    /** Status handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPColorUtils.class);

    private ColorMapParameters colormapparams = null;

    /** FFMP field to be drawn **/
    private FIELDS field = null;

    private TreeMap<Double, String> hourColorMapMap = new TreeMap<>();

    private StyleRule styleRule = null;

    // DR 14833: used when no colormap found
    private static final String DEFAULT_COLORMAP = "ffmp/qpe";

    // DR 14833: used when paramname not matching colormap name found
    private static final String DEFAULT_PARAMNAME = "qpe";

    /**
     * Set up FFMP Color maps
     * 
     * @param field
     * @param time
     * @param ffgName
     * @param tableLoad
     * @param colorMapName
     */
    public FFMPColorUtils(FIELDS field, double time, String ffgName,
            boolean tableLoad, String colorMapName) {
        this.field = field;

        styleRule = findStyleRule(time, ffgName, tableLoad);
        if (styleRule == null) {
            throw new IllegalStateException(
                    "Cannot find imagery style rule for " + ffgName);
        }

        IColorMap colorMap = null;
        if (colorMapName != null) {
            try {
                colorMap = ColorMapLoader.loadColorMap(colorMapName);
            } catch (ColorMapException e) {
                statusHandler.warn("Unable to load color map " + colorMap
                        + ". Trying to use defaults.", e);
            }
        }

        if (colorMap == null) {
            colorMap = getColorMapFromStyleRules(styleRule);
        }

        colormapparams = new ColorMapParameters();
        colormapparams.setColorMap(colorMap);
        colormapparams
                .setDisplayUnit(((ImagePreferences) styleRule.getPreferences())
                        .getDisplayUnits());
        colormapparams
                .setDataMapping(((ImagePreferences) styleRule.getPreferences())
                        .getDataMapping());

        colormapparams.setColorMapMin(0);
        colormapparams.setColorMapMax(255);
    }

    protected StyleRule findStyleRule(double time, String ffgName,
            boolean tableLoad) {
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY,
                    getMatchCriteria(time, ffgName, tableLoad));
            // Different style rules for different guidance sources selected
            if (sr == null) {
                // Could be a new source, load the default guidance map.
                statusHandler.warn("No style rule matching " + ffgName
                        + " in ffmpImageryStyleRules.xml, loading default!");
                // try again
                sr = StyleManager.getInstance().getStyleRule(
                        StyleManager.StyleType.IMAGERY,
                        getMatchCriteria(time, "", tableLoad));
            }
        } catch (StyleException e) {
            statusHandler
                    .error("Error getting style rule: ffgName = " + ffgName, e);
        }
        return sr;
    }

    public StyleRule getStyleRule() {
        return styleRule;
    }

    protected IColorMap getColorMapFromStyleRules(StyleRule sr) {
        IColorMap colorMap = null;
        String colorMapFile = null;

        if (sr != null) {
            try {
                colorMapFile = ((ImagePreferences) sr.getPreferences())
                        .getDefaultColormap();
                colorMap = ColorMapLoader.loadColorMap(colorMapFile);
            } catch (ColorMapException e) {
                statusHandler.error(
                        "Error loading ColorMap file: " + colorMapFile, e);
                colorMap = getDefaultColorMap();
            }
        } else {
            colorMap = getDefaultColorMap();
        }

        return colorMap;
    }

    public ColorMapParameters getColorMapParameters() {
        return colormapparams;
    }

    /**
     * Gets the ColorMap
     * 
     * @return
     */
    private IColorMap getColorMap() {
        return getColorMapParameters().getColorMap();
    }

    /**
     * convert color
     * 
     * @param color
     * @return
     */
    private static RGB convert(Color color) {

        if (color != null) {
            int blue = (int) (color.getBlue() * 255.0f);
            int green = (int) (color.getGreen() * 255.0f);
            int red = (int) (color.getRed() * 255.0f);
            return new RGB(red, green, blue);
        }

        return null;
    }

    /**
     * the check and assignment of colors
     * 
     * @param value
     * @return rgb value
     * @throws VizException
     */
    protected RGB colorByValue(double valueArg) throws VizException {

        int ret = 0;
        RGB rgb = null;

        if (Double.isNaN(valueArg)) {
            rgb = convert(getColorMap().getColors().get(ret));
            return rgb;
        }

        double value = (Math.round(valueArg * 100.0)) / 100.0;

        if (field == FIELDS.DIFF) {
            Color color = colormapparams.getColorByValue((float) value);
            rgb = convert(color);
            return rgb;

        } else if (value >= 0.005) {
            Color color = colormapparams.getColorByValue((float) value);
            rgb = convert(color);
            return rgb;
        }

        List<Color> colors = getColorMap().getColors();

        if (ret >= colors.size()) {
            ret = colors.size() - 1;
        }

        if (ret < 0) {
            ret = 0;
        }

        rgb = convert(colors.get(ret));
        return rgb;
    }

    /**
     * Get and load the style rule
     * 
     * @return
     */
    private ParamLevelMatchCriteria getMatchCriteria(double time,
            String ffgName, boolean tableLoad) {
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        List<String> paramList = new ArrayList<>();

        if (field == FFMPRecord.FIELDS.QPE || field == FIELDS.QPF
                || field == FIELDS.GUIDANCE) {
            // qpe cases
            if (tableLoad) {
                String qpeName = FIELDS.QPE.getFieldName()
                        + determineQpeToUse(time);

                paramList.add(qpeName);
            } else {
                if (field == FIELDS.GUIDANCE) {
                    paramList.add(FIELDS.GUIDANCE.getFieldName() + ffgName);
                } else {
                    paramList.add(FIELDS.QPE.getFieldName());
                }
            }
        } else if (field == FIELDS.RATIO) {
            // ratio case
            paramList.add(FIELDS.RATIO.getFieldName());
        } else if (field == FIELDS.DIFF) {
            // rate, ratio and diff cases
            paramList.add(FIELDS.DIFF.getFieldName());
        } else if (field == FFMPRecord.FIELDS.RATE) {
            // rate case
            paramList.add(FIELDS.RATE.getFieldName());
        }

        match.setParameterName(paramList);

        return match;
    }

    private String determineQpeToUse(double time) {
        parseFileNames(getQpeColorMapFiles());
        String qpeHourToUse = determineColorMap(time);

        return qpeHourToUse;
    }

    private void parseFileNames(List<String> fileArray) {
        double hour = 0.0;
        for (String fn : fileArray) {
            hour = 0.0;

            if (fn.indexOf("ffmp/qpe") >= 0) {

                String name1 = fn.replaceAll("colormaps/ffmp/qpe", "");
                String name2 = name1.replaceAll(".cmap", "");

                if (name2.length() == 0) {
                    hourColorMapMap.put(0.0, fn);
                } else {
                    hour = Double.parseDouble(name2);
                    hourColorMapMap.put(hour, fn);
                }
            }
        }
    }

    private String determineColorMap(double durHour) {
        String qpeHourToUse = null;
        for (Double dblHour : hourColorMapMap.keySet()) {
            if (durHour < dblHour) {
                break;
            }

            if (dblHour != 0.0) {

                // create a tmp value that will store the integer
                // part of the time. Example: 6.25 --> 6
                int intHour = dblHour.intValue();

                /*
                 * If the difference between double hour and int hour is greater
                 * than 0, set the qpeHourToUse to the double value.
                 * 
                 * If the difference between double hour and int hour is zero,
                 * then set qpeHourToUse to the int hour value.
                 * 
                 * The reason for this is that a color map name would be
                 * "qpe6.cmap" not qpe6.0.cmap. However, if you have an hour
                 * with a decimal greater than zero then qpe4.5.cmap would be
                 * valid and the qpeHourToUse would be 4.5
                 */
                if ((dblHour - intHour) > 0) {
                    qpeHourToUse = String.valueOf(dblHour);
                } else {
                    qpeHourToUse = String.valueOf(intHour);
                }

            }
        }

        /*
         * If qpeHourToUse is null then set qpeHourToUse to "". qpeHourToUse
         * will be added to the qpe file name to determine the color map to use.
         */
        if (qpeHourToUse == null) {
            qpeHourToUse = "";
        }

        return qpeHourToUse;
    }

    private List<String> getQpeColorMapFiles() {
        List<String> colormaps = new ArrayList<>();
        ILocalizationFile[] files = ColorMapLoader.listColorMapFiles("ffmp");
        for (ILocalizationFile file : files) {
            String fn = file.getPath();
            if (fn.indexOf("qpe") > -1) {
                colormaps.add(fn);
            }
        }
        return colormaps;
    }

    /**
     * DR 14833: Error handling for the following: when a user modified the
     * ffmpImageryStyleRules.xml file without adding the related qpeX.cmap and
     * for a user made error like: qpe6/qpe4.cmap then default qpe/qpe.cmap
     * used.
     * 
     */
    protected IColorMap getDefaultColorMap() {
        IColorMap cxml = null;

        /*
         * see parseFileNames(): colormap_name is "0.0" or qpe+key+".cmap"
         * double hour = hourColorMapMap.firstKey(); String cmapHour = (
         * hour==0.0 ? "" : String.valueOf(hour) );
         * System.out.println("FFMPColorUtils.getDefaultColorMap() cmapHour: "
         * +cmapHour );
         */

        /*
         * Loop through all StyleRules to get the default. In
         * StyleManager.loadRules(StyleType), all levels(not only USER)
         * StyleRule loaded. So it is guaranteed the default can be loaded.
         */

        StyleRuleset srs = StyleManager.getInstance()
                .getStyleRuleSet(StyleManager.StyleType.IMAGERY);
        StyleRule sr = null;
        for (StyleRule srl : srs.getStyleRules()) {
            String pn = "", cm = "";
            try {
                pn = ((ParamLevelMatchCriteria) srl.getMatchCriteria())
                        .getParameterNames().get(0);
                cm = ((ImagePreferences) srl.getPreferences())
                        .getDefaultColormap();
            } catch (Exception e) {
                continue;
            }

            if (DEFAULT_PARAMNAME.equalsIgnoreCase(pn)
                    && DEFAULT_COLORMAP.equalsIgnoreCase(cm)) {
                sr = srl;
                break;
            }

        }

        // get the colormapfile name
        String colormapfile = ((ImagePreferences) sr.getPreferences())
                .getDefaultColormap();

        // load the colormap
        try {
            cxml = ColorMapLoader.loadColorMap(colormapfile);
        } catch (ColorMapException e) {
            statusHandler.error("Error loading colormap " + colormapfile, e);
        }

        return cxml;
    }
}
