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
package com.raytheon.viz.gfe.rsc;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.colortable.ColorTable;
import com.raytheon.viz.gfe.colortable.ColorTable.ImageAttr;
import com.raytheon.viz.gfe.colortable.DiscreteColorTable;
import com.raytheon.viz.gfe.colortable.WeatherColorTable;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * Utilities for displaying GFEResources correctly. Determines the fill color
 * and/or pattern for discrete data (Weather or Hazard grids).
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 9, 2009            njensen     Initial creation
 * Jan 9, 2013  15648     ryu         Update colormap when new discrete colrmap is selected.
 *
 * </pre>
 *
 * @author njensen
 * @version 1.0
 */

public class DiscreteDisplayUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiscreteDisplayUtil.class);

    private static ColorMap defaultSpectrum;

    private static synchronized ColorMap getDefaultSpectrum() {
        if (defaultSpectrum == null) {
            PythonPreferenceStore prefs = Activator.getDefault()
                    .getPreferenceStore();
            float minWaveLength = prefs
                    .getFloat("DefaultColorTable_leftWavelength");
            float maxWaveLength = prefs
                    .getFloat("DefaultColorTable_rightWavelength");
            int numColors = prefs.getInt("DefaultColorTable_numColors");
            minWaveLength = (minWaveLength == 0.0f) ? 380.0f : minWaveLength;
            maxWaveLength = (maxWaveLength == 0.0f) ? 660.0f : maxWaveLength;
            numColors = (numColors == 0) ? 50 : numColors;
            defaultSpectrum = new ColorMap("--DEFAULT--", numColors,
                    minWaveLength, maxWaveLength, false);
        }
        return defaultSpectrum;
    }

    private static WeatherColorTable weatherColorTable;

    private static Map<String, DiscreteColorTable> discreteColorTable;

    /**
     * Delete the discrete color map for parm. This should be done whenever the
     * color map in the resource is changed (to make getFillColor() load the new
     * color map), or when the parm is destroyed (to conserve storage).
     *
     * @param parm
     *            The discrete parm whose color map is to be deleted.
     */
    public static synchronized void deleteParmColorMap(Parm parm) {
        if (discreteColorTable == null) {
            discreteColorTable = new HashMap<String, DiscreteColorTable>();
        }
        String compositeName = parm.getParmID().getCompositeName();
        discreteColorTable.remove(compositeName);
    }

    /**
     * Given a parm, build a ColorMapParameters object for it.
     *
     * @param aparm
     *            The parm for which color map parameters should be built.
     * @return the ColorMapParameters for the parm.
     */
    public static ColorMapParameters buildColorMapParameters(Parm aparm) {
        ColorMapParameters colorMP = new ColorMapParameters();
        GridParmInfo info = aparm.getGridInfo();
        colorMP.setDataMax(info.getMaxValue());
        colorMP.setDataMin(info.getMinValue());

        String parmName = aparm.getParmID().getParmName();

        // Set colormap max,min. Use user preferences, if present.
        float maxColorTableValue = info.getMaxValue();
        float minColorTableValue = info.getMinValue();

        PythonPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        String maxColorTablePref = parmName + "_maxColorTableValue";
        if (prefs.contains(maxColorTablePref)) {
            maxColorTableValue = prefs.getFloat(maxColorTablePref);
            if (maxColorTableValue > info.getMaxValue()) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                String.format(
                                        "%s (%4.3G) is greater than the data type maximum (%4.3G), ignored.",
                                        maxColorTablePref, maxColorTableValue,
                                        info.getMaxValue()));
                maxColorTableValue = info.getMaxValue();
            }
        }

        String minColorTablePref = parmName + "_minColorTableValue";
        if (prefs.contains(minColorTablePref)) {
            minColorTableValue = prefs.getFloat(minColorTablePref);
            if (minColorTableValue < info.getMinValue()) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                String.format(
                                        "%s (%4.3G) is less than the data type minimum (%4.3G), ignored.",
                                        minColorTablePref, minColorTableValue,
                                        info.getMinValue()));
                minColorTableValue = info.getMinValue();
            }
        }

        if (maxColorTableValue < minColorTableValue) {
            statusHandler.handle(Priority.PROBLEM, String.format(
                    "%s (%4.3G) is less than %s (%4.3G), both are ignored.",
                    maxColorTablePref, maxColorTableValue, minColorTablePref,
                    minColorTableValue));
            maxColorTableValue = info.getMaxValue();
            minColorTableValue = info.getMinValue();
        }

        colorMP.setColorMapMax(maxColorTableValue);
        colorMP.setColorMapMin(minColorTableValue);

        String cmap = null;
        // Look for a color map in GFE preferences
        String dftColorTablePref = parmName + "_defaultColorTable";
        if (prefs.contains(dftColorTablePref)) {
            cmap = prefs.getString(dftColorTablePref);
            if (!cmap.contains("/")) {
                cmap = "GFE/" + cmap;
            }
        } else if (parmName.startsWith("haz")) {
            cmap = "GFE/TempHaz";
        } else if (GridType.WEATHER == aparm.getGridInfo().getGridType()) {
            // AWIPS 1 used the "Weather" colormap, but we don't have one.
            cmap = null;
        }

        IColorMap cm = null;
        if (cmap != null) {
            try {
                cm = ColorMapLoader.loadColorMap(cmap);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading colormap \"" + cmap
                                + "\"--using default", e);
            }
        }

        if (cm == null) {
            cm = getDefaultSpectrum();
        }

        colorMP.setColorMap(cm);

        String logFactorPref = parmName + "_LogFactor";
        if (prefs.contains(logFactorPref)) {
            float logFactor = prefs.getFloat(logFactorPref);
            colorMP.setLogFactor(logFactor);
        }

        if (info.getGridType() == GridType.DISCRETE) {
            List<String> keys = info.getDiscreteKeys();
            DataMappingPreferences dataMap = new DataMappingPreferences();
            for (int i=0; i < keys.size(); i++) {
                DataMappingEntry entry = new DataMappingEntry();
                entry.setPixelValue((double) i+0.5);
                entry.setLabel(keys.get(i));
                entry.setOperator("<");
                dataMap.addEntry(entry);
            }
            colorMP.setDataMapping(dataMap);
            colorMP.setDataMin(0);
            colorMP.setDataMax(keys.size()-1);
            colorMP.setColorMapMin(0);
            colorMP.setColorMapMax(keys.size()-1);
        }

        deleteParmColorMap(aparm);
        return colorMP;
    }

    public static List<ImageAttr> getFillAttributes(WxValue wxValue) {

        if (wxValue instanceof WeatherWxValue) {
            return getWeatherColorTable().map(wxValue);
        }

        if (wxValue instanceof DiscreteWxValue) {
            return getDiscreteColorTable(wxValue.getParm()).map(wxValue);
        }

        return ColorTable.NOT_IN_TABLE_ENTRY;
    }

    private static synchronized WeatherColorTable getWeatherColorTable() {
        if (weatherColorTable == null) {
            weatherColorTable = new WeatherColorTable();
        }

        return weatherColorTable;
    }

    private static synchronized DiscreteColorTable getDiscreteColorTable(
            Parm parm) {
        if (discreteColorTable == null) {
            discreteColorTable = new HashMap<String, DiscreteColorTable>();
        }

        String compName = parm.getParmID().getCompositeName();
        DiscreteColorTable colorTable = discreteColorTable.get(compName);
        if (colorTable == null) {
            DataManager dataManager = parm.getDataManager();
            ISpatialDisplayManager spatialDisplayManager = dataManager
                    .getSpatialDisplayManager();
            ResourcePair resourcePair = spatialDisplayManager
                    .getResourcePair(parm);
            AbstractVizResource<?, ?> resource = resourcePair.getResource();
            ColorMapParameters params = resource.getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            IColorMap colorMap = params.getColorMap();
            colorTable = new DiscreteColorTable(parm, colorMap);
            discreteColorTable.put(compName, colorTable);
        }

        return colorTable;
    }
}
