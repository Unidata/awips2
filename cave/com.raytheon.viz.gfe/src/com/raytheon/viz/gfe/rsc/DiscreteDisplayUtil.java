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
import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.IConfigurationChangeListener;
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
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 09, 2009           njensen   Initial creation
 * Jan 09, 2013  15648    ryu       Update colormap when new discrete colormap
 *                                  is selected.
 * Jan 16, 2017  5976     bsteffen  Update Usage of ColorMapLoader
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Jun 06, 2018  7310     mapeters  Allow maxColorTableValue to be less than
 *                                  minColorTableValue
 * Jul 16, 2021  8591     randerso  Added extra entry for Local hazards to the
 *                                  ColorMapParameters.
 *
 * </pre>
 *
 * @author njensen
 */
public class DiscreteDisplayUtil {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiscreteDisplayUtil.class);

    private static ColorMap defaultSpectrum;

    private static WeatherColorTable weatherColorTable;

    private static Map<String, DiscreteColorTable> discreteColorTable;

    static {
        GFEPreference.addConfigurationChangeListener(
                new IConfigurationChangeListener() {

                    @Override
                    public void configurationChanged(String config) {
                        reset();
                    }
                });
    }

    private DiscreteDisplayUtil() {
        // don't allow instantiation as this class contains only static methods
    }

    private static synchronized void reset() {
        defaultSpectrum = null;
        weatherColorTable = null;
        discreteColorTable = null;
    }

    private static synchronized ColorMap getDefaultSpectrum() {
        if (defaultSpectrum == null) {
            float minWaveLength = GFEPreference
                    .getFloat("DefaultColorTable_leftWavelength", 380.0f);
            float maxWaveLength = GFEPreference
                    .getFloat("DefaultColorTable_rightWavelength", 660.0f);
            int numColors = GFEPreference.getInt("DefaultColorTable_numColors",
                    50);
            defaultSpectrum = new ColorMap("--DEFAULT--", numColors,
                    minWaveLength, maxWaveLength, false);
        }
        return defaultSpectrum;
    }

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
            discreteColorTable = new HashMap<>();
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

        String parmName = aparm.getParmID().getParmName();

        // Set colormap max,min. Use user preferences, if present.

        String maxColorTablePref = parmName + "_maxColorTableValue";
        float maxColorTableValue = GFEPreference.getFloat(maxColorTablePref,
                info.getMaxValue());
        if (maxColorTableValue > info.getMaxValue()) {
            statusHandler.handle(Priority.PROBLEM, String.format(
                    "%s (%4.3G) is greater than the data type maximum (%4.3G), ignored.",
                    maxColorTablePref, maxColorTableValue, info.getMaxValue()));
            maxColorTableValue = info.getMaxValue();
        }

        String minColorTablePref = parmName + "_minColorTableValue";
        float minColorTableValue = GFEPreference.getFloat(minColorTablePref,
                info.getMinValue());
        if (minColorTableValue < info.getMinValue()) {
            statusHandler.handle(Priority.PROBLEM, String.format(
                    "%s (%4.3G) is less than the data type minimum (%4.3G), ignored.",
                    minColorTablePref, minColorTableValue, info.getMinValue()));
            minColorTableValue = info.getMinValue();
        }

        colorMP.setColorMapMax(maxColorTableValue);
        colorMP.setColorMapMin(minColorTableValue);

        String cmap = null;
        // Look for a color map in GFE preferences
        String dftColorTablePref = parmName + "_defaultColorTable";
        if (GFEPreference.contains(dftColorTablePref)) {
            cmap = GFEPreference.getString(dftColorTablePref);
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
            } catch (ColorMapException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading colormap \"" + cmap
                                + "\"--using default",
                        e);
            }
        }

        if (cm == null) {
            cm = getDefaultSpectrum();
        }

        colorMP.setColorMap(cm);

        String logFactorPref = parmName + "_LogFactor";
        float logFactor = GFEPreference.getFloat(logFactorPref, -1.0f);
        colorMP.setLogFactor(logFactor);

        if (info.getGridType() == GridType.DISCRETE) {
            List<String> keys = info.getDiscreteKeys();
            DataMappingPreferences dataMap = new DataMappingPreferences();
            for (int i = 0; i < keys.size(); i++) {
                DataMappingEntry entry = new DataMappingEntry();
                entry.setPixelValue(i + 0.5);
                entry.setLabel(keys.get(i));
                entry.setOperator("<");
                dataMap.addEntry(entry);
            }
            DataMappingEntry local = new DataMappingEntry();
            local.setPixelValue(keys.size() - 0.5);
            local.setLabel("Local");
            local.setOperator(">");
            dataMap.addEntry(local);

            colorMP.setDataMapping(dataMap);
            colorMP.setColorMapMin(0);
            colorMP.setColorMapMax(keys.size() + 1);
        }

        deleteParmColorMap(aparm);
        return colorMP;
    }

    /**
     * @param wxValue
     * @return the fill attributes for wxValue
     */
    public static List<ImageAttr> getFillAttributes(WxValue wxValue) {

        if (wxValue instanceof WeatherWxValue) {
            return getWeatherColorTable().map(wxValue);
        }

        if (wxValue instanceof DiscreteWxValue) {
            return getDiscreteColorTable(wxValue.getParm()).map(wxValue);
        }

        throw new IllegalArgumentException(
                "WeatherWxValue or DiscreteWxValue expected, recieved: "
                        + wxValue.getClass().getSimpleName());
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
            discreteColorTable = new HashMap<>();
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
            ColorMapParameters params = resource
                    .getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            IColorMap colorMap = params.getColorMap();
            colorTable = new DiscreteColorTable(parm, colorMap);
            discreteColorTable.put(compName, colorTable);
        }

        return colorTable;
    }
}
