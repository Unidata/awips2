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
package com.raytheon.viz.volumebrowser.datacatalog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataplugin.grid.util.GridLevelTranslator;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.dataplugin.level.util.LevelUtilities;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.IStyleType;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

/**
 * Abstract data catalog implementation for shared functionality between data
 * catalogs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------------
 * Oct 06, 2009  2987     jelkins   Initial creation
 * Oct 21, 2009  1711     bsteffen  Updated Baseline and Points to use new
 *                                  ToolsDataManager
 * Jan 30, 2012  14308    dfriedma  Use correct style for arrow types.
 * Jul 31, 2012  875      rferrel   Now uses points.
 * Feb 21, 2013  1617     bsteffen  fixed vb sounding point selection for
 *                                  points which contain the word Point
 * May 03, 2013  14824    mgamazay  Added alterProductParameters method
 * Aug 20, 2013  2259     bsteffen  Delete old skewt plugin.
 * Sep 06, 2013  2251     mnash     Move graph prefs style type to graph plugin
 * Jan 30, 2014  2725     ekladstr  updated exception handling during move of
 *                                  derived parameters to common
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * Aug 03, 2015  3861     bsteffen  Extract point/line methods to PointLineUtil
 *                                  Move resource creation to ProductCreators
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public abstract class AbstractDataCatalog implements IDataCatalog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractDataCatalog.class);

    /**
     * 
     * @return a list of plugin Names this catalog supports
     */
    protected abstract String[] getPlugins();

    /**
     * 
     * @return the default plugin to use in any case
     */
    protected String getDefaultPlugin() {
        return getPlugins()[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog#getName(com.raytheon
     * .viz.volumebrowser.datacatalog.IDataCatalogEntry,
     * com.raytheon.uf.viz.core.rsc.DisplayType)
     */
    @Override
    public String getName(IDataCatalogEntry catalogEntry,
            DisplayType displayType) {

        StringBuilder name = new StringBuilder();

        name.append(catalogEntry.getSelectedData().getSourcesText());

        if (catalogEntry.getDialogSettings().getViewSelection() == ViewMenu.TIMESERIES) {
            name.append(" "
                    + catalogEntry.getDialogSettings().getPointsSelection()
                            .getName());
        }

        name.append(String.format(" %s %s", catalogEntry.getSelectedData()
                .getPlanesText(), catalogEntry.getSelectedData()
                .getFieldsText()));
        List<DisplayType> displayTypes = catalogEntry.getSelectedData()
                .getDisplayTypes();
        String displayTypeAbbreviation = null;
        if (displayTypes == null || displayTypes.isEmpty()
                || !displayTypes.get(0).equals(displayType)) {
            // If this is the first display type in the list then it is the
            // default and should not show up in the legend.
            displayTypeAbbreviation = DisplayType.getAbbreviation(displayType);
        }
        if (displayTypeAbbreviation != null
                && displayTypeAbbreviation.length() > 0) {
            name.append(" " + displayTypeAbbreviation);
        }

        String units = getDisplayUnit(catalogEntry, displayType);

        name.append(" (");
        if (units != null) {
            name.append(units);
        }
        name.append(")");

        return name.toString();
    }

    /**
     * Get the display units for the given catalog entry
     * 
     * @param catalogEntry
     * @param displayType
     *            the units are different for some products depending on how
     *            they are displayed
     * @return the unit string, null if no unit can be determined. an empty
     *         string if no unit applies for this product.
     */
    protected String getDisplayUnit(IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        String displayUnit = null;

        // soundings don't have a displayUnit
        if (catalogEntry.getDialogSettings().getViewSelection() == ViewMenu.SOUNDING) {
            return "";
        }

        String planesKey = catalogEntry.getSelectedData().getPlanesKey();

        Collection<Level> levels = Collections.emptyList();
        if (planesKey.startsWith("spatial-")) {
            levels = LevelUtilities.getOrderedSetOfStandardLevels(planesKey
                    .replace("spatial-", ""));
        } else {
            LevelMappingFactory lmf = LevelMappingFactory
                    .getInstance(LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE);
            LevelMapping lm = lmf.getLevelMappingForKey(planesKey);
            if (lm != null) {
                levels = lm.getLevels();
            }
        }
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        if (levels.size() > 0) {
            match.setLevel(GridLevelTranslator.constructMatching(levels
                    .iterator().next()));
        } else {
            match.setLevel(null);
        }

        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(catalogEntry.getSelectedData().getFieldsKey());
        match.setParameterName(paramList);
        match.setCreatingEntityNames(Arrays.asList(catalogEntry
                .getSelectedData().getSourcesKey()));
        StyleRule sr = null;
        try {

            IStyleType styleType = StyleManager.StyleType.CONTOUR;

            if (displayType.equals(DisplayType.IMAGE)) {
                styleType = StyleManager.StyleType.IMAGERY;
            }

            if (displayType.equals(DisplayType.BARB)
                    || displayType.equals(DisplayType.DUALARROW)
                    || displayType.equals(DisplayType.ARROW)) {
                styleType = StyleManager.StyleType.ARROW;
            }

            if (catalogEntry.getDialogSettings().getViewSelection() == ViewMenu.TIMESERIES
                    || catalogEntry.getDialogSettings().getViewSelection() == ViewMenu.VARVSHGT) {
                styleType = GraphPrefsFactory.GRAPH_STYLE_TYPE;
            }

            sr = StyleManager.getInstance().getStyleRule(styleType, match);
        } catch (StyleException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to obtain a style rule for"
                            + catalogEntry.getSelectedData().getUniqueKey(), e);
        }

        if (sr != null) {
            displayUnit = sr.getPreferences().getDisplayUnitLabel();
            if (displayUnit == null) {
                displayUnit = "";
            }
        }

        return displayUnit;
    }

    @Override
    public HashMap<String, RequestConstraint> getProductParameters(
            IDataCatalogEntry catalogEntry) {
        HashMap<String, RequestConstraint> productParameters = new HashMap<String, RequestConstraint>();
        productParameters.put("pluginName", new RequestConstraint(
                getDefaultPlugin()));

        addProductParameters(catalogEntry, productParameters);

        return productParameters;
    }

    /**
     * Add additional product parameters if needed
     * 
     * @param catalogEntry
     *            catalog entry
     * @param productParameters
     *            parameters map to add to
     */
    protected abstract void addProductParameters(
            IDataCatalogEntry catalogEntry,
            HashMap<String, RequestConstraint> productParameters);

    /**
     * Alter product parameters
     * 
     * @param selectedKey
     * @param selectedValue
     * @param productParameters
     */
    public void alterProductParameters(String selectedKey,
            String selectedValue,
            HashMap<String, RequestConstraint> productParameters) {
        return;
    }

}
