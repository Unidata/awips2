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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.level.LevelUtilities;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.xy.crosssection.rsc.CrossSectionResourceData;
import com.raytheon.uf.viz.xy.timeheight.rsc.TimeHeightResourceData;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResourceData;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResourceData.AxisParameter;
import com.raytheon.uf.viz.xy.varheight.rsc.VarHeightResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.grid.GridLevelTranslator;
import com.raytheon.viz.grid.rsc.GridLoadProperties;
import com.raytheon.viz.skewt.rscdata.SkewTResourceData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Abstract data catalog implementation for shared functionality between data
 * catalogs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2009  2987       jelkins     Initial creation
 * 10-21-09     #1711      bsteffen    Updated Baseline and Points to use new ToolsDataManager
 * 01/30/2012   DR 14308   D.Friedman  Use correct style for arrow types.
 * 07/31/2012   #875       rferrel     Now uses points.
 * Feb 21, 2013 1617       bsteffen    fixed vb sounding point selection for
 *                                     points which contain the word Point
 * May 03, 2013 DR14824 mgamazaychikov Added alterProductParameters method
 * 
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public abstract class AbstractDataCatalog implements IDataCatalog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractDataCatalog.class);

    /** key representing all latitude and longitude planes **/
    public static final String LAT_LON_KEY = "LatLon";

    /** key representing all point and line planes **/
    public static final String POINT_LINE_KEY = "PointLine";

    public static final Pattern POINT_PATTERN = Pattern.compile("^Point");

    /**
     * 
     * @return a list of plugin Names used for the given setting
     */
    protected abstract String[] getPlugins(ViewMenu setting);

    /**
     * 
     * @return the default plugin to use in any case
     */
    protected String getDefaultPlugin() {
        return getPlugins(null)[0];
    }

    @Override
    public Collection<ResourcePair> getResourcesToLoad(
            IDataCatalogEntry catalogEntry, ResourceType resourceType,
            DisplayType displayType) {

        ResourcePair jobRequest = new ResourcePair();

        AbstractRequestableResourceData resourceData = getResourceData(
                catalogEntry, resourceType);

        resourceData.setMetadataMap(getProductParameters(catalogEntry));

        LoadProperties loadProperties;
        loadProperties = getLoadProperties(catalogEntry, resourceType,
                displayType);
        loadProperties.setResourceType(resourceType);

        jobRequest.setResourceData(resourceData);
        jobRequest.setLoadProperties(loadProperties);
        jobRequest.setProperties(new ResourceProperties());

        return Arrays.asList(jobRequest);
    }

    /**
     * 
     * @param catalogEntry
     * @param resourceType
     * @param inputResourceData
     * @return
     */
    protected AbstractRequestableResourceData getResourceData(
            IDataCatalogEntry catalogEntry, ResourceType resourceType,
            AbstractRequestableResourceData inputResourceData) {

        AbstractRequestableResourceData resourceData;

        switch (resourceType) {

        case CROSS_SECTION:
            CrossSectionResourceData csData = (CrossSectionResourceData) inputResourceData;
            csData.setParameter(catalogEntry.getSelectedData().getFieldsKey());
            csData.setParameterName(catalogEntry.getSelectedData()
                    .getFieldsText());
            csData.setSource(catalogEntry.getSelectedData().getSourcesText());
            resourceData = csData;
            break;
        case VAR_HEIGHT:
            VarHeightResourceData vhData = (VarHeightResourceData) inputResourceData;
            vhData.setPoint(getPointCoordinate(catalogEntry));
            vhData.setParameter(catalogEntry.getSelectedData().getFieldsKey());
            vhData.setParameterName(catalogEntry.getSelectedData()
                    .getFieldsText());
            vhData.setPointLetter(getPointLetter(catalogEntry));
            vhData.setSource(catalogEntry.getSelectedData().getSourcesText());
            resourceData = vhData;
            break;
        case TIME_SERIES:
            TimeSeriesResourceData tsData = (TimeSeriesResourceData) inputResourceData;
            tsData.setCoordinate(getPointCoordinate(catalogEntry));
            tsData.setPointLetter(getPointLetter(catalogEntry));
            tsData.setSource(catalogEntry.getSelectedData().getSourcesText());

            AxisParameter yParameter = new AxisParameter();
            yParameter.code = catalogEntry.getSelectedData().getFieldsKey();
            yParameter.name = catalogEntry.getSelectedData().getFieldsText();

            tsData.setYParameter(yParameter);
            tsData.setLevelKey(catalogEntry.getSelectedData().getPlanesKey());
            resourceData = tsData;
            break;
        case TIME_HEIGHT:
            TimeHeightResourceData thData = (TimeHeightResourceData) inputResourceData;
            thData.setPoint(getPointCoordinate(catalogEntry));
            thData.setParameter(catalogEntry.getSelectedData().getFieldsKey());
            thData.setParameterName(catalogEntry.getSelectedData()
                    .getFieldsText());
            thData.setPointLetter(getPointLetter(catalogEntry));
            thData.setSource(catalogEntry.getSelectedData().getSourcesText());
            resourceData = thData;
            break;
        case SOUNDING:
            resourceData = inputResourceData;
            if ((catalogEntry != null)
                    && (resourceData instanceof SkewTResourceData)) {
                ((SkewTResourceData) resourceData)
                        .setPointLetter(getPointLetter(catalogEntry));
            }
            break;
        default: // PLAN_VIEW
            resourceData = null;
            break;
        }
        return resourceData;
    }

    /**
     * Get the resource data
     * 
     * Override this method to provide an implementation for PLAN_VIEW if
     * needed.
     * 
     * @param catalogEntry
     * @param resourceType
     * @return the resource data for the given catalogEntry
     */
    protected AbstractRequestableResourceData getResourceData(
            IDataCatalogEntry catalogEntry, ResourceType resourceType) {

        AbstractRequestableResourceData resourceData;

        switch (resourceType) {

        case CROSS_SECTION:
            resourceData = getResourceData(catalogEntry, resourceType,
                    new CrossSectionResourceData());
            break;
        case VAR_HEIGHT:
            resourceData = getResourceData(catalogEntry, resourceType,
                    new VarHeightResourceData());
            break;
        case TIME_SERIES:
            resourceData = getResourceData(catalogEntry, resourceType,
                    new TimeSeriesResourceData());
            break;
        case TIME_HEIGHT:
            resourceData = getResourceData(catalogEntry, resourceType,
                    new TimeHeightResourceData());
            break;
        case SOUNDING:
            resourceData = getResourceData(catalogEntry, resourceType,
                    new SkewTResourceData());
            break;
        default: // PLAN_VIEW
            resourceData = null;
            break;
        }
        return resourceData;
    }

    /**
     * Obtain the point from the given catalog entry or from directly from the
     * volume browser if in time series.
     * 
     * @param catalogEntry
     *            the catalogEntry for which to obtain a point
     * @return the coordinates belonging to the point. Null if there is no point
     *         associated with this catalog Entry.
     */
    protected Coordinate getPointCoordinate(IDataCatalogEntry catalogEntry) {

        String pointLetter = getPointLetter(catalogEntry);

        Coordinate c = PointsDataManager.getInstance().getCoordinate(
                pointLetter);
        if (c == null) {
            c = PointsDataManager.getInstance().getCoordinate("A");
        }
        return c;

    }

    protected String getPointLetter(IDataCatalogEntry catalogEntry) {
        String pointLetter = null;
        switch (catalogEntry.getDialogSettings().getViewSelection()) {
        case TIMEHEIGHT:
        case VARVSHGT:
        case CROSSSECTION:
        case SOUNDING:
            pointLetter = POINT_PATTERN.matcher(
                    catalogEntry.getSelectedData().getPlanesKey())
                    .replaceFirst("");
            break;
        case TIMESERIES:
            pointLetter = catalogEntry.getDialogSettings().getPointsSelection()
                    .getName();
            break;
        }
        return pointLetter;
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
            try {
                levels = LevelUtilities.getOrderedSetOfStandardLevels(planesKey
                        .replace("spatial-", ""));
            } catch (VizCommunicationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else {
            try {
                LevelMappingFactory lmf = LevelMappingFactory
                        .getInstance(LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE);
                LevelMapping lm = lmf.getLevelMappingForKey(planesKey);
                if (lm != null) {
                    levels = lm.getLevels();
                }
            } catch (CommunicationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
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

            StyleManager.StyleType styleType = StyleManager.StyleType.CONTOUR;

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
                styleType = StyleManager.StyleType.GRAPH;
            }

            sr = StyleManager.getInstance().getStyleRule(styleType, match);
        } catch (VizStyleException e) {
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
     * 
     * @param dataCatalogEntry
     * @param resourceType
     * @param displayType
     * @return the load properties to use, override this method if needed
     */
    protected LoadProperties getLoadProperties(
            IDataCatalogEntry dataCatalogEntry, ResourceType resourceType,
            DisplayType displayType) {

        // we should really add the displayType attribute to LoadProperties
        return new GridLoadProperties(displayType);
    }

    public static Set<String> getPointLineKeys() {
        Set<String> keySet = new HashSet<String>();
        for (String letter : PointsDataManager.getInstance().getPointNames()) {
            keySet.add("Point" + letter);
        }
        for (String letter : ToolsDataManager.getInstance().getBaselineNames()) {
            keySet.add("Line" + letter);
        }
        return keySet;
    }

    /**
     * @param plane
     * @return true if the given plane is a line or point
     */
    protected boolean isLineOrPoint(String plane) {
        return (isLatLon(plane) || isPointLine(plane));
    }

    protected boolean isLatLon(String plane) {
        return ((plane != null) && (plane.startsWith("Lat")
                || plane.startsWith("Lon") || plane.equals("LATS") || plane
                .equals("LONS")));
    }

    protected boolean isPointLine(String plane) {
        return ((plane != null) && (plane.startsWith("Line") || plane
                .startsWith("Point")));
    }
    
    /**
     * Alter product parameters
     * 
     * @param selectedKey
     * @param selectedValue
     * @param productParameters
     */
    public void alterProductParameters(
            String selectedKey,
            String selectedValue, HashMap<String, RequestConstraint> productParameters) {
        return;
    }

}
