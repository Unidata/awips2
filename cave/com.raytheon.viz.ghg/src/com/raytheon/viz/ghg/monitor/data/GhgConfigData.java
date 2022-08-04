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
package com.raytheon.viz.ghg.monitor.data;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonEval;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.ghg.constants.StatusConstants;
import com.raytheon.viz.ghg.monitor.config.GhgConfigXml;
import com.raytheon.viz.ghg.monitor.event.AbstractGhgMonitorEvent.GhgEventListener;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorFilterChangeEvent;
import com.raytheon.viz.ui.statusline.StatusMessage;
import com.raytheon.viz.ui.statusline.StatusStore;

import jep.JepConfig;
import jep.JepException;

/**
 * This is a singleton class containing the GHG configuration data.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 25, 2008  N/A      lvenable  Initial creation
 * Jun 13, 2008  1157     MW Fegan  Renamed enums using standard camel case.
 * Jun 17, 2008  1157     MW Fegan  Converted List&lt;String&gt; to String[] in
 *                                  alert and filter interactions.
 * Jun 18, 2008  1157     MW Fegan  Added support for saving and deleting named
 *                                  filters.
 * Jun 18, 2008  1157     MW Fegan  Use clone of default filter.
 * Jun 20, 2008  1157     MW Fegan  Add resetting to default alerts.
 * Nov 28, 2012  1353     rferrel   Sort the list of filter names for dialog
 *                                  display.
 * Apr 10, 2014  15769    ryu       Modified default config and GUI items to
 *                                  match A1. Default config changed to hard
 *                                  coding instead of reading from config file.
 * Nov 12, 2015  4834     njensen   Changed LocalizationOpFailedException to
 *                                  LocalizationException
 * Dec 16, 2015  5184     dgilling  Remove viz.gfe dependencies.
 * Feb 05, 2016  5316     randerso  Moved notification of filter change into
 *                                  this class
 * Feb 05, 2016  5242     dgilling  Remove calls to deprecated Localization
 *                                  APIs.
 * Jun 03, 2019  7852     dgilling  Update code for jep 3.8.
 * Nov 14, 2019  7919     randerso  Removed font creation and tracking. Code
 *                                  cleanup.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public final class GhgConfigData {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GhgConfigData.class);

    public static final String CUSTOM_FILTER_NAME = "<Dialog>";

    private static final String DEFAULT_FILTER_NAME = "<Default>";

    private static final String CONFIG_PATH = "ghg/config/GHGMonitorConfig.xml";

    /**
     * The VTEC Action Names
     */
    public static final String[] vtecActionNames = { "CAN", "CON", "EXA", "EXB",
            "EXP", "EXT", "NEW", "UPG" };

    /**
     * The VTEC Afos Product (PIL) Names
     */
    public static final String[] vtecPILNames = { "CFW", "CWF", "EWW", "FFA",
            "FFS", "FFW", "FLS", "FLW", "GLF", "HLS", "MWS", "MWW", "NPW",
            "NSH", "OFF", "RFW", "SMW", "SVR", "SVS", "TCV", "TOR", "WCN",
            "WOU", "WSW" };

    /**
     * The VTECTable dictionary.
     */
    private Map<String, Map<String, String>> vtecTable;

    /**
     * Instance of the GHG configuration data.
     */
    private static GhgConfigData classInstance;

    /**
     * Alert Level 1 colors.
     */
    private GhgColorData alertLvl1Colors;

    /**
     * Alert Level 2 colors.
     */
    private GhgColorData alertLvl2Colors;

    /**
     * Expired Alert colors.
     */
    private GhgColorData expiredAlertColors;

    /**
     * Map Selection colors.
     */
    private GhgColorData mapSelectionsColors;

    /**
     * Regular Entries colors.
     */
    private GhgColorData regularEntriesColors;

    /**
     * Monitor Selections colors.
     */
    private GhgColorData monitorSelectionsColors;

    /**
     * Test products colors.
     */
    private GhgColorData testProductsColors;

    /**
     * the configured alerts data.
     */
    private GhgAlertsConfigData currentAlerts;

    /**
     * The defaults alerts.
     */
    private GhgAlertsConfigData defaultAlerts = new GhgAlertsConfigData();

    /**
     * contains the currently configured font indicator
     */
    private GhgFontSizeEnum currentFont = GhgFontSizeEnum.SMALL_FONT;

    private GhgDataFilter currentFilter = null;

    private GhgDataFilter defaultFilter = null;

    private Map<String, GhgDataFilter> filters = null;

    private Set<DataEnum> visibleColumns;

    private Set<DataEnum> defaultColumns;

    private DataEnum sortColumn;

    private boolean descending;

    private boolean identifyTestEvents;

    /**
     * Filter change listener list
     */
    private List<GhgEventListener> filterChangeListenerList = new ArrayList<>();

    /**
     * Alerts enumeration. Contains the available alerts. {@code display}
     * attribute contains the text to display in the Alert Dialog.
     */
    public enum AlertsEnum {
        NoAlerts(""),
        AlertLvl1("Alert1"),
        AlertLvl2("Alert2"),
        ExpiredAlert("Expired");
        private String display = "";

        private AlertsEnum(String display) {
            this.display = display;
        }

        public String getDisplay() {
            return display;
        }
    }

    /**
     * Enumeration of UI selection states. Records can be unselected, selected
     * through the map, or selected through the table.
     */
    public enum SelectionEnum {
        NoSelection, MapSelection, MonitorSelection;
    }

    /**
     * Selections enumeration. Contains the available selections for alerts and
     * filtering. Use {@code VALUE.name} to get display text.
     */
    public enum AlertsFilterEnum {
        Action("Action", "act"),
        PhenSig("Phen.Sig", "phensig"),
        Pil("Pil", "pil"),
        WFO("WFO", "officeid"),
        GeoId("GeoID", "countyheader"),
        ETN("ETN", "etn"),
        Seg("Seg", "seg");
        private String name;

        private String column;

        private AlertsFilterEnum(String name, String column) {
            this.name = name;
            this.column = column;
        }

        public String getName() {
            return name;
        }

        public String getColumn() {
            return column;
        }
    }

    /**
     * Enumeration of data sources.
     */
    public enum DataEnum {
        ACTION("Action"),
        VTEC_STRING("VTEC String"),
        ETN("ETN"),
        PHEN_SIG("Phen.Sig"),
        HAZARD("Hazard"),
        PHEN("Phen"),
        SIG("Sig"),
        START("Start"),
        END("End"),
        PURGE("Purge"),
        ISSUE_TIME("Issue Time"),
        PIL("Pil"),
        SEG("Seg"),
        WFO("WFO"),
        GEO_ID("GeoID"),
        PRODUCT_CLASS("ProductClass");

        /**
         * The column label displayed with this data source. This is used in
         * save/load configuration to provide an index-independent way of saving
         * column visibility and order.
         */
        public final String columnLabel;

        private DataEnum(String columnLabel) {
            this.columnLabel = columnLabel;
        }
    }

    /**
     * Enumeration of fonts.
     */
    public enum GhgFontSizeEnum {
        X_SMALL_FONT(8),
        SMALL_FONT(10),
        MEDIUM_FONT(12),
        LARGE_FONT(14),
        X_LARGE_FONT(16);

        private int fontHeight;

        private GhgFontSizeEnum(int fontHeight) {
            this.fontHeight = fontHeight;
        }

        public int getFontHeight() {
            return fontHeight;
        }

        public String getFontName() {
            return "Monospace";
        }

    }

    public static enum FeatureEnum {
        ALERTS, COLUMNS, FILTERS
    }

    /**
     * Private constructor.
     *
     * @param displayMgr
     */
    private GhgConfigData() {
        init();
    }

    /**
     * Constructs an instance of the GHG configuration data. Used to build the
     * initial instance and should only ever be called from the top-level GHG
     * Monitor dialog.
     *
     * @return An instance of the GHG configuration data.
     */
    public static synchronized GhgConfigData buildInstance() {
        // If the GHG configuration data has not been created
        // then create a new instance.
        if (classInstance == null) {
            classInstance = new GhgConfigData();
        }

        return classInstance;
    }

    /**
     * Returns the instance of the GHG configuration data. Should be used to
     * retrieve the config data instance by all other parts of the GHG Monitor
     * code.
     *
     * @return An instance of the GHG configuration data.
     */
    public static synchronized GhgConfigData getInstance() {
        return classInstance;
    }

    /**
     * Initialize the configuration data.
     */
    private void init() {
        loadDefault();

        defaultFilter = currentFilter.clone();
        defaultAlerts = currentAlerts.clone();
        defaultColumns = EnumSet.copyOf(visibleColumns);

        // Get the VTECTable
        initializePython();
    }

    /**
     * Get the color data associated with the alert passed in.
     *
     * @param alert
     *            Alert value.
     * @return The color data.
     */
    public GhgColorData getGhgColorData(AlertsEnum alert) {
        switch (alert) {
        case AlertLvl1:
            return getAlertLvl1Colors();
        case AlertLvl2:
            return getAlertLvl2Colors();
        case ExpiredAlert:
            return getExpiredAlertColors();
        default:
            return getRegularEntriesColors();
        }
    }

    public GhgColorData getGhgColorData(SelectionEnum selLvl) {
        switch (selLvl) {
        case MonitorSelection:
            return getMonitorSelectionsColors();
        case MapSelection:
            return getMapSelectionsColors();
        default:
            return getRegularEntriesColors();
        }
    }

    /**
     * Get the Alert Level 1 colors.
     *
     * @return The Alert Level 1 colors.
     */
    public GhgColorData getAlertLvl1Colors() {
        return alertLvl1Colors;
    }

    /**
     * Set the Alert Level 1 colors.
     *
     * @param alertLvl1Colors
     *            The Alert Level 1 colors.
     */
    public void setAlertLvl1Colors(GhgColorData alertLvl1Colors) {
        this.alertLvl1Colors = alertLvl1Colors;
    }

    /**
     * Get the Alert Level 2 colors.
     *
     * @return The Alert Level 2 colors.
     */
    public GhgColorData getAlertLvl2Colors() {
        return alertLvl2Colors;
    }

    /**
     * Set the Alert Level 2 colors.
     *
     * @param alertLvl2Colors
     *            The Alert Level 2 colors.
     */
    public void setAlertLvl2Colors(GhgColorData alertLvl2Colors) {
        this.alertLvl2Colors = alertLvl2Colors;
    }

    /**
     * Get the Expired Alert colors.
     *
     * @return The Expired Alert colors.
     */
    public GhgColorData getExpiredAlertColors() {
        return expiredAlertColors;
    }

    /**
     * Set the Expired Alert colors.
     *
     * @return The Expired Alert colors.
     */
    public void setExpiredAlertColors(GhgColorData expiredAlertColors) {
        this.expiredAlertColors = expiredAlertColors;
    }

    /**
     * Get the Map Selections colors.
     *
     * @return The Map Selections colors.
     */
    public GhgColorData getMapSelectionsColors() {
        return mapSelectionsColors;
    }

    /**
     * Set the Map Selections colors.
     *
     * @param mapSelectionsColors
     *            The Map Selections colors.
     */
    public void setMapSelectionsColors(GhgColorData mapSelectionsColors) {
        this.mapSelectionsColors = mapSelectionsColors;
    }

    /**
     * Get the Regular Entries colors.
     *
     * @return The Regular Entries colors.
     */
    public GhgColorData getRegularEntriesColors() {
        return regularEntriesColors;
    }

    /**
     * Set the Regular Entries colors.
     *
     * @param regularEntriesColors
     *            The Regular Entries colors.
     */
    public void setRegularEntriesColors(GhgColorData regularEntriesColors) {
        this.regularEntriesColors = regularEntriesColors;
    }

    /**
     * Get the Monitor Selections colors.
     *
     * @return The Monitor Selections colors.
     */
    public GhgColorData getMonitorSelectionsColors() {
        return monitorSelectionsColors;
    }

    /**
     * Set the Monitor Selections colors.
     *
     * @param monitorSelectionsColors
     *            The Monitor Selections colors.
     */
    public void setMonitorSelectionsColors(
            GhgColorData monitorSelectionsColors) {
        this.monitorSelectionsColors = monitorSelectionsColors;
    }

    /**
     * Get the Test Products colors.
     *
     * @return The Test Products colors.
     */
    public GhgColorData getTestProductsColors() {
        return testProductsColors;
    }

    /**
     * Set the Test Products colors.
     *
     * @return The Test Products colors.
     */
    public void setTestProductsColors(GhgColorData testProductsColors) {
        this.testProductsColors = testProductsColors;
    }

    /**
     * @return the alerts
     */
    public GhgAlertsConfigData getAlerts() {
        return currentAlerts;
    }

    /**
     * @param alerts
     *            the alerts to set
     */
    public void setAlerts(GhgAlertsConfigData alerts) {
        currentAlerts = alerts;
    }

    /**
     * Gets the currently configured font size.
     */
    public GhgFontSizeEnum getCurrentFont() {
        return currentFont;
    }

    /**
     * Set a new font size.
     */
    public void setCurrentFont(GhgFontSizeEnum font) {
        currentFont = font;
    }

    /**
     * Gets the current filter.
     */
    public GhgDataFilter getCurrentFilter() {
        return currentFilter;
    }

    /**
     * sets a new filter as the current filter. Causes the display be be
     * updated.
     */
    public void setCurrentFilter(GhgDataFilter filter) {
        currentFilter = filter;
        GhgMonitorFilterChangeEvent evt = new GhgMonitorFilterChangeEvent();
        fireFilterChangeEvent(evt);
    }

    /**
     * Returns the saved filters as a Set of Entries.
     */
    public Set<Entry<String, GhgDataFilter>> getSavedFilters() {
        return filters.entrySet();
    }

    /**
     * Sets a named filter as the current filter.
     *
     * @param name
     *            the name of the desired filter
     * @throws VizException
     *             if the named filter does not exist
     */
    public void setCurrentFilter(String name) throws VizException {
        if (DEFAULT_FILTER_NAME.equals(name)) {
            setDefaultAsCurrent(FeatureEnum.FILTERS);
        } else if (!CUSTOM_FILTER_NAME.equals(name)) {
            if (!filters.containsKey(name)) {
                throw new VizException(
                        "Requested filter " + name + " not found.");
            }
            currentFilter = new GhgDataFilter(filters.get(name));
        }
        // do nothing if name is "<Dialog>"
    }

    /**
     * Gets a previously saved named filter.
     *
     * @param name
     *            the name of the filter
     *
     * @return the filter
     *
     * @throws VizException
     *             if the named filter does not exist
     */
    public GhgDataFilter getNamedFilter(String name) throws VizException {
        if (!filters.containsKey(name)) {
            throw new VizException("Requested filter " + name + "not found.");
        }
        return filters.get(name);
    }

    /**
     * adds a named filter to the configuration.
     *
     * @param name
     *            the name of the filter
     * @param filter
     *            the filter
     *
     * @throws VizException
     *             if the name and/or filter is invalid
     */
    public void addNamedFilter(String name, GhgDataFilter filter)
            throws VizException {
        if (StringUtil.isEmptyString(name) || (filter == null)) {
            throw new VizException("Invalid filter save request - name = "
                    + StringUtil.printString(name));
        }
        filters.put(name, filter);
    }

    /**
     * Removes the named filter from the configuration.
     *
     * @param name
     *            the name of the filter to remove
     *
     * @throws VizException
     *             if the named filter does not exist
     */
    public void deleteNamedFilter(String name) throws VizException {
        if (StringUtil.isEmptyString(name) || !filters.containsKey(name)) {
            throw new VizException("Invalid filter delete request, name "
                    + StringUtil.printString(name) + " does not exist");
        }
        if (filters.get(name).name.equals(currentFilter.name)) {
            setDefaultAsCurrent(FeatureEnum.FILTERS);
        }
        filters.remove(name);
    }

    /**
     * Returns a sorted list of the filter names.
     */
    public String[] getFilterNames() {
        String[] names = filters.keySet().toArray(new String[] {});
        Arrays.sort(names);
        return names;
    }

    /**
     * Copies the default feature into the current feature.
     *
     * @param feature
     *            The feature type to copy from the feature's default
     */
    public void setDefaultAsCurrent(FeatureEnum feature) {
        switch (feature) {
        case FILTERS:
            currentFilter = (defaultFilter != null) ? defaultFilter.clone()
                    : new GhgDataFilter();
            break;
        case ALERTS:
            currentAlerts = (defaultAlerts != null) ? defaultAlerts.clone()
                    : new GhgAlertsConfigData();
            break;
        case COLUMNS:
            visibleColumns = defaultColumns != null
                    ? EnumSet.copyOf(defaultColumns)
                    : EnumSet.noneOf(DataEnum.class);
            break;

        default:
            throw new UnsupportedOperationException(
                    "Unknown Feature " + feature);
        }
    }

    public void save() {
        GhgConfigXml config = new GhgConfigXml();
        config.setAlertLvl1Colors(alertLvl1Colors);
        config.setAlertLvl2Colors(alertLvl2Colors);
        config.setMapSelectionsColors(mapSelectionsColors);
        config.setMonitorSelectionsColors(monitorSelectionsColors);
        config.setRegularEntriesColors(regularEntriesColors);
        config.setTestProductsColors(testProductsColors);

        config.setCurrentAlerts(currentAlerts);
        config.setCurrentFilter(currentFilter);
        config.setCurrentFilter(currentFilter);
        config.setCurrentFont(currentFont);
        config.setExpiredAlertColors(expiredAlertColors);
        config.setFilters(filters);

        config.setVisibleColumns(visibleColumns);
        config.setSortColumn(sortColumn);
        config.setDescending(descending);

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext locCtx = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        ILocalizationFile localizationFile = pathMgr.getLocalizationFile(locCtx,
                CONFIG_PATH);

        try (SaveableOutputStream outStream = localizationFile
                .openOutputStream()) {
            SingleTypeJAXBManager<GhgConfigXml> jaxb = SingleTypeJAXBManager
                    .createWithoutException(GhgConfigXml.class);
            jaxb.marshalToStream(config, outStream);
            outStream.save();
            StatusStore.updateStatus(StatusConstants.CATEGORY_GHG,
                    "Saved configuration", StatusMessage.Importance.REGULAR);
        } catch (IOException | LocalizationException
                | SerializationException e) {
            statusHandler.error("Error saving GHG Monitor configuration", e);
        }
    }

    public void loadDefault() {
        alertLvl1Colors = new GhgColorData(new RGB(0, 0, 255),
                new RGB(255, 255, 0));
        alertLvl2Colors = new GhgColorData(new RGB(255, 255, 255),
                new RGB(255, 0, 0));
        expiredAlertColors = new GhgColorData(new RGB(255, 255, 255),
                new RGB(171, 0, 201));
        mapSelectionsColors = new GhgColorData(new RGB(255, 255, 255),
                new RGB(0, 218, 240));
        regularEntriesColors = new GhgColorData(new RGB(0, 0, 0),
                new RGB(180, 180, 180));
        monitorSelectionsColors = new GhgColorData(new RGB(255, 255, 255),
                new RGB(0, 0, 255));
        testProductsColors = new GhgColorData(new RGB(255, 255, 255),
                new RGB(128, 128, 128));

        /* create the default alerts data */
        GhgAlertsConfigData alerts = new GhgAlertsConfigData();
        alerts.setLocal(true);
        alerts.setTest(true);
        alerts.addAlert(new GhgAlertData(true, true, 30, AlertsEnum.AlertLvl1));
        alerts.addAlert(new GhgAlertData(true, true, 10, AlertsEnum.AlertLvl2));
        alerts.addAlert(
                new GhgAlertData(true, true, 0, AlertsEnum.ExpiredAlert));
        alerts.setActions(
                new String[] { "NEW", "CON", "COR", "EXT", "EXA", "EXB" });
        alerts.setPhenSigs(new String[] {});
        alerts.setPils(new String[] {});
        currentAlerts = alerts;

        final String siteId = SiteMap.getInstance()
                .getSite4LetterId(LocalizationManager.getInstance().getSite());

        /* generate some hardcoded default filter data */
        currentFilter = new GhgDataFilter() {
            {
                currentHazards = false;
                name = DEFAULT_FILTER_NAME;
                actions = new String[] { "CON", "EXA", "EXB", "EXT", "NEW" };
                phenSigs = new String[] {};
                pils = new String[] {};
                wfos = new String[] { siteId };
                geoids = new String[] {};
                etns = new String[] {};
                segs = new String[] {};

                combineGeoId = true;
                combineSegments = true;
                combinePurgeTimes = true;
                combineActions = true;

                includeAlerts = true;
                includeMapSelections = true;
                includePastEvents = false;
                includeOrgPilEvents = false;
            }
        };

        /* add a couple of named filters */
        filters = new HashMap<>();

        // The initial columns visible. These need to match the ones set up by
        // GhgMonitorDlg.
        visibleColumns = EnumSet.of(DataEnum.ACTION, DataEnum.ETN,
                DataEnum.PHEN_SIG, DataEnum.START, DataEnum.END, DataEnum.PURGE,
                DataEnum.ISSUE_TIME, DataEnum.PIL, DataEnum.WFO);
        sortColumn = DataEnum.PURGE;

        descending = false;
        identifyTestEvents = true;

        // loadFrom(DEFAULT_PATH, true);
    }

    public void load(boolean reportMissing) {
        loadFrom(CONFIG_PATH, reportMissing);
    }

    /**
     * Load the file.
     *
     * @param fileName
     *            The filename of the file to load
     * @param reportMissing
     *            Report messages through AlertViz
     */
    private void loadFrom(String fileName, boolean reportMissing) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        File configFile = pathMgr.getStaticFile(fileName);
        if (configFile == null) {
            if (reportMissing) {
                statusHandler.handle(Priority.PROBLEM,
                        "Configuration file \"..." + fileName + "\" not found");
            }
            return;
        }

        GhgConfigXml config = null;
        try {
            JAXBContext ctx = JAXBContext.newInstance(GhgConfigXml.class);
            Unmarshaller unmarshaller = ctx.createUnmarshaller();
            config = (GhgConfigXml) unmarshaller.unmarshal(configFile);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error loading GHG Monitor configuration", e);
            return;
        }

        currentAlerts = config.getCurrentAlerts();
        if (currentAlerts.getActions() == null) {
            currentAlerts.setActions(new String[0]);
        }
        if (currentAlerts.getPhenSigs() == null) {
            currentAlerts.setPhenSigs(new String[0]);
        }
        if (currentAlerts.getPils() == null) {
            currentAlerts.setPils(new String[0]);
        }
        currentFilter = config.getCurrentFilter();
        currentFont = config.getCurrentFont();
        filters = config.getFilters();
        if (filters == null) {
            filters = new HashMap<>();
        }

        alertLvl1Colors = config.getAlertLvl1Colors();
        alertLvl2Colors = config.getAlertLvl2Colors();
        expiredAlertColors = config.getExpiredAlertColors();
        mapSelectionsColors = config.getMapSelectionsColors();
        monitorSelectionsColors = config.getMonitorSelectionsColors();
        regularEntriesColors = config.getRegularEntriesColors();
        testProductsColors = config.getTestProductsColors();

        visibleColumns = EnumSet.copyOf(config.getVisibleColumns());
        sortColumn = config.getSortColumn();
        descending = config.isDescending();
        identifyTestEvents = config.isIdentifyTestEvents();
    }

    /**
     * @return the visibleColumns
     */
    public Set<DataEnum> getVisibleColumns() {
        return visibleColumns;
    }

    /**
     * @param visibleColumns
     *            the visibleColumns to set
     */
    public void setVisibleColumns(Set<DataEnum> visibleColumns) {
        this.visibleColumns = visibleColumns;
    }

    /**
     * @return the sortColumn
     */
    public DataEnum getSortColumn() {
        return sortColumn;
    }

    /**
     * @param sortColumn
     *            the sortColumn to set
     */
    public void setSortColumn(DataEnum sortColumn) {
        this.sortColumn = sortColumn;
    }

    /**
     * @return the descending
     */
    public boolean isDescending() {
        return descending;
    }

    /**
     * @param descending
     *            the descending to set
     */
    public void setDescending(boolean descending) {
        this.descending = descending;
    }

    /**
     * @return the identifyTestEvents
     */
    public boolean isIdentifyTestEvents() {
        return identifyTestEvents;
    }

    /**
     * @param identifyTestEvents
     *            the identifyTestEvents to set
     */
    public void setIdentifyTestEvents(boolean identifyTestEvents) {
        this.identifyTestEvents = identifyTestEvents;
    }

    /**
     *
     */
    public void makeCurrentFilterDefault() {
        defaultFilter = currentFilter.clone();
    }

    /**
     *
     */
    public void makeCurrentAlertsDefault() {
        defaultAlerts = currentAlerts.clone();
    }

    public void makeVisibleColumnsDefault() {
        defaultColumns = EnumSet.copyOf(visibleColumns);
    }

    public String[] getPhenSigCodes() {
        Set<String> phenSigSet = vtecTable.keySet();
        List<String> phenSigList = new ArrayList<>(phenSigSet);
        Collections.sort(phenSigList);

        return phenSigList.toArray(new String[phenSigList.size()]);
    }

    public Map<String, Map<String, String>> getVtecTable() {
        return vtecTable;
    }

    /**
     * Load the python VTECTable dictionary.
     */
    private void initializePython() {
        // Add some getters we need "in the script" that we want hidden
        List<String> preEvals = new ArrayList<>();
        preEvals.add("from VTECTable import VTECTable");

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String pythonPath = pathMgr.getFile(commonCx, "python").getPath();
        String vtecPath = pathMgr.getFile(commonCx, "vtec").getPath();

        try (PythonEval python = new PythonEval(
                new JepConfig()
                        .setIncludePath(PyUtil.buildJepIncludePath(pythonPath,
                                vtecPath))
                        .setClassLoader(getClass().getClassLoader()),
                preEvals)) {
            // Open the Python interpreter using the designated script.
            @SuppressWarnings("unchecked")
            Map<String, Map<String, String>> vtecTable = (Map<String, Map<String, String>>) python
                    .getValue("VTECTable");
            this.vtecTable = vtecTable;
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error initializing VTEC table python", e);
        }
    }

    /**
     * Add a listener to the list.
     *
     * @param listener
     */
    public void addFilterChangeListener(GhgEventListener listener) {
        filterChangeListenerList.add(listener);
    }

    /**
     * Remove a listener from the list.
     *
     * @param listener
     */
    public void removeFilterChangeListener(GhgEventListener listener) {
        if (filterChangeListenerList.contains(listener)) {
            filterChangeListenerList.remove(listener);
        }
    }

    /**
     * Fire event so listeners are aware of change.
     *
     * @param event
     *            The GhgMonitorFilterChangeEvent
     */
    private void fireFilterChangeEvent(GhgMonitorFilterChangeEvent event) {
        for (GhgEventListener listener : filterChangeListenerList) {
            listener.notifyUpdate(event);
        }
    }

}
