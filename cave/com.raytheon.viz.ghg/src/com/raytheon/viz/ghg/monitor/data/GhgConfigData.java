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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import jep.JepException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ghg.monitor.GhgDisplayManager;
import com.raytheon.viz.ghg.monitor.config.GhgConfigXml;
import com.raytheon.viz.ui.statusline.StatusMessage;
import com.raytheon.viz.ui.statusline.StatusStore;

/**
 * This is a singleton class containing the GHG configuration data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation 
 * 13Jun2008    1157       MW Fegan    Renamed enums using standard camel case.
 * 17Jun2008    1157       MW Fegan    Converted List&lt;String&gt; to String[] in 
 *                                     alert and filter interactions.
 * 18Jun2008    1157       MW Fegan    Added support for saving and deleting
 *                                     named filters.
 * 18Jun2008    1157       MW Fegan    Use clone of default filter.
 * 20Jun2008    1157       MW Fegan    Add resetting to default alerts.
 * 28Nov2012    1353       rferrel     Sort the list of filter names for dialog display.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public final class GhgConfigData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GhgConfigData.class);

    public static final String CUSTOM_FILTER_NAME = "<Dialog>";

    private static final String DEFAULT_FILTER_NAME = "<Default>";

    private static final String CONFIG_PATH = "ghg/config/GHGMonitorConfig.xml";

    private static final String DEFAULT_PATH = "ghg/config/DefaultGHGMonitorConfig.xml";

    /**
     * The VTEC Action Names
     */
    public static final String[] vtecActionNames = { "CAN", "CON", "COR",
            "EXA", "EXB", "EXP", "EXT", "UPG", "NEW", "ROU" };

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
    private Map<String, List<String>> vtecTable;

    /**
     * The name of the VTECTable.py python script.
     */
    private String vtecTableScriptName;

    /**
     * The actual python interpreter for the script specified by scriptName.
     */
    private PythonScript script;

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

    private List<DataEnum> visibleColumns;

    private List<DataEnum> defaultColumns;

    private DataEnum sortColumn;

    private boolean descending;

    /**
     * Alerts enumeration. Contains the available alerts. {@code display}
     * attribute contains the text to display in the Alert Dialog.
     * 
     * @author lvenable
     */
    public enum AlertsEnum {
        NoAlerts(""), AlertLvl1("Alert1"), AlertLvl2("Alert2"), ExpiredAlert(
                "Expired");
        public String display = "";

        private AlertsEnum(String display) {
            this.display = display;
        }
    }

    /**
     * Enumeration of UI selection states. Records can be unselected, selected
     * through the map, or selected through the table.
     * 
     * @author wldougher
     * 
     */
    public enum SelectionEnum {
        NoSelection, MapSelection, MonitorSelection;
    }

    /**
     * Selections enumeration. Contains the available selections for alerts and
     * filtering. Use {@code VALUE.name} to get display text.
     * 
     * @author mfegan
     */
    public enum AlertsFilterEnum {
        Action("Action", "act"), PhenSig("Phen.Sig", "phensig"), Pil("Pil",
                "pil"), WFO("WFO", "officeid"), GeoId("GeoID", "countyheader"), ETN(
                "ETN", "etn"), Seg("Seg", "seg");
        public String name;

        public String column;

        private AlertsFilterEnum(String name, String column) {
            this.name = name;
            this.column = column;
        }
    }

    /**
     * Enumeration of data sources.
     * 
     * @author lvenable
     */
    public enum DataEnum {
        ACTION("Action", 50), VTEC_STRING("VTEC String", 150), ETN("ETN", 38), PHEN_SIG(
                "Phen.Sig", 64), HAZARD("Hazard", 50), PHEN("Phen", 30), SIG(
                "Sig", 20), START("Start", 134), END("End", 134), PURGE(
                "Purge", 134), ISSUE_TIME("Issue Time", 134), PIL("Pil", 41), SEG(
                "Seg", 30), WFO("WFO", 41), GEO_ID("GeoID", 150), PRODUCT_CLASS(
                "ProductClass", 30);

        /**
         * The column label displayed with this data source. This is used in
         * save/load configuration to provide an index-independent way of saving
         * column visibility and order.
         */
        public final String columnLabel;

        /**
         * The minimum column width when this column is visible.
         */
        public final int minColWidth;

        private DataEnum(String columnLabel, int minColWidth) {
            this.columnLabel = columnLabel;
            this.minColWidth = minColWidth;
        }
    }

    /**
     * Enumeration of font sizes.
     */
    public enum GhgFontSizeEnum {
        X_SMALL_FONT, SMALL_FONT, MEDIUM_FONT, LARGE_FONT, X_LARGE_FONT;

        private Map<Display, Font> fontMap;

        private GhgFontSizeEnum() {
            fontMap = new HashMap<Display, Font>();
        }

        /**
         * Get the font data for this enum and the given display.
         * 
         * @param display
         *            The display for which font data should be prepared.
         * @return The font data appropriate to this enum's size and display.
         */
        public FontData getFontData(Display display) {
            Font font = getFont(display);
            return font.getFontData()[0];
        }

        /**
         * Get the Font this enum represents. The font returned will be
         * automatically disposed when the display it is associated with is
         * disposed; application code should not dispose of it.
         * 
         * @param display
         *            The display for which the Font is created.
         * @return the Font associated with the display and this font size enum.
         */
        public Font getFont(Display display) {
            Font font = fontMap.get(display);
            if (font == null) {
                int fontHeight = 10;
                switch (this) {
                case X_SMALL_FONT:
                    fontHeight = 8;
                    break;
                case SMALL_FONT:
                    fontHeight = 10;
                    break;
                case MEDIUM_FONT:
                    fontHeight = 12;
                    break;
                case LARGE_FONT:
                    fontHeight = 14;
                    break;
                case X_LARGE_FONT:
                    fontHeight = 16;
                    break;
                }
                font = new Font(display, "Monospace", fontHeight, SWT.NORMAL);

                // Fonts are system resources. We have to dispose of them
                // properly.
                display.addListener(SWT.Dispose, new Listener() {
                    public void handleEvent(Event e) {
                        Font font = fontMap.get(e.display);
                        font.dispose();
                        fontMap.remove(e.display);
                        // Technically, we should remove this listener from the
                        // display, but it shouldn't matter if the display is
                        // being disposed. The garbage collector will eventually
                        // reclaim it.
                    }
                });
                fontMap.put(display, font);
            }
            return font;
        }
    }

    public static enum FeatureEnum {
        ALERTS, COLUMNS, COLORS, FILTERS
    }

    /**
     * Private constructor.
     */
    private GhgConfigData() {
        init();
    }

    /**
     * Get an instance of the GHG configuration data.
     * 
     * @return An instance of the GHG configuration data.
     */
    public static synchronized GhgConfigData getInstance() {
        // If the GHG configuration data has not been created
        // then create a new instance.
        if (classInstance == null) {
            classInstance = new GhgConfigData();
        }

        return classInstance;
    }

    /**
     * Initialize the configuration data.
     */
    private void init() {
        alertLvl1Colors = new GhgColorData(new RGB(0, 0, 255), new RGB(255,
                255, 0));
        alertLvl2Colors = new GhgColorData(new RGB(255, 255, 255), new RGB(255,
                0, 0));
        expiredAlertColors = new GhgColorData(new RGB(255, 255, 255), new RGB(
                171, 0, 201));
        mapSelectionsColors = new GhgColorData(new RGB(255, 255, 255), new RGB(
                0, 218, 240));
        regularEntriesColors = new GhgColorData(new RGB(0, 0, 0), new RGB(180,
                180, 180));
        monitorSelectionsColors = new GhgColorData(new RGB(255, 255, 255),
                new RGB(0, 0, 255));
        testProductsColors = new GhgColorData(new RGB(255, 255, 255), new RGB(
                128, 128, 128));

        /* create the default alerts data */
        defaultAlerts = new GhgAlertsConfigData();
        defaultAlerts.setLocal(false);
        defaultAlerts.setTest(false);
        defaultAlerts.addAlert(new GhgAlertData(true, true, 10,
                AlertsEnum.AlertLvl1));
        defaultAlerts.addAlert(new GhgAlertData(true, true, 5,
                AlertsEnum.AlertLvl2));
        defaultAlerts.addAlert(new GhgAlertData(true, true, 0,
                AlertsEnum.ExpiredAlert));
        defaultAlerts.setActions(new String[] { "NEW", "CON", "COR", "EXT",
                "EXA", "EXB" });
        defaultAlerts.setPhenSigs(new String[] { "SV.W", "TO.W" });
        defaultAlerts.setPils(new String[] { "SVR", "SVS", "TOR" });

        final String siteId = SiteMap.getInstance().getSite4LetterId(
                DataManager.getCurrentInstance().getSiteID());

        /* generate some hardcoded default filter data */
        GhgDataFilter filter = new GhgDataFilter() {
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
        defaultFilter = filter;

        /* add a couple of named filters */
        filters = new HashMap<String, GhgDataFilter>();

        visibleColumns = new ArrayList<DataEnum>(DataEnum.values().length);
        // The initial columns visible. These need to match the ones set up by
        // GhgMonitorDlg.
        visibleColumns.addAll(Arrays.asList(DataEnum.ACTION, DataEnum.ETN,
                DataEnum.PHEN_SIG, DataEnum.START, DataEnum.END,
                DataEnum.PURGE, DataEnum.ISSUE_TIME, DataEnum.PIL,
                DataEnum.WFO, DataEnum.GEO_ID));
        sortColumn = DataEnum.PURGE;

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
    public void setMonitorSelectionsColors(GhgColorData monitorSelectionsColors) {
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
        GhgDisplayManager.getInstance().setFilterChanged(true);
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
                throw new VizException("Requested filter " + name
                        + " not found.");
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
        if (Util.isEmptyString(name) || (filter == null)) {
            throw new VizException("Invalid filter save request - name = "
                    + Util.printString(name));
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
        if (Util.isEmptyString(name) || !filters.containsKey(name)) {
            throw new VizException("Invalid filter delete request, name "
                    + Util.printString(name) + " does not exist");
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
            visibleColumns = defaultColumns != null ? new ArrayList<DataEnum>(
                    defaultColumns) : new ArrayList<DataEnum>();
            break;
        case COLORS:
            // TODO: Support default colors
            // this.alertLvl1Colors = null;
            // this.alertLvl2Colors = null;
            // this.expiredAlertColors = null;
            // this.regularEntriesColors = null;
            // this.mapSelectionsColors = null;
            // this.monitorSelectionsColors = null;
            // this.testProductsColors = null;
            // break;
        default:
            throw new UnsupportedOperationException("Unknown Feature "
                    + feature);
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
        LocalizationFile localizationFile = pathMgr.getLocalizationFile(locCtx,
                CONFIG_PATH);
        File configFile = localizationFile.getFile();

        try {
            JAXBContext ctx = JAXBContext.newInstance(GhgConfigXml.class);
            Marshaller marshaller = ctx.createMarshaller();
            marshaller.marshal(config, configFile);
            localizationFile.save();
            StatusStore.updateStatus(StatusConstants.SUBCATEGORY_GHG,
                    "Saved configuration", StatusMessage.Importance.REGULAR);

        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error saving GHG Monitor configuration", e);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error saving GHG Monitor configuration", e);
        }
    }

    public void load(boolean reportMissing) {
        loadFrom(CONFIG_PATH, reportMissing);
    }

    public void loadDefault() {
        loadFrom(DEFAULT_PATH, true);
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

        alertLvl1Colors = config.getAlertLvl1Colors();
        alertLvl2Colors = config.getAlertLvl2Colors();
        expiredAlertColors = config.getExpiredAlertColors();
        mapSelectionsColors = config.getMapSelectionsColors();
        monitorSelectionsColors = config.getMonitorSelectionsColors();
        regularEntriesColors = config.getRegularEntriesColors();
        testProductsColors = config.getTestProductsColors();

        visibleColumns = config.getVisibleColumns();
        sortColumn = config.getSortColumn();
        descending = config.isDescending();
    }

    /**
     * @return the visibleColumns
     */
    public List<DataEnum> getVisibleColumns() {
        return visibleColumns;
    }

    /**
     * @param visibleColumns
     *            the visibleColumns to set
     */
    public void setVisibleColumns(List<DataEnum> visibleColumns) {
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
        defaultColumns = new ArrayList<DataEnum>(visibleColumns);
    }

    public String[] getPhenSigCodes() {
        Set<String> phenSigSet = vtecTable.keySet();
        List<String> phenSigList = new ArrayList<String>(phenSigSet);
        Collections.sort(phenSigList);

        return phenSigList.toArray(new String[phenSigList.size()]);
    }

    /**
     * Load the python VTECTable dictionary.
     */
    private void initializePython() {
        PythonScript python = null;
        List<String> preEvals = new ArrayList<String>();

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String filePath = pathMgr.getFile(commonCx,
                "vtec" + File.separator + "VTECTable.py").getPath();
        String pythonPath = pathMgr.getFile(commonCx, "python").getPath();
        String vtecPath = pathMgr.getFile(commonCx, "vtec").getPath();

        try {
            // Add some getters we need "in the script" that we want hidden
            preEvals.add("from JUtil import pyDictToJavaMap");
            preEvals.add("def getVTECTable() :\n     return pyDictToJavaMap(VTECTable)");

            python = new PythonScript(filePath, PyUtil.buildJepIncludePath(
                    pythonPath, vtecPath), this.getClass().getClassLoader(),
                    preEvals);

            // Open the Python interpreter using the designated script.
            vtecTable = (Map<String, List<String>>) python.execute(
                    "getVTECTable", null);

        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error initializing VTEC table python", e);
        } finally {
            if (python != null) {
                python.dispose();
            }
        }
    }
}
