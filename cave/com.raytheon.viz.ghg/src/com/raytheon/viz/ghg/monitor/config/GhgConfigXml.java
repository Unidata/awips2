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
package com.raytheon.viz.ghg.monitor.config;

import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.viz.ghg.monitor.data.GhgAlertsConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgColorData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgDataFilter;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.GhgFontSizeEnum;


@XmlRootElement(name="ghgConfig")
@XmlAccessorType(XmlAccessType.NONE)
public final class GhgConfigXml {

    /**
     * Alert Level 1 colors.
     */
    @XmlElement
    private GhgColorData alertLvl1Colors;

    /**
     * Alert Level 2 colors.
     */
    @XmlElement
    private GhgColorData alertLvl2Colors;

    /**
     * Expired Alert colors.
     */
    @XmlElement
    private GhgColorData expiredAlertColors;

    /**
     * Map Selection colors.
     */
    @XmlElement
    private GhgColorData mapSelectionsColors;

    /**
     * Regular Entries colors.
     */
    @XmlElement
    private GhgColorData regularEntriesColors;

    /**
     * Monitor Selections colors.
     */
    @XmlElement
    private GhgColorData monitorSelectionsColors;

    /**
     * Test products colors.
     */
    @XmlElement
    private GhgColorData testProductsColors;

    /**
     * the configured alerts data.
     */
    @XmlElement
    private GhgAlertsConfigData currentAlerts;

    /**
     * The defaults alerts.
     */
    @XmlElement
    private GhgAlertsConfigData defaultAlerts;

    /**
     * contains the currently configured font indicator
     */
    @XmlElement
    private GhgFontSizeEnum currentFont;

    @XmlElement
    private GhgDataFilter currentFilter;

    @XmlElement
    private GhgDataFilter defaultFilter;

    @XmlElement
    @XmlJavaTypeAdapter(GhgDataFilterAdapter.class)
    private Map<String, GhgDataFilter> filters;

    @XmlElement
    private List<GhgConfigData.DataEnum> visibleColumns;

    @XmlElement
    private GhgConfigData.DataEnum sortColumn;

    @XmlElement
    private boolean descending;
    
    @XmlElement
    private boolean identifyTestEvents;

    /**
     * Default constructor.
     */
    public GhgConfigXml() {
    }

    /**
     * @return the alertLvl1Colors
     */
    public GhgColorData getAlertLvl1Colors() {
        return alertLvl1Colors;
    }

    /**
     * @param alertLvl1Colors
     *            the alertLvl1Colors to set
     */
    public void setAlertLvl1Colors(GhgColorData alertLvl1Colors) {
        this.alertLvl1Colors = alertLvl1Colors;
    }

    /**
     * @return the alertLvl2Colors
     */
    public GhgColorData getAlertLvl2Colors() {
        return alertLvl2Colors;
    }

    /**
     * @param alertLvl2Colors
     *            the alertLvl2Colors to set
     */
    public void setAlertLvl2Colors(GhgColorData alertLvl2Colors) {
        this.alertLvl2Colors = alertLvl2Colors;
    }

    /**
     * @return the expiredAlertColors
     */
    public GhgColorData getExpiredAlertColors() {
        return expiredAlertColors;
    }

    /**
     * @param expiredAlertColors
     *            the expiredAlertColors to set
     */
    public void setExpiredAlertColors(GhgColorData expiredAlertColors) {
        this.expiredAlertColors = expiredAlertColors;
    }

    /**
     * @return the mapSelectionsColors
     */
    public GhgColorData getMapSelectionsColors() {
        return mapSelectionsColors;
    }

    /**
     * @param mapSelectionsColors
     *            the mapSelectionsColors to set
     */
    public void setMapSelectionsColors(GhgColorData mapSelectionsColors) {
        this.mapSelectionsColors = mapSelectionsColors;
    }

    /**
     * @return the regularEntriesColors
     */
    public GhgColorData getRegularEntriesColors() {
        return regularEntriesColors;
    }

    /**
     * @param regularEntriesColors
     *            the regularEntriesColors to set
     */
    public void setRegularEntriesColors(GhgColorData regularEntriesColors) {
        this.regularEntriesColors = regularEntriesColors;
    }

    /**
     * @return the monitorSelectionsColors
     */
    public GhgColorData getMonitorSelectionsColors() {
        return monitorSelectionsColors;
    }

    /**
     * @param monitorSelectionsColors
     *            the monitorSelectionsColors to set
     */
    public void setMonitorSelectionsColors(GhgColorData monitorSelectionsColors) {
        this.monitorSelectionsColors = monitorSelectionsColors;
    }

    /**
     * @return the testProductsColors
     */
    public GhgColorData getTestProductsColors() {
        return testProductsColors;
    }

    /**
     * @param testProductsColors
     *            the testProductsColors to set
     */
    public void setTestProductsColors(GhgColorData testProductsColors) {
        this.testProductsColors = testProductsColors;
    }

    /**
     * @return the currentAlerts
     */
    public GhgAlertsConfigData getCurrentAlerts() {
        return currentAlerts;
    }

    /**
     * @param currentAlerts
     *            the currentAlerts to set
     */
    public void setCurrentAlerts(GhgAlertsConfigData currentAlerts) {
        this.currentAlerts = currentAlerts;
    }

    /**
     * @return the defaultAlerts
     */
    public GhgAlertsConfigData getDefaultAlerts() {
        return defaultAlerts;
    }

    /**
     * @param defaultAlerts
     *            the defaultAlerts to set
     */
    public void setDefaultAlerts(GhgAlertsConfigData defaultAlerts) {
        this.defaultAlerts = defaultAlerts;
    }

    /**
     * @return the currentFont
     */
    public GhgFontSizeEnum getCurrentFont() {
        return currentFont;
    }

    /**
     * @param currentFont
     *            the currentFont to set
     */
    public void setCurrentFont(GhgFontSizeEnum currentFont) {
        this.currentFont = currentFont;
    }

    /**
     * @return the currentFilter
     */
    public GhgDataFilter getCurrentFilter() {
        return currentFilter;
    }

    /**
     * @param currentFilter
     *            the currentFilter to set
     */
    public void setCurrentFilter(GhgDataFilter currentFilter) {
        this.currentFilter = currentFilter;
    }

    /**
     * @return the defaultFilter
     */
    public GhgDataFilter getDefaultFilter() {
        return defaultFilter;
    }

    /**
     * @param defaultFilter
     *            the defaultFilter to set
     */
    public void setDefaultFilter(GhgDataFilter defaultFilter) {
        this.defaultFilter = defaultFilter;
    }

    /**
     * @return the filters
     */
    public Map<String, GhgDataFilter> getFilters() {
        return filters;
    }

    /**
     * @param filters
     *            the filters to set
     */
    public void setFilters(Map<String, GhgDataFilter> filters) {
        this.filters = filters;
    }

    /**
     * @return the visibleColumns
     */
    public List<GhgConfigData.DataEnum> getVisibleColumns() {
        return visibleColumns;
    }

    /**
     * @param visibleColumns
     *            the visibleColumns to set
     */
    public void setVisibleColumns(List<GhgConfigData.DataEnum> visibleColumns) {
        this.visibleColumns = visibleColumns;
    }

    /**
     * @return the sortColumn
     */
    public GhgConfigData.DataEnum getSortColumn() {
        return sortColumn;
    }

    /**
     * @param sortColumn
     *            the sortColumn to set
     */
    public void setSortColumn(GhgConfigData.DataEnum sortColumn) {
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
}