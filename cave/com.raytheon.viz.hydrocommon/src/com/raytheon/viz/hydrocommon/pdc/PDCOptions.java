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
package com.raytheon.viz.hydrocommon.pdc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroConstants.AdHocDataElementType;

/**
 * Base PDC Options class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 24, 2008            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public abstract class PDCOptions {

    /**
     * Ad Hoc Mode = 0; Timestep mode = 1;
     */
    private int queryMode = 0;

    /** Process as observed 1 or processed 0 */
    private int processMode = 0;

    private int processSelected = 0;

    /**
     * RIVER_AD_HOC_TYPE = 0 RAIN_AD_HOC_TYPE = 1 SNOW_AD_HOC_TYPE = 2
     * TEMP_AD_HOC_TYPE = 3 OTHER_AD_HOC_TYPE = 4
     */
    private int elementType = 0;

    private String selectedAdHocElementString = null;

    private String selectedAdHocElementFullString = null;

    private int tsDataElement;

    private AdHocDataElementType ahDataElement = AdHocDataElementType.RIVER_AD_HOC_TYPE;

    private int peSelection = 0;

    private int pcAndpp = 0;

    private int primary = 1;

    /**
     * Time Mode LATEST = 0, SETTIME = 1, MINSELECT = 2, MAXSELECT = 3,
     * VALUE_CHANGE = 4
     */
    private int timeMode = 0;

    private String pcTimeStr = null;

    /**
     * PRECIP_TIME_30_MINUTES = 0, PRECIP_TIME_1_HOUR = 1, PRECIP_TIME_2_HOURS =
     * 2, PRECIP_TIME_3_HOURS = 3, PRECIP_TIME_4_HOURS = 4, PRECIP_TIME_6_HOURS
     * = 5, PRECIP_TIME_12_HOURS = 6, PRECIP_TIME_18_HOURS = 7,
     * PRECIP_TIME_24_HOURS = 8, PRECIP_TIME_COUNT = 9
     */
    private int instPrecipAccumTimeSelection;

    private Date validTime = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();

    private int durHours = HydroConstants.MISSING_VALUE;

    private int filterByTypeSource = 0;

    private int typeSourceChosenCount = 0;

    private List<String> typeSourceChosenList = new ArrayList<String>();

    private int filterByHSA = 0;

    private List<String> hsaList = new ArrayList<String>();

    private int filterByDataSource = 0;

    private int dataSourceChosenCount = HydroConstants.MISSING_VALUE;

    private String[] dataSourcesChosen = null;

    /**
     * ALL_STATIONS_RSF 0, STREAM_STATIONS_RSF 1, RESERVOIR_STATIONS_RSF 2
     */
    private int riverStationFilter;

    /**
     * PC_AND_PP_PPF(0), PC_ONLY_PPF(1), PP_ONLY_PPF(2);
     */
    private int precipPeFilter;

    private int supressMissing = 0;

    private int fcstptsOnly = 0;

    private int value = 1;

    private int id = 1;

    private int name = 0;

    private int time = 0;

    private int icon = 1;

    private int elevation = 1;

    private int paramCode = 1;

    private int riverStatus = 1;

    private int deriveStageFlow = 1;

    private int floodLevel = 0;

    /**
     * 0 = value 1 = departure
     */
    private int valueType = 0;

    /**
     * 0 = obs 1 = fcst 2 = max of obs and fcst.
     */
    private int stageBasis = 2;

/**
     * SHOW_ALL ("Any Value") 0,
     * SHOW_EQUAL ("Value =") 1,
     * SHOW_NOT_EQUAL ("Value Not =") 2,
     * SHOW_GREATER_EQUAL ("Value >=") 3,
     * SHOW_LESS_EQUAL ("Value <=") 4,
     * SHOW_GREATER ("Value >") 5,
     * SHOW_LESS ("Value <") 6;
     */
    private int valueFilterOperation;

    private double valueFilterValue = HydroConstants.MISSING_VALUE;

/**
     * SHOW_ALL ("Any Elev") 0,
     * SHOW_EQUAL ("Elev =") 1,
     * SHOW_NOT_EQUAL ("Elev Not =") 2,
     * SHOW_GREATER_EQUAL ("Elev >=") 3,
     * SHOW_LESS_EQUAL ("Elev <=") 4,
     * SHOW_GREATER ("Elev >") 5,
     * SHOW_LESS ("Elev <") 6;
     */
    private int elevFilterOperation;

    private double elevFilterValue = HydroConstants.MISSING_VALUE;

    public void reset() {
        setElementType(0);
        setSelectedAdHocElementString("");
        setPrimary(1);
        setPcAndpp(0);
        setPeSelection(0);

        setFilterByTypeSource(0);
        setTypeSourceChosenCount(0);

        /*
         * Set the shef processing mode based on the shef_procobs token. This
         * determines whether to comingle the processed data with the
         * observations or to treat them separately.
         */
        AppsDefaults ad = AppsDefaults.getInstance();
        String tokenValue = ad.getToken("shef_procobs");
        if ((tokenValue != null) && tokenValue.equalsIgnoreCase("on")) {
            setProcessMode(1);
        } else {
            setProcessMode(0);
        }

        setProcessSelected(0);

        setTimeMode(0);
        setDurHours(24);
        setValidTime(Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime());

        /* No support provided for predefining the filter by source options. */
        setFilterByDataSource(0);
        setDataSourceChosenCount(0);

        /* Make sure that by default filtering by service backup is not enabled. */
        setFilterByHSA(0);

        /* Data filter settings */
        setSupressMissing(0);
        setFcstptsOnly(0);

        /* Map Display Options */
        setValue(1);
        setId(1);
        setName(0);
        setTime(0);
        setIcon(1);

        setFloodLevel(0);
        setValueType(0);

        setQueryMode(0);

        setStageBasis(2);

        tokenValue = ad.getToken("hv_pointdata_display");
        if (tokenValue.equalsIgnoreCase("on")) {
            setRiverStatus(1);
        } else {
            setRiverStatus(0);
        }
    }

    /**
     * Ad Hoc Mode = 0; Timestep mode = 1;
     * 
     * @return the queryMode
     */
    public int getQueryMode() {
        return queryMode;
    }

    /**
     * Ad Hoc Mode = 0; Timestep mode = 1;
     * 
     * @param queryMode
     *            the queryMode to set
     */
    public void setQueryMode(int queryMode) {
        this.queryMode = queryMode;
    }

    /**
     * @return the processMode
     */
    public int getProcessMode() {
        return processMode;
    }

    /**
     * @param processMode
     *            the processMode to set
     */
    public void setProcessMode(int processMode) {
        this.processMode = processMode;
    }

    /**
     * @return the processSelected
     */
    public int getProcessSelected() {
        return processSelected;
    }

    /**
     * @param processSelected
     *            the processSelected to set
     */
    public void setProcessSelected(int processSelected) {
        this.processSelected = processSelected;
    }

    /**
     * RIVER_AD_HOC_TYPE = 0 RAIN_AD_HOC_TYPE = 1 SNOW_AD_HOC_TYPE = 2
     * TEMP_AD_HOC_TYPE = 3 OTHER_AD_HOC_TYPE = 4
     * 
     * @return the elementType
     */
    public int getElementType() {
        return elementType;
    }

    /**
     * RIVER_AD_HOC_TYPE = 0 RAIN_AD_HOC_TYPE = 1 SNOW_AD_HOC_TYPE = 2
     * TEMP_AD_HOC_TYPE = 3 OTHER_AD_HOC_TYPE = 4
     * 
     * @param elementType
     *            the elementType to set
     */
    public void setElementType(int elementType) {
        this.elementType = elementType;
    }

    /**
     * @return the selectedAdHocElementString
     */
    public String getSelectedAdHocElementString() {
        return selectedAdHocElementString;
    }

    /**
     * @param selectedAdHocElementString
     *            the selectedAdHocElementString to set
     */
    public void setSelectedAdHocElementString(String selectedAdHocElementString) {
        this.selectedAdHocElementString = selectedAdHocElementString;
    }

    /**
     * @return the selectedAdHocElementFullString
     */
    public String getSelectedAdHocElementFullString() {
        return selectedAdHocElementFullString;
    }

    /**
     * @param selectedAdHocElementFullString
     *            the selectedAdHocElementFullString to set
     */
    public void setSelectedAdHocElementFullString(
            String selectedAdHocElementFullString) {
        this.selectedAdHocElementFullString = selectedAdHocElementFullString;
    }

    /**
     * @return the tsDataElement
     */
    public int getTsDataElement() {
        return tsDataElement;
    }

    /**
     * @param tsDataElement
     *            the tsDataElement to set
     */
    public void setTsDataElement(int tsDataElement) {
        this.tsDataElement = tsDataElement;
    }

    /**
     * @return the ahDataElement
     */
    public AdHocDataElementType getAhDataElement() {
        return ahDataElement;
    }

    /**
     * @param ahDataElement
     *            the ahDataElement to set
     */
    public void setAhDataElement(AdHocDataElementType ahDataElement) {
        this.ahDataElement = ahDataElement;
    }

    /**
     * @return the pcAndpp
     */
    public int getPcAndpp() {
        return pcAndpp;
    }

    /**
     * @param pcAndpp
     *            the pcAndpp to set
     */
    public void setPcAndpp(int pcAndpp) {
        this.pcAndpp = pcAndpp;
    }

    /**
     * @return the primary
     */
    public int getPrimary() {
        return primary;
    }

    /**
     * @param primary
     *            the primary to set
     */
    public void setPrimary(int primary) {
        this.primary = primary;
    }

    /**
     * LATEST = 0, SETTIME = 1, MINSELECT = 2, MAXSELECT = 3, VALUE_CHANGE = 4
     * 
     * @return the timeMode
     */
    public int getTimeMode() {
        return timeMode;
    }

    /**
     * LATEST = 0, SETTIME = 1, MINSELECT = 2, MAXSELECT = 3, VALUE_CHANGE = 4
     * 
     * @param timeMode
     *            the timeMode to set
     */
    public void setTimeMode(int timeMode) {
        this.timeMode = timeMode;
    }

    /**
     * @return the pcTimeStr
     */
    public String getPcTimeStr() {
        return pcTimeStr;
    }

    /**
     * @param pcTimeStr
     *            the pcTimeStr to set
     */
    public void setPcTimeStr(String pcTimeStr) {
        this.pcTimeStr = pcTimeStr;
    }

    /**
     * PRECIP_TIME_30_MINUTES = 0, PRECIP_TIME_1_HOUR = 1, PRECIP_TIME_2_HOURS =
     * 2, PRECIP_TIME_3_HOURS = 3, PRECIP_TIME_4_HOURS = 4, PRECIP_TIME_6_HOURS
     * = 5, PRECIP_TIME_12_HOURS = 6, PRECIP_TIME_18_HOURS = 7,
     * PRECIP_TIME_24_HOURS = 8, PRECIP_TIME_COUNT = 9
     * 
     * @return the instPrecipAccumTimeSelection
     */
    public int getInstPrecipAccumTimeSelection() {
        return instPrecipAccumTimeSelection;
    }

    /**
     * PRECIP_TIME_30_MINUTES = 0, PRECIP_TIME_1_HOUR = 1, PRECIP_TIME_2_HOURS =
     * 2, PRECIP_TIME_3_HOURS = 3, PRECIP_TIME_4_HOURS = 4, PRECIP_TIME_6_HOURS
     * = 5, PRECIP_TIME_12_HOURS = 6, PRECIP_TIME_18_HOURS = 7,
     * PRECIP_TIME_24_HOURS = 8, PRECIP_TIME_COUNT = 9
     * 
     * @param instPrecipAccumTimeSelection
     *            the instPrecipAccumTimeSelection to set
     */
    public void setInstPrecipAccumTimeSelection(int instPrecipAccumTimeSelection) {
        this.instPrecipAccumTimeSelection = instPrecipAccumTimeSelection;
    }

    /**
     * @return the validTime
     */
    public Date getValidTime() {
        return validTime;
    }

    /**
     * @param validTime
     *            the validTime to set
     */
    public void setValidTime(Date validTime) {
        this.validTime = validTime;
    }

    /**
     * @return the durHours
     */
    public int getDurHours() {
        return durHours;
    }

    /**
     * @param durHours
     *            the durHours to set
     */
    public void setDurHours(int durHours) {
        this.durHours = durHours;
    }

    /**
     * @return the filterByTypeSource
     */
    public int getFilterByTypeSource() {
        return filterByTypeSource;
    }

    /**
     * @param filterByTypeSource
     *            the filterByTypeSource to set
     */
    public void setFilterByTypeSource(int filterByTypeSource) {
        this.filterByTypeSource = filterByTypeSource;
    }

    /**
     * @return the typeSourceChosenCount
     */
    public int getTypeSourceChosenCount() {
        return typeSourceChosenCount;
    }

    /**
     * @param typeSourceChosenCount
     *            the typeSourceChosenCount to set
     */
    public void setTypeSourceChosenCount(int typeSourceChosenCount) {
        this.typeSourceChosenCount = typeSourceChosenCount;
    }

    /**
     * @return the typeSourceChosenList
     */
    public List<String> getTypeSourceChosenList() {
        return typeSourceChosenList;
    }

    /**
     * @param typeSourceChosenList
     *            the typeSourceChosenList to set
     */
    public void setTypeSourceChosenList(List<String> typeSourceChosenList) {
        this.typeSourceChosenList = typeSourceChosenList;
    }

    /**
     * @return the filterByHSA
     */
    public int getFilterByHSA() {
        return filterByHSA;
    }

    /**
     * @param filterByHSA
     *            the filterByHSA to set
     */
    public void setFilterByHSA(int filterByHSA) {
        this.filterByHSA = filterByHSA;
    }

    /**
     * @return the hsaList
     */
    public List<String> getHsaList() {
        return hsaList;
    }

    /**
     * @param hsaList
     *            the hsaList to set
     */
    public void setHsaList(List<String> hsaList) {
        this.hsaList = hsaList;
    }

    /**
     * @return the filterByDataSource
     */
    public int getFilterByDataSource() {
        return filterByDataSource;
    }

    /**
     * @param filterByDataSource
     *            the filterByDataSource to set
     */
    public void setFilterByDataSource(int filterByDataSource) {
        this.filterByDataSource = filterByDataSource;
    }

    /**
     * @return the dataSourceChosenCount
     */
    public int getDataSourceChosenCount() {
        return dataSourceChosenCount;
    }

    /**
     * @param dataSourceChosenCount
     *            the dataSourceChosenCount to set
     */
    public void setDataSourceChosenCount(int dataSourceChosenCount) {
        this.dataSourceChosenCount = dataSourceChosenCount;
    }

    /**
     * @return the dataSourcesChosen
     */
    public String[] getDataSourcesChosen() {
        return dataSourcesChosen;
    }

    /**
     * @param dataSourcesChosen
     *            the dataSourcesChosen to set
     */
    public void setDataSourcesChosen(String[] dataSourcesChosen) {
        this.dataSourcesChosen = dataSourcesChosen;
    }

    /**
     * ALL_STATIONS_RSF 0, STREAM_STATIONS_RSF 1, RESERVOIR_STATIONS_RSF 2
     * 
     * @return the riverStationFilter
     */
    public int getRiverStationFilter() {
        return riverStationFilter;
    }

    /**
     * ALL_STATIONS_RSF 0, STREAM_STATIONS_RSF 1, RESERVOIR_STATIONS_RSF 2
     * 
     * @param riverStationFilter
     *            the riverStationFilter to set
     */
    public void setRiverStationFilter(int riverStationFilter) {
        this.riverStationFilter = riverStationFilter;
    }

    /**
     * PC_AND_PP_PPF(0), PC_ONLY_PPF(1), PP_ONLY_PPF(2);
     * 
     * @return the precipPeFilter
     */
    public int getPrecipPeFilter() {
        return precipPeFilter;
    }

    /**
     * PC_AND_PP_PPF(0), PC_ONLY_PPF(1), PP_ONLY_PPF(2);
     * 
     * @param precipPeFilter
     *            the precipPeFilter to set
     */
    public void setPrecipPeFilter(int precipPeFilter) {
        this.precipPeFilter = precipPeFilter;
    }

    /**
     * @return the supressMissing
     */
    public int getSupressMissing() {
        return supressMissing;
    }

    /**
     * @param supressMissing
     *            the supressMissing to set
     */
    public void setSupressMissing(int supressMissing) {
        this.supressMissing = supressMissing;
    }

    /**
     * @return the fcstptsOnly
     */
    public int getFcstptsOnly() {
        return fcstptsOnly;
    }

    /**
     * @param fcstptsOnly
     *            the fcstptsOnly to set
     */
    public void setFcstptsOnly(int fcstptsOnly) {
        this.fcstptsOnly = fcstptsOnly;
    }

    /**
     * @return the value
     */
    public int getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(int value) {
        this.value = value;
    }

    /**
     * @return the id
     */
    public int getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * @return the name
     */
    public int getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(int name) {
        this.name = name;
    }

    /**
     * @return the time
     */
    public int getTime() {
        return time;
    }

    /**
     * @param time
     *            the time to set
     */
    public void setTime(int time) {
        this.time = time;
    }

    /**
     * @return the icon
     */
    public int getIcon() {
        return icon;
    }

    /**
     * @param icon
     *            the icon to set
     */
    public void setIcon(int icon) {
        this.icon = icon;
    }

    /**
     * @return the elevation
     */
    public int getElevation() {
        return elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(int elevation) {
        this.elevation = elevation;
    }

    /**
     * @return the paramCode
     */
    public int getParamCode() {
        return paramCode;
    }

    /**
     * @param paramCode
     *            the paramCode to set
     */
    public void setParamCode(int paramCode) {
        this.paramCode = paramCode;
    }

    /**
     * @return the riverStatus
     */
    public int getRiverStatus() {
        return riverStatus;
    }

    /**
     * @param riverStatus
     *            the riverStatus to set
     */
    public void setRiverStatus(int riverStatus) {
        this.riverStatus = riverStatus;
    }

    /**
     * @return the deriveStageFlow
     */
    public int getDeriveStageFlow() {
        return deriveStageFlow;
    }

    /**
     * @param deriveStageFlow
     *            the deriveStageFlow to set
     */
    public void setDeriveStageFlow(int deriveStageFlow) {
        this.deriveStageFlow = deriveStageFlow;
    }

    /**
     * @return the floodLevel
     */
    public int getFloodLevel() {
        return floodLevel;
    }

    /**
     * @param floodLevel
     *            the floodLevel to set
     */
    public void setFloodLevel(int floodLevel) {
        this.floodLevel = floodLevel;
    }

    /**
     * @return the valueType
     */
    public int getValueType() {
        return valueType;
    }

    /**
     * @param valueType
     *            the valueType to set
     */
    public void setValueType(int valueType) {
        this.valueType = valueType;
    }

    /**
     * 0 = obs 1 = fcst 2 = max of obs and fcst.
     * 
     * @return the stageBasis
     */
    public int getStageBasis() {
        return stageBasis;
    }

    /**
     * 0 = obs 1 = fcst 2 = max of obs and fcst.
     * 
     * @param stageBasis
     *            the stageBasis to set
     */
    public void setStageBasis(int stageBasis) {
        this.stageBasis = stageBasis;
    }

/**
     * SHOW_ALL ("Any Value") 0,
     * SHOW_EQUAL ("Value =") 1,
     * SHOW_NOT_EQUAL ("Value Not =") 2,
     * SHOW_GREATER_EQUAL ("Value >=") 3,
     * SHOW_LESS_EQUAL ("Value <=") 4,
     * SHOW_GREATER ("Value >") 5,
     * SHOW_LESS ("Value <") 6;
     * @return the valueFilterOperation
     */
    public int getValueFilterOperation() {
        return valueFilterOperation;
    }

/**
     * SHOW_ALL ("Any Value") 0,
     * SHOW_EQUAL ("Value =") 1,
     * SHOW_NOT_EQUAL ("Value Not =") 2,
     * SHOW_GREATER_EQUAL ("Value >=") 3,
     * SHOW_LESS_EQUAL ("Value <=") 4,
     * SHOW_GREATER ("Value >") 5,
     * SHOW_LESS ("Value <") 6;
     * @param valueFilterOperation the valueFilterOperation to set
     */
    public void setValueFilterOperation(int valueFilterOperation) {
        this.valueFilterOperation = valueFilterOperation;
    }

    /**
     * @return the valueFilterValue
     */
    public double getValueFilterValue() {
        return valueFilterValue;
    }

    /**
     * @param valueFilterValue
     *            the valueFilterValue to set
     */
    public void setValueFilterValue(double valueFilterValue) {
        this.valueFilterValue = valueFilterValue;
    }

/**
     * SHOW_ALL ("Any Elev") 0,
     * SHOW_EQUAL ("Elev =") 1,
     * SHOW_NOT_EQUAL ("Elev Not =") 2,
     * SHOW_GREATER_EQUAL ("Elev >=") 3,
     * SHOW_LESS_EQUAL ("Elev <=") 4,
     * SHOW_GREATER ("Elev >") 5,
     * SHOW_LESS ("Elev <") 6;
     * 
     * @return the elevFilterOperation
     */
    public int getElevFilterOperation() {
        return elevFilterOperation;
    }

/**
     * SHOW_ALL ("Any Elev") 0,
     * SHOW_EQUAL ("Elev =") 1,
     * SHOW_NOT_EQUAL ("Elev Not =") 2,
     * SHOW_GREATER_EQUAL ("Elev >=") 3,
     * SHOW_LESS_EQUAL ("Elev <=") 4,
     * SHOW_GREATER ("Elev >") 5,
     * SHOW_LESS ("Elev <") 6;
     *
     * @param elevFilterOperation the elevFilterOperation to set
     */
    public void setElevFilterOperation(int elevFilterOperation) {
        this.elevFilterOperation = elevFilterOperation;
    }

    /**
     * @return the elevFilterValue
     */
    public double getElevFilterValue() {
        return elevFilterValue;
    }

    /**
     * @param elevFilterValue
     *            the elevFilterValue to set
     */
    public void setElevFilterValue(double elevFilterValue) {
        this.elevFilterValue = elevFilterValue;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Instant Precip Accum Time Sel: ["
                + getInstPrecipAccumTimeSelection() + "]\n");
        sb.append("TS Chosen Count:               ["
                + getTypeSourceChosenCount() + "]\n");
        sb
                .append("Element Type:                  [" + getElementType()
                        + "]\n");
        sb.append("Derive Stage Flow:             [" + getDeriveStageFlow()
                + "]\n");
        sb.append("Duration Hours:                [" + getDurHours() + "]\n");
        sb.append("Filter By Data Source:         [" + getFilterByDataSource()
                + "]\n");
        sb.append("Valid Time:                    ["
                + getValidTime().toString() + "]\n");
        sb.append("Elevation Filter Operation:    [" + getElevFilterOperation()
                + "]\n");
        sb.append("Elevation Filter Value:        [" + getElevFilterValue()
                + "]\n");
        sb.append("Flood Level:                   [" + getFloodLevel() + "]\n");
        sb
                .append("Forecast Points Only:          [" + getFcstptsOnly()
                        + "]\n");
        sb.append("Icon:                          [" + getIcon() + "]\n");
        sb.append("ID:                            [" + getId() + "]\n");
        sb.append("Name:                          [" + getName() + "]\n");
        sb.append("Data Source Chosen Count:      ["
                + getDataSourceChosenCount() + "]\n");
        sb.append("AdHoc Element String Full:     ["
                + getSelectedAdHocElementFullString() + "]\n");
        sb.append("AdHoc Element String:          ["
                + getSelectedAdHocElementString() + "]\n");
        sb.append("PC and PP:                     [" + getPcAndpp() + "]\n");
        sb.append("Primary:                       [" + getPrimary() + "]\n");
        sb.append("Process Selected:              [" + getProcessSelected()
                + "]\n");
        sb.append("Query Mode:                    [" + getQueryMode() + "]\n");
        sb
                .append("River Status:                  [" + getRiverStatus()
                        + "]\n");
        sb.append("Stage Basis:                   [" + getStageBasis() + "]\n");
        sb.append("Elevation:                     [" + getElevation() + "]\n");
        sb.append("River Station Filter:          [" + getRiverStationFilter()
                + "]\n");
        sb.append("Supress Missing:               [" + getSupressMissing()
                + "]\n");
        sb.append("Param Code:                    [" + getParamCode() + "]\n");
        sb.append("TS Chosen List:                [");

        for (String s : getTypeSourceChosenList()) {
            sb.append(s + "|");
        }

        sb.append("]\n");

        sb.append("TS Data Element:               [" + getTsDataElement()
                + "]\n");
        sb
                .append("Filter By HSA:                 [" + getFilterByHSA()
                        + "]\n");
        sb.append("Time:                          [" + getTime() + "]\n");
        sb.append("Time Mode:                     [" + getTimeMode() + "]\n");
        sb.append("Precip PE Filter:              [" + getPrecipPeFilter()
                + "]\n");
        sb.append("Filter By TS:                  [" + getFilterByTypeSource()
                + "]\n");
        sb.append("Value:                         [" + getValue() + "]\n");
        sb.append("Value Filter Operation:        ["
                + getValueFilterOperation() + "]\n");
        sb.append("Value Type:                    [" + getValueType() + "]\n");
        sb.append("Value Filter Value:            [" + getValueFilterValue()
                + "]\n");
        if (getHsaList() != null) {
            sb.append("HSA List:                      [");
            for (String s : getHsaList()) {
                sb.append(s + "|");
            }
            sb.append("]\n");
        }

        return sb.toString();
    }

    /**
     * @return the peSelection
     */
    public int getPeSelection() {
        return peSelection;
    }

    /**
     * @param peSelection
     *            the peSelection to set
     */
    public void setPeSelection(int peSelection) {
        this.peSelection = peSelection;
    }
}