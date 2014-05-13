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
package com.raytheon.uf.viz.monitor.config;

import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.VarName;
import com.raytheon.uf.viz.monitor.data.ColumnAttribData;

/**
 * Common configuration data for the table (zone, station, obs history, etc.).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            lvenable     Initial creation
 * Dec 17, 2009 3424      zhao         added snowZoneStnTableColVarNames
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class CommonTableConfig {
    /**
     * Instance of this class.
     */
    private static CommonTableConfig classInstance;

    /**
     * Graph type enumeration.
     */
    public static enum GraphType {
        Trend, HodoWindDir, None;
    }

    /**
     * Obs History configuration enumeration.
     */
    public static enum ObsHistType {
        Maritime, METAR;
    }

    /**
     * Data cell type enumeration.
     */
    public static enum CellType {
        R("R"), Y("Y"), G("G"), W("W"), NotAvailable("N/A"), NotMonitored("NM"), NotDetermined(
        "N/D"), AreaId(""), Default(""), Custom(""), ObsHist(""), StationID(
        ""), PresWx("");

        String displayString;

        CellType(String displayStr) {
            displayString = displayStr;
        }

        public String getDisplayString() {
            return displayString;
        }
    };

    /**
     * Sort direction enumeration.
     */
    public static enum SortDirection {
        Ascending(SWT.UP), Decending(SWT.DOWN), Both(-1), None(SWT.NONE);

        private int sortDir;

        SortDirection(int sort) {
            this.sortDir = sort;
        }

        public int getSortDir() {
            return sortDir;
        }
    };

    /**
     * SAFESEAS Zone & Station table column "keys" used to access table/column
     * attributes.
     */
    private final String[] ssZoneStnTableCols = new String[] { "SSZT_AreaId",
            "SSZT_SCA", "SSZT_GaleWarning", "SSZT_StormWarning", "SSZT_HFWW",
            "SSZT_WindDir", "SSZT_WindSpeed", "SSZT_PeakWind", "SSZT_WindGust",
            "SSZT_Vis", "SSZT_Temp", "SSZT_Dewpt", "SSZT_SLP", "SSZT_SST",
            "SSZT_WaveHeight", "SSZT_WaveSteep", "SSZT_SwellHeight",
            "SSZT_SwellPeriod", "SSZT_SwellDir", "SSZT_Swell2Height",
            "SSZT_Swell2Period", "SSZT_Swell2Dir", "SSZT_Fog" };

    /**
     * Snow obs variable names corresponding to snowZoneStnTableCols
     */
    private final ObConst.VarName[] ssZoneStnTableColVarNames = new ObConst.VarName[] {
            ObConst.VarName.ZONE_ID,
            ObConst.VarName.UNDEFINED_VARIABLE, // SCA
            ObConst.VarName.UNDEFINED_VARIABLE, // Gale warning
            ObConst.VarName.UNDEFINED_VARIABLE, // Storm warning
            ObConst.VarName.UNDEFINED_VARIABLE, // HFWW
            ObConst.VarName.WIND_DIR, ObConst.VarName.WIND_SPEED,
            ObConst.VarName.MAX_WIND_SPEED, ObConst.VarName.GUST_SPEED,
            ObConst.VarName.VISIBILITY, ObConst.VarName.TEMPERATURE,
            ObConst.VarName.DEWPOINT, ObConst.VarName.SEA_LEVEL_PRESS,
            ObConst.VarName.SEA_SURFACE_TEMPERATURE,
            ObConst.VarName.WAVE_HEIGHT, ObConst.VarName.WAVE_STEEPNESS,
            ObConst.VarName.PRIM_SWELL_HT, ObConst.VarName.PRIM_SWELL_PD,
            ObConst.VarName.PRIM_SWELL_DIR, ObConst.VarName.SEC_SWELL_HT,
            ObConst.VarName.SEC_SWELL_PD, ObConst.VarName.SEC_SWELL_DIR,
            ObConst.VarName.FOG };

    /**
     * Snow Zone & Station table column "keys" used to access table/column
     * attributes.
     */
    private final String[] snowZoneStnTableCols = new String[] { "SNOW_AreaId",
            "SNOW_BlizWarn", "SNOW_FrzPrecip", "SNOW_HvySnowWarn",
            "SNOW_PresWx", "SNOW_WindDir", "SNOW_WindSpeed", "SNOW_PeakWind",
            "SNOW_WindGust", "SNOW_Temp", "SNOW_Dewpt", "SNOW_Vis", "SNOW_SLP",
            "SNOW_WindChill", "SNOW_FrstBite", "SNOW_HourlyPrecip",
            "SNOW_SnowDepth", "SNOW_SnIncRHrly", "SNOW_SnIncRTot" };

    /**
     * Snow obs variable names corresponding to snowZoneStnTableCols
     */
    private final ObConst.VarName[] snowZoneStnTableColVarNames = new ObConst.VarName[] {
            ObConst.VarName.ZONE_ID,
            ObConst.VarName.UNDEFINED_VARIABLE, // BlizWarn
            ObConst.VarName.UNDEFINED_VARIABLE, // FrzPrecip
            ObConst.VarName.UNDEFINED_VARIABLE, // HvySnowWarn
            ObConst.VarName.PRES_WX, ObConst.VarName.WIND_DIR,
            ObConst.VarName.WIND_SPEED, ObConst.VarName.MAX_WIND_SPEED,
            ObConst.VarName.GUST_SPEED, ObConst.VarName.TEMPERATURE,
            ObConst.VarName.DEWPOINT, ObConst.VarName.VISIBILITY,
            ObConst.VarName.SEA_LEVEL_PRESS, ObConst.VarName.WIND_CHILL,
            ObConst.VarName.FROSTBITE_TIME, ObConst.VarName.HOURLY_PRECIP,
            ObConst.VarName.SNOW_DEPTH, ObConst.VarName.SNINCR_HOURLY,
            ObConst.VarName.SNINCR_TOTAL };

    /**
     * Fog Zone & Station table column "keys" used to access table/column
     * attributes.
     */
    private final String[] fogZoneStnTableCols = new String[] { "FOG_AreaId",
            "FOG_Vis", "FOG_PresWx", "FOG_Ceiling", "FOG_WindDir",
            "FOG_WindSpeed", "FOG_PeakWind", "FOG_WindGust", "FOG_Temp",
            "FOG_Dewpt", "FOG_Depr", "FOG_RelHumid", "FOG_ALG" };

    private final ObConst.VarName[] fogZoneStnTableColVarNames = new ObConst.VarName[] {
            ObConst.VarName.ZONE_ID, ObConst.VarName.VISIBILITY,
            ObConst.VarName.PRES_WX, ObConst.VarName.CEILING,
            ObConst.VarName.WIND_DIR, ObConst.VarName.WIND_SPEED,
            ObConst.VarName.MAX_WIND_SPEED, ObConst.VarName.GUST_SPEED,
            ObConst.VarName.TEMPERATURE, ObConst.VarName.DEWPOINT,
            ObConst.VarName.DEWPOINT_DEPR, ObConst.VarName.RELATIVE_HUMIDITY,
            ObConst.VarName.UNDEFINED_VARIABLE // ALG
    };

    /**
     * Zone/Station column attribute map.
     */
    private HashMap<String, ColumnAttribData> tableColumnAttrMap;

    /**
     * Available Observation History columns for SAFESEAS, Snow, Fog.
     */
    public static enum obsHistCols {
        Time, Lat, Lon, WindSpd, MaxWindSpd, WindGust, WindDir, Vis_mi, Vis_mn, P, PTend, SigWaveHgt, SwellHgt, SwellPer, SwellDir, T, Dewpt, SST, WaveSteep, RelHum, Ceiling, DewptDepr
    };

    /**
     * SAFESEAS Maritime table columns.
     */
	private final String[] ssMaritimeTableCols = new String[] {
			obsHistCols.Time.name(), obsHistCols.Lat.name(),
			obsHistCols.Lon.name(), obsHistCols.WindSpd.name(),
			obsHistCols.MaxWindSpd.name(), obsHistCols.WindGust.name(),
			obsHistCols.Vis_mn.name(), obsHistCols.P.name(),
			obsHistCols.PTend.name(), obsHistCols.T.name(),
			obsHistCols.Dewpt.name(), obsHistCols.SST.name(),
			obsHistCols.SigWaveHgt.name(), obsHistCols.WaveSteep.name(),
			obsHistCols.SwellHgt.name(), obsHistCols.SwellPer.name(),
			obsHistCols.SwellDir.name() };

	/**
	 * SAFESEAS METAR table columns.
	 */
	private final String[] ssMetarTableCols = new String[] {
			obsHistCols.Time.name(), obsHistCols.Lat.name(),
			obsHistCols.Lon.name(), obsHistCols.WindDir.name(),
			obsHistCols.WindSpd.name(), obsHistCols.WindGust.name(),
			obsHistCols.P.name(), obsHistCols.T.name(),
			obsHistCols.Dewpt.name(), obsHistCols.PTend.name() };

	/**
	 * Snow METAR table columns.
	 */
	private final String[] snowMetarTableCols = new String[] {
			obsHistCols.Time.name(), obsHistCols.Lat.name(),
			obsHistCols.Lon.name(), obsHistCols.WindDir.name(),
			obsHistCols.WindSpd.name(), obsHistCols.WindGust.name(),
			obsHistCols.P.name(), obsHistCols.T.name(),
			obsHistCols.Dewpt.name(), obsHistCols.PTend.name() };

    /**
     * METAR configure names
     */
    private final String[] metarConfigureNames = new String[] {
    		"Lat (deg)", "Lon (deg)", "Wind Dir (deg)", "Wind Spd (kt)", "Wind Gust (kt)", "P (in)",
            "T (deg F)", "Dewpt (deg F)", "P Tendency (in)" };

    /**
     * Fog METAR configure names
     */

    private final String[] metarFogConfigureNames = new String[] {
            "Lat (deg)", "Lon (deg)", "Rel Hum (%)", "Vis (mi)", "Ceiling (ft x 100)", "Wind Dir (deg)",
            "Wind Spd (kt)", "Wind Gust (kt)", "P (in)", "T (deg F)",
            "Dewpt (deg F)", "DewptDepr (deg F)", "P Tendency (in)" };

    /**
     * Fog Maritime configure names
     */

    private final String[] maritimeFogConfigureNames = new String[] {
    		"Lat (deg)", "Lon (deg)", "Wind Dir (deg)", "Wind Spd (kt)", "Wind Gust (kt)", "Vis (nm)",
            "P (in)", "P Tendency (in)", "T (deg F)", "Dewpt (deg F)",
            "SST (deg F)", "Significant Wave Hgt (ft)", "Wave Steep",
            "Swell Hgt (ft)", "Swell Per (sec)", "Swell Dir (deg)",
    "Rel Hum (%)" };

    /**
     * SAFESEAS Maritime configure names
     */

    private final String[] maritimeSSConfigureNames = new String[] {
    		"Lat (deg)", "Lon (deg)", "Wind Spd (kt)", "MaxWindSpd (kt)", "Wind Gust (kt)", "Vis (nm)",
            "P (in)", "P Tendency (in)", "T (deg F)", "Dewpt (deg F)",
            "SST (deg F)", "Significant Wave Hgt (ft)", "Wave Steep",
            "Swell Hgt (ft)", "Swell Per (sec)", "Swell Dir (deg)" };

    /**
     * Fog Maritime table columns.
     */
	private final String[] fogMaritimeTableCols = new String[] {
			obsHistCols.Time.name(), obsHistCols.Lat.name(),
			obsHistCols.Lon.name(), obsHistCols.WindSpd.name(),
			obsHistCols.MaxWindSpd.name(), obsHistCols.WindGust.name(),
			obsHistCols.Vis_mi.name(), obsHistCols.P.name(),
			obsHistCols.PTend.name(), obsHistCols.T.name(),
			obsHistCols.Dewpt.name(), obsHistCols.SST.name(),
			obsHistCols.SigWaveHgt.name(), obsHistCols.WaveSteep.name(),
			obsHistCols.SwellHgt.name(), obsHistCols.SwellPer.name(),
			obsHistCols.SwellDir.name(), obsHistCols.RelHum.name() };

    /**
     * Fog METAR table columns.
     */
    private final String[] fogMetarTableCols = new String[] {
            obsHistCols.Time.name(), obsHistCols.Lat.name(),
			obsHistCols.Lon.name(),obsHistCols.RelHum.name(),
            obsHistCols.Vis_mi.name(), obsHistCols.Ceiling.name(),
            obsHistCols.WindDir.name(), obsHistCols.WindSpd.name(),
            obsHistCols.WindGust.name(), obsHistCols.P.name(),
            obsHistCols.T.name(), obsHistCols.Dewpt.name(),
            obsHistCols.DewptDepr.name(), obsHistCols.PTend.name() };

    /**
     * Map of Observation History column attributes.
     */
    private HashMap<String, ColumnAttribData> obsHistColumnAttrMap;

    /**
     * Map of RGB for table cell background colors.
     */
    private HashMap<CellType, RGB> colorMap;

    /**
     * Constructor.
     */
    private CommonTableConfig() {
        initData();
    }

    /**
     * Create an instance of this class.
     * 
     * @return An instance of this class.
     */
    public static synchronized CommonTableConfig getInstance() {
        if (classInstance == null) {
            classInstance = new CommonTableConfig();
        }
        return classInstance;
    }

    /**
     * Initialize the data.
     */
    private void initData() {
        createColorMap();
        createColumnAttributeMaps();
    }

    /**
     * Create the map of RGB colors.
     */
    private void createColorMap() {
        colorMap = new HashMap<CellType, RGB>();

        colorMap.put(CellType.R, new RGB(230, 0, 0));
        colorMap.put(CellType.Y, new RGB(245, 245, 0));
        colorMap.put(CellType.G, new RGB(0, 235, 0));
        colorMap.put(CellType.W, new RGB(255, 255, 255));
        colorMap.put(CellType.NotAvailable, new RGB(229, 229, 229));
        colorMap.put(CellType.NotMonitored, new RGB(191, 191, 191));
        colorMap.put(CellType.NotDetermined, new RGB(191, 191, 191));
        colorMap.put(CellType.AreaId, new RGB(220, 175, 55));
        colorMap.put(CellType.Default, new RGB(255, 255, 255));
        colorMap.put(CellType.Custom, new RGB(255, 255, 255));
        colorMap.put(CellType.ObsHist, new RGB(245, 215, 85));
        colorMap.put(CellType.PresWx, new RGB(229, 229, 229));
    }

    /**
     * Create the Zone/Station & Ons History column maps.
     */
    private void createColumnAttributeMaps() {
        /*
         * Create zone/station attribute map
         */
        tableColumnAttrMap = new HashMap<String, ColumnAttribData>();

        createSafeSeasZoneStationColumnAttributes();
        createSnowZoneStationColumnAttributes();
        createFogZoneStationColumnAttributes();

        /*
         * Create obs history attribute map
         */
        obsHistColumnAttrMap = new HashMap<String, ColumnAttribData>();

        createObsHistoryColumnAttributes();
    }

    /**
     * Create the SAFESEAS Zone/Station column attributes.
     */
    private void createSafeSeasZoneStationColumnAttributes() {
        tableColumnAttrMap.put(ssZoneStnTableCols[0], new ColumnAttribData(
                "Area_Id", SortDirection.Ascending));
        tableColumnAttrMap.put(ssZoneStnTableCols[1], new ColumnAttribData(
                "SCA", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[2], new ColumnAttribData(
                "Gale Warning", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[3], new ColumnAttribData(
                "Storm Warning", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[4], new ColumnAttribData(
                "HFWW", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[5], new ColumnAttribData(
                "Wind Direc", SortDirection.Decending, GraphType.HodoWindDir));
        tableColumnAttrMap.put(ssZoneStnTableCols[6], new ColumnAttribData(
                "Wind Speed", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[7], new ColumnAttribData(
                "Peak Wind", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[8], new ColumnAttribData(
                "Wind Gust", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[9], new ColumnAttribData(
                "Vis", SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[10], new ColumnAttribData(
                "Temp", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[11], new ColumnAttribData(
                "Dewpt", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[12], new ColumnAttribData(
                "SLP", SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[13], new ColumnAttribData(
                "SST", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[14], new ColumnAttribData(
                "Wave Height", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[15], new ColumnAttribData(
                "Wave Steep", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[16], new ColumnAttribData(
                "Swell Height", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[17], new ColumnAttribData(
                "Swell Period", SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[18], new ColumnAttribData(
                "Swell Dir", SortDirection.Ascending, GraphType.HodoWindDir));
        tableColumnAttrMap.put(ssZoneStnTableCols[19], new ColumnAttribData(
                "Swell2 Height", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[20], new ColumnAttribData(
                "Swell2 Period", SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(ssZoneStnTableCols[21], new ColumnAttribData(
                "Swell2 Dir", SortDirection.Ascending, GraphType.HodoWindDir));
        tableColumnAttrMap.put(ssZoneStnTableCols[22], new ColumnAttribData(
                "Fog",
                SortDirection.Decending, GraphType.None));
    }

    /**
     * Create the Snow Zone/Station column attributes.
     */
    private void createSnowZoneStationColumnAttributes() {
        tableColumnAttrMap.put(snowZoneStnTableCols[0], new ColumnAttribData(
                "Area_Id", SortDirection.Ascending));
        tableColumnAttrMap.put(snowZoneStnTableCols[1], new ColumnAttribData(
                "Bliz Warn", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[2], new ColumnAttribData(
                "Frz Precip", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[3], new ColumnAttribData(
                "Hsnow Warn", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[4], new ColumnAttribData(
                "Pres Weather", SortDirection.None, GraphType.None));
        tableColumnAttrMap.put(snowZoneStnTableCols[5], new ColumnAttribData(
                "Wind Direc", SortDirection.Decending, GraphType.HodoWindDir));
        tableColumnAttrMap.put(snowZoneStnTableCols[6], new ColumnAttribData(
                "Wind Speed", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[7], new ColumnAttribData(
                "Peak Wind", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[8], new ColumnAttribData(
                "Wind Gust", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[9], new ColumnAttribData(
                "Temp", SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[10], new ColumnAttribData(
                "Dewpt", SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[11], new ColumnAttribData(
                "Vis", SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[12], new ColumnAttribData(
                "SLP",
                SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[13], new ColumnAttribData(
                "Wind Chill", SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[14], new ColumnAttribData(
                "FrstBt Time",
                SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[15], new ColumnAttribData(
                "Hourly Precip", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[16], new ColumnAttribData(
                "Snow Depth", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[17], new ColumnAttribData(
                "SNINCR Hourly", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(snowZoneStnTableCols[18], new ColumnAttribData(
                "SNINCR Total", SortDirection.Decending, GraphType.Trend));
    }

    /**
     * Create the Fog Zone/Station column attributes.
     */
    private void createFogZoneStationColumnAttributes() {
        tableColumnAttrMap.put(fogZoneStnTableCols[0], new ColumnAttribData(
                "Area_Id", SortDirection.Ascending));
        tableColumnAttrMap.put(fogZoneStnTableCols[1], new ColumnAttribData(
                "Vis",
                SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(fogZoneStnTableCols[2], new ColumnAttribData(
                "Pres Weather", SortDirection.None, GraphType.None));
        tableColumnAttrMap.put(fogZoneStnTableCols[3], new ColumnAttribData(
                "Ceiling",
                SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(fogZoneStnTableCols[4], new ColumnAttribData(
                "Wind Direc", SortDirection.Decending, GraphType.HodoWindDir));
        tableColumnAttrMap.put(fogZoneStnTableCols[5], new ColumnAttribData(
                "Wind Speed", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(fogZoneStnTableCols[6], new ColumnAttribData(
                "Peak Wind", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(fogZoneStnTableCols[7], new ColumnAttribData(
                "Wind Gust", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(fogZoneStnTableCols[8], new ColumnAttribData(
                "Temp",
                SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(fogZoneStnTableCols[9], new ColumnAttribData(
                "Dewpt", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(fogZoneStnTableCols[10], new ColumnAttribData(
                "Depr",
                SortDirection.Ascending, GraphType.Trend));
        tableColumnAttrMap.put(fogZoneStnTableCols[11], new ColumnAttribData(
                "Rel Humid", SortDirection.Decending, GraphType.Trend));
        tableColumnAttrMap.put(fogZoneStnTableCols[12], new ColumnAttribData(
                "ALG",
                SortDirection.Decending, GraphType.None));
    }

	/**
	 * Create the Observation History column attributes.
	 */
	private void createObsHistoryColumnAttributes() {
		obsHistColumnAttrMap.put(obsHistCols.Time.name(), new ColumnAttribData(
				"Time (UTC)", "Time (UTC)"));
		obsHistColumnAttrMap.put(obsHistCols.Lat.name(), new ColumnAttribData(
				"Lat (deg)", "Lat (deg)"));
		obsHistColumnAttrMap.put(obsHistCols.Lon.name(), new ColumnAttribData(
				"Lon (deg)", "Lon (deg)"));
		obsHistColumnAttrMap.put(obsHistCols.WindSpd.name(),
				new ColumnAttribData("Wind Spd (kt)", "Wind Spd\n(kt)"));
		obsHistColumnAttrMap
				.put(obsHistCols.MaxWindSpd.name(), new ColumnAttribData(
						"Max Wind Spd (kt)", "Max Wind\nSpd (kt)"));
		obsHistColumnAttrMap.put(obsHistCols.WindGust.name(),
				new ColumnAttribData("Wind Gust (kt)", "Wind Gust\n(kt)"));
		obsHistColumnAttrMap.put(obsHistCols.WindDir.name(),
				new ColumnAttribData("Wind Dir (deg)", "Wind Dir\n(deg)"));
		obsHistColumnAttrMap.put(obsHistCols.Vis_mi.name(),
				new ColumnAttribData("Vis (mi)", "Vis (mi)"));
		obsHistColumnAttrMap.put(obsHistCols.Vis_mn.name(),
				new ColumnAttribData("Vis (nm)", "Vis (nm)"));
		obsHistColumnAttrMap.put(obsHistCols.P.name(), new ColumnAttribData(
				"P (in)", "P (in)"));
		obsHistColumnAttrMap.put(obsHistCols.PTend.name(),
				new ColumnAttribData("P Tendency (in)", "P Tendency\n(in)"));
		obsHistColumnAttrMap.put(obsHistCols.SigWaveHgt.name(),
				new ColumnAttribData("Significant Wave Hgt (ft)",
						"Significant\nWave Hgt (ft)"));
		obsHistColumnAttrMap.put(obsHistCols.SwellHgt.name(),
				new ColumnAttribData("Swell Hgt (ft)", "Swell Hgt\n(ft)"));
		obsHistColumnAttrMap.put(obsHistCols.SwellPer.name(),
				new ColumnAttribData("Swell Per (sec)", "Swell Per\n(sec)"));
		obsHistColumnAttrMap.put(obsHistCols.SwellDir.name(),
				new ColumnAttribData("Swell Dir (deg)", "Swell Dir\n(deg)"));
		obsHistColumnAttrMap.put(obsHistCols.T.name(), new ColumnAttribData(
				"T (deg F)", "T (deg F)"));
		obsHistColumnAttrMap.put(obsHistCols.Dewpt.name(),
				new ColumnAttribData("Dewpt (deg F)", "Dewpt\n(deg F)"));
		obsHistColumnAttrMap.put(obsHistCols.SST.name(), new ColumnAttribData(
				"SST (deg F)", "SST\n(deg F)"));
		obsHistColumnAttrMap.put(obsHistCols.WaveSteep.name(),
				new ColumnAttribData("Wave Steep", "Wave\nSteep"));
		obsHistColumnAttrMap.put(obsHistCols.RelHum.name(),
				new ColumnAttribData("Rel. Humid (%)", "Rel. Humid\n(%)"));
		obsHistColumnAttrMap.put(obsHistCols.Ceiling.name(),
				new ColumnAttribData("Ceiling (ft x 100)",
						"Ceiling\n(ft x 100)"));
		obsHistColumnAttrMap
				.put(obsHistCols.DewptDepr.name(), new ColumnAttribData(
						"DewptDepr (deg F)", "DewptDepr\n(deg F)"));

	}

    /**
     * Get the Zone/Station column keys.
     * 
     * @param app
     *            Application name.
     * @return String array of column keys.
     */
    public String[] getTableColumnKeys(CommonConfig.AppName app) {
        if (app == CommonConfig.AppName.SAFESEAS) {
            return ssZoneStnTableCols;
        } else if (app == CommonConfig.AppName.SNOW) {
            return snowZoneStnTableCols;
        } else if (app == CommonConfig.AppName.FOG) {
            return fogZoneStnTableCols;
        }
        return new String[0];
    }

    /**
     * Get history configuration column names.
     */
    public String[] getMetarConfigureNames() {
        return metarConfigureNames;
    }

    public String[] getMetarFogConfigureNames() {
        return metarFogConfigureNames;
    }

    public String[] getMaritimeFogConfigureNames() {
        return maritimeFogConfigureNames;
    }

    public String[] getMaritimeSSConfigureNames() {
        return maritimeSSConfigureNames;
    }

    /**
     * Get observation history column keys.
     * 
     * @param app
     *            Application name.
     * @param obsType
     *            Observation type.
     * @return String array of column keys.
     */
    public String[] getObsHistColumnKeys(CommonConfig.AppName app,
            ObsHistType obsType) {
        if (app == CommonConfig.AppName.SAFESEAS) {
            if (obsType == ObsHistType.Maritime) {
                return ssMaritimeTableCols;
            } else {
                return ssMetarTableCols;
            }
        } else if (app == CommonConfig.AppName.SNOW) {
            if (obsType == ObsHistType.METAR) {
                return snowMetarTableCols;
            }
        } else if (app == CommonConfig.AppName.FOG) {
            if (obsType == ObsHistType.Maritime) {
                return fogMaritimeTableCols;
            } else {
                return fogMetarTableCols;
            }
        }

        return new String[0];
    }

    /**
     * Get the Zone/Station column index.
     * 
     * @param app
     *            Application name.
     * @param columnName
     *            Column name/key.
     * @return The column index.
     */
    public int getTableColumnIndex(CommonConfig.AppName app, String columnName) {
        if (app == CommonConfig.AppName.SAFESEAS) {
            return getColumnIndex(columnName, ssZoneStnTableCols);
        } else if (app == CommonConfig.AppName.SNOW) {
            return getColumnIndex(columnName, snowZoneStnTableCols);
        } else if (app == CommonConfig.AppName.FOG) {
            return getColumnIndex(columnName, fogZoneStnTableCols);
        }

        return 0;
    }

    /**
     * Get the number of observation history columns.
     * 
     * @param app
     *            Application name.
     * @param obsType
     *            Observation type.
     * @return Number of columns.
     */
    public int getNumberOfObsHistColumns(CommonConfig.AppName app,
            ObsHistType obsType) {
        if (app == CommonConfig.AppName.SAFESEAS) {
            if (obsType == ObsHistType.Maritime) {
                return ssMaritimeTableCols.length;
            } else {
                return ssMetarTableCols.length;
            }

        } else if (app == CommonConfig.AppName.SNOW) {
            if (obsType == ObsHistType.METAR) {
                return snowMetarTableCols.length;
            }
        } else if (app == CommonConfig.AppName.FOG) {
            if (obsType == ObsHistType.Maritime) {
                return fogMaritimeTableCols.length;
            } else {
                return fogMetarTableCols.length;
            }
        }

        return 0;
    }

    /**
     * Get the observation history column index.
     * 
     * @param app
     *            Application name.
     * @param obsType
     *            Observation type.
     * @param columnName
     *            Column name/key.
     * @return Column index.
     */
    public int getObsHistColumnIndex(CommonConfig.AppName app,
            ObsHistType obsType, String columnName) {
        if (app == CommonConfig.AppName.SAFESEAS) {
            if (obsType == ObsHistType.Maritime) {
                return getColumnIndex(columnName, ssMaritimeTableCols);
            } else {
                return getColumnIndex(columnName, ssMetarTableCols);
            }

        } else if (app == CommonConfig.AppName.SNOW) {
            if (obsType == ObsHistType.METAR) {
                return getColumnIndex(columnName, snowMetarTableCols);
            }
        } else if (app == CommonConfig.AppName.FOG) {
            if (obsType == ObsHistType.Maritime) {
                return getColumnIndex(columnName, fogMaritimeTableCols);
            } else {
                return getColumnIndex(columnName, fogMetarTableCols);
            }
        }

        return 0;
    }

    /**
     * Get column index.
     * 
     * @param name
     *            Column name/key.
     * @param columns
     *            String of column names/keys.
     * @return Column index.
     */
    private int getColumnIndex(String columnName, String[] columnNames) {
        for (int i = 0; i < columnNames.length; i++) {
            if (columnName.equals(columnNames[i]) == true) {
                return i;
            }
        }

        return 0;
    }

    /**
     * Get the column attribute data for the Zone/Station table.
     * 
     * @param columnNameKey
     *            Column name/key.
     * @return Column attribute data.
     */
    public ColumnAttribData getTableColumnAttr(String columnNameKey) {
        return tableColumnAttrMap.get(columnNameKey);
    }

    /**
     * Get the column attribute data for the Observation History table.
     * 
     * @param columnNameKey
     *            Column name/key.
     * @return Column attribute data.
     */
    public ColumnAttribData getObsHistColumnAttr(String columnNameKey) {
        return obsHistColumnAttrMap.get(columnNameKey);
    }

    /**
     * Get the cell background color.
     * 
     * @param type
     *            Cell type.
     * @return RGB color.
     */
    public RGB getCellColor(CellType type) {
        return colorMap.get(type);
    }

    /**
     * Get the Zone/Station default column width.
     * 
     * @param app
     *            Application name.
     * @return Default column width.
     */
    public int getTableDefaultColWidth(CommonConfig.AppName app) {
        if (app == CommonConfig.AppName.SAFESEAS) {
            return 60;
        } else if (app == CommonConfig.AppName.SNOW) {
            return 68;
        } else if (app == CommonConfig.AppName.FOG) {
            return 70;
        }

        return 75;
    }

    /**
     * Get the Observation History default column width.
     * 
     * @return The default column width.
     */
    public int getObsHistDefaultColWidth() {
        return 70;
    }

    /**
     * Gets a string that will be used for sorting (prefixed to existing data).
     * 
     * @param dir
     *            Sort direction.
     * @param type
     *            Cell type.
     * @return String "offset".
     */
    public String getStringOffset(SortDirection dir, CellType type) {
        return getStringOffset(dir.getSortDir(), type);
    }

    /**
     * Gets a string that will be used for sorting (prefixed to existing data).
     * 
     * @param dir
     *            Sort direction.
     * @param type
     *            Cell type.
     * @return String "offset".
     */
    public String getStringOffset(int dir, CellType type) {
        if (dir == SortDirection.Ascending.getSortDir()) {
            switch (type) {
            case R:
                return "100000";
            case Y:
                return "200000";
            case G:
                return "300000";
            case W:
                return "300000";
            case NotAvailable:
                return "7777777";
            case NotMonitored:
                return "88888888";
            case NotDetermined:
                return "99999999";
            }
        } else if (dir == SortDirection.Decending.getSortDir()) {
            switch (type) {
            case R:
                return "300000";
            case Y:
                return "200000";
            case G:
                return "100000";
            case W:
                return "100000";
            case NotAvailable:
                return "-7777777";
            case NotMonitored:
                return "-8888888";
            case NotDetermined:
                return "-9999999";
            }
        }

        return "-9999999";
    }

    /**
     * 
     * @return array of enumerated-type of Snow zone/station table column
     *         variable names
     */
    public ObConst.VarName[] getSnowZoneStnTableColVarNames() {
        return snowZoneStnTableColVarNames;
    }

    /**
     * 
     * @return array of enumerated-type of Safeseas zone/station table column
     *         variable names
     */
    public ObConst.VarName[] getSafeseasZoneStnTableColVarNames() {
        return ssZoneStnTableColVarNames;
    }

    /**
     * 
     * @return array of enumerated-type of Fog zone/station table column
     *         variable names
     */
    public ObConst.VarName[] getFogZoneStnTableColVarNames() {
        return fogZoneStnTableColVarNames;
    }

}
