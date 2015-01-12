package gov.noaa.nws.ncep.common.dataplugin.ncpafm;

import gov.noaa.nws.ncep.common.dataplugin.ncpafm.dao.NcPafmDao;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
//import gov.noaa.nws.ncep.common.dataplugin.ncpafm.NcPafmLayerData;

/**
 * Provides a transform from NcPafmRecords to PointDataContainer and vice versa.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 30 Sep 2011  126        B. Hebbard  Initial creation (from ncpirep)
 * 14 Oct 2011  126        B. Hebbard  Change windDir and pwindDir from String to Float;
 *                                     decoder will do conversion (vs. resource) per GH/SJ
 * Jul 30, 2014 3410        bclement   dataURI no longer stored in hdf5
 * </pre>
 * 
 * @author bhebbard
 * @version 1.0
 */

public class NcPafmPointDataTransform {

    private static final String DATAURI = "dataURI";    
    private static final String WMO_HEADER = "wmoHeader";
    private static final String STATION_ID = "stationId";
    private static final String REPORT_TYPE = "reportType";

    private static final String FORECAST_TIME_UTC = "forecastTimeUtc";
    private static final String FORECAST_TIME_LOCAL = "forecastTimeLocal";
    private static final String FORECAST_TIME_ZONE = "forecastTimeZone";
    private static final String AVG_MX_TMPF = "avgMxTmpf";
    private static final String HI_MX_TMPF = "hiMxTmpf";
    private static final String LO_MX_TMPF = "loMxTmpf";
    private static final String AVG_MN_TMPF = "avgMnTmpf"; 
    private static final String HI_MN_TMPF = "hiMnTmpf";
    private static final String LO_MN_TMPF = "loMnTmpf";
    private static final String TMPF = "tmpf";
    private static final String DWPF = "dwpf";
    private static final String RELH = "relh";
    private static final String WIND_DIR = "windDir";
    private static final String WIND_S_MPH = "windSmph";
    private static final String GUST_MPH = "gust_Mph";
    private static final String P_WIND_DIR = "pwindDir";
    private static final String WIND_CHAR = "windChar";
    private static final String SKY_COVER = "skyCover";
    private static final String POP12 = "pop12";
    private static final String QPF_12_MN = "qpf12Mn";
    private static final String QPF_12_MX = "qpf12Mx";
    private static final String HIGHEST_MAX_QPF = "highestMaxQpf";
    private static final String LOWEST_MAX_QPF = "lowestMaxQpf";
    private static final String SNOW_12_MN = "snow12Mn";
    private static final String SNOW_12_MX = "snow12Mx";
    private static final String AVG_SKY_COVER = "avgSkyCover";
    private static final String OBVIS = "obvis";
    private static final String WIND_CHILL = "windChill";
    private static final String HEAT_INDEX = "heatIndex";
    private static final String MIN_CHILL = "minChill";
    private static final String MAX_HEAT = "maxHeat";
    private static final String RAIN = "rain";
    private static final String RAIN_SHWRS = "rainShwrs";
    private static final String SPRINKLES = "sprinkles";
    private static final String TSTMS = "tstms";
    private static final String DRIZZLE = "drizzle";
    private static final String SNOW = "snow";  
    private static final String SNOW_SHWRS = "snowShwrs";
    private static final String FLURRIES = "flurries";
    private static final String SLEET = "sleet";
    private static final String FRZGRAIN = "frzgRain";
    private static final String FRZGDRZL = "frzgDrzl";
    private static final String HAZARDS = "hazards";

    /**
     * It is important to keep this up to date or risk breaking backwards
     * compatibility
     */
    private static final String[] ALL_PARAMS = { 
    	DATAURI, WMO_HEADER, STATION_ID, REPORT_TYPE,
        FORECAST_TIME_UTC, FORECAST_TIME_LOCAL, FORECAST_TIME_ZONE,
        AVG_MX_TMPF, HI_MX_TMPF, LO_MX_TMPF,
        AVG_MN_TMPF, HI_MN_TMPF, LO_MN_TMPF, TMPF, DWPF, RELH,
        WIND_DIR, WIND_S_MPH, GUST_MPH, P_WIND_DIR, WIND_CHAR, SKY_COVER,
        POP12, QPF_12_MN, QPF_12_MX, HIGHEST_MAX_QPF, LOWEST_MAX_QPF,
        SNOW_12_MN, SNOW_12_MX, AVG_SKY_COVER, OBVIS,
        WIND_CHILL, HEAT_INDEX, MIN_CHILL, MAX_HEAT, 
        RAIN, RAIN_SHWRS, SPRINKLES, TSTMS, DRIZZLE, SNOW,
        SNOW_SHWRS, FLURRIES, SLEET, FRZGRAIN, FRZGDRZL, HAZARDS };

    public static final String ALL_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (String s : ALL_PARAMS) {
            if (!first) {
                sb.append(", ");
            } else {
                first = false;
            }
            sb.append(s);
        }
        ALL_PARAMS_LIST = sb.toString();
    }

    private NcPafmDao dao;

    private PointDataDescription pdd;

    public NcPafmPointDataTransform() {
        try {
            this.dao = new NcPafmDao("ncpafm");
            this.pdd = dao.getPointDataDescription();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public PluginDataObject[] toPointData(PluginDataObject[] pdo) {
    	
        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();
            for (PluginDataObject p : pdo) {
                if (!(p instanceof NcPafmRecord))
                    continue;
                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.pdd);
                    pointMap.put(f, pdc);
                }
                NcPafmRecord npr = (NcPafmRecord) p;
                PointDataView pdv = buildView(pdc, npr);
                npr.setPointDataView(pdv);
            }
        }
        return pdo;
    }

    private PointDataView buildView(PointDataContainer container,
            NcPafmRecord record) {
        PointDataView pdv = container.append();

        pdv.setString(REPORT_TYPE, record.getReportType());
        
        NcPafmParameters npp = record.getPafmParms();

        //TODO:  check times
        pdv.setLong(FORECAST_TIME_UTC, npp.getForecastTimeUtc().getTimeInMillis());
        pdv.setLong(FORECAST_TIME_LOCAL, npp.getForecastTimeLocal());  //TODO:  Needed?
        pdv.setString(FORECAST_TIME_ZONE, npp.getForecastTimeZone());
     
        pdv.setFloat(AVG_MX_TMPF, npp.getAvgMxTmpf());
        pdv.setFloat(HI_MX_TMPF, npp.getHiMxTmpf());
        pdv.setFloat(LO_MX_TMPF, npp.getLoMxTmpf());
        pdv.setFloat(AVG_MN_TMPF, npp.getAvgMnTmpf()); 
        pdv.setFloat(HI_MN_TMPF, npp.getHiMnTmpf());
        pdv.setFloat(LO_MN_TMPF, npp.getLoMnTmpf());
        pdv.setFloat(TMPF, npp.getTmpf());
        pdv.setFloat(DWPF, npp.getDwpf());
        pdv.setFloat(RELH, npp.getRelh());
        pdv.setFloat(WIND_DIR, npp.getWindDir());
        pdv.setString(WIND_S_MPH, npp.getWindSmph());
        pdv.setString(GUST_MPH, npp.getGust_Mph());
        pdv.setFloat(P_WIND_DIR, npp.getPwindDir());
        pdv.setString(WIND_CHAR, npp.getWindChar());
        pdv.setString(SKY_COVER, npp.getSkyCover());
        pdv.setFloat(POP12, npp.getPop12());
        pdv.setFloat(QPF_12_MN, npp.getQpf12Mn());
        pdv.setFloat(QPF_12_MX, npp.getQpf12Mx());
        pdv.setFloat(HIGHEST_MAX_QPF, npp.getHighestMaxQpf());
        pdv.setFloat(LOWEST_MAX_QPF, npp.getLowestMaxQpf());
        pdv.setFloat(SNOW_12_MN, npp.getSnow12Mn());
        pdv.setFloat(SNOW_12_MX, npp.getSnow12Mx());
        pdv.setString(AVG_SKY_COVER, npp.getAvgSkyCover());
        pdv.setString(OBVIS, npp.getObvis());
        pdv.setFloat(WIND_CHILL, npp.getWindChill());
        pdv.setFloat(HEAT_INDEX, npp.getHeatIndex());
        pdv.setFloat(MIN_CHILL, npp.getMinChill());
        pdv.setFloat(MAX_HEAT, npp.getMaxHeat());
        pdv.setString(RAIN, npp.getRain());
        pdv.setString(RAIN_SHWRS, npp.getRainShwrs());
        pdv.setString(SPRINKLES, npp.getSprinkles());
        pdv.setString(TSTMS, npp.getTstms());
        pdv.setString(DRIZZLE, npp.getDrizzle());
        pdv.setString(SNOW, npp.getSnow());  
        pdv.setString(SNOW_SHWRS, npp.getSnowShwrs());
        pdv.setString(FLURRIES, npp.getFlurries());
        pdv.setString(SLEET, npp.getSleet());
        pdv.setString(FRZGRAIN, npp.getFrzgRain());
        pdv.setString(FRZGDRZL, npp.getFrzgDrzl());
        pdv.setString(HAZARDS, npp.getHazards());

        return pdv;
    }

    public static NcPafmRecord toNcPafmRecord(PointDataView pdv) {
    	
    	//TODO:  Needs retrieval testing for hooking up to display resource!
    	
    	NcPafmRecord npr = new NcPafmRecord();

    	NcPafmParameters npp = new NcPafmParameters();

    	////npp.setForecastTimeUtc(pdv.getLong(FORECAST_TIME_UTC).getRefTime().getTime());
    	////npp.setForecastTimeLocal(pdv.getLong(FORECAST_TIME_LOCAL));
    	npp.setForecastTimeZone(pdv.getString(FORECAST_TIME_ZONE));

    	npp.setAvgMxTmpf(pdv.getFloat(AVG_MX_TMPF));
    	npp.setHiMxTmpf(pdv.getFloat(HI_MX_TMPF));
    	npp.setLoMxTmpf(pdv.getFloat(LO_MX_TMPF));
    	npp.setAvgMnTmpf(pdv.getFloat(AVG_MN_TMPF));
    	npp.setHiMnTmpf(pdv.getFloat(HI_MN_TMPF));
    	npp.setLoMnTmpf(pdv.getFloat(LO_MN_TMPF));
    	npp.setTmpf(pdv.getFloat(TMPF));
    	npp.setDwpf(pdv.getFloat(DWPF));
    	npp.setRelh(pdv.getFloat(RELH));
    	npp.setWindDir(pdv.getFloat(WIND_DIR));
    	npp.setWindSmph(pdv.getString(WIND_S_MPH));
    	npp.setGust_Mph(pdv.getString(GUST_MPH));
    	npp.setPwindDir(pdv.getFloat(P_WIND_DIR));
    	npp.setWindChar(pdv.getString(WIND_CHAR));
    	npp.setSkyCover(pdv.getString(SKY_COVER));
    	npp.setPop12(pdv.getFloat(POP12));
    	npp.setQpf12Mn(pdv.getFloat(QPF_12_MN));
    	npp.setQpf12Mx(pdv.getFloat(QPF_12_MX));
    	npp.setHighestMaxQpf(pdv.getFloat(HIGHEST_MAX_QPF));
    	npp.setLowestMaxQpf(pdv.getFloat(LOWEST_MAX_QPF));
    	npp.setSnow12Mn(pdv.getFloat(SNOW_12_MN));
    	npp.setSnow12Mx(pdv.getFloat(SNOW_12_MX));
    	npp.setAvgSkyCover(pdv.getString(AVG_SKY_COVER));
    	npp.setObvis(pdv.getString(OBVIS));
    	npp.setWindChill(pdv.getFloat(WIND_CHILL));
    	npp.setHeatIndex(pdv.getFloat(HEAT_INDEX));
    	npp.setMinChill(pdv.getFloat(MIN_CHILL));
    	npp.setMaxHeat(pdv.getFloat(MAX_HEAT));
    	npp.setRain(pdv.getString(RAIN));
    	npp.setRainShwrs(pdv.getString(RAIN_SHWRS));
    	npp.setSprinkles(pdv.getString(SPRINKLES));
    	npp.setTstms(pdv.getString(TSTMS));
    	npp.setDrizzle(pdv.getString(DRIZZLE));
    	npp.setSnow(pdv.getString(SNOW));  
    	npp.setSnowShwrs(pdv.getString(SNOW_SHWRS));
    	npp.setFlurries(pdv.getString(FLURRIES));
    	npp.setSleet(pdv.getString(SLEET));
    	npp.setFrzgRain(pdv.getString(FRZGRAIN));
    	npp.setFrzgDrzl(pdv.getString(FRZGDRZL));
    	npp.setHazards(pdv.getString(HAZARDS));

    	npr.setPafmParms(npp);

        return npr;
    }

    public static NcPafmRecord[] toNcPafmRecords(PointDataContainer container) {
        List<NcPafmRecord> records = new ArrayList<NcPafmRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);
            records.add(toNcPafmRecord(pdv));
        }
        return records.toArray(new NcPafmRecord[records.size()]);
    }
}
