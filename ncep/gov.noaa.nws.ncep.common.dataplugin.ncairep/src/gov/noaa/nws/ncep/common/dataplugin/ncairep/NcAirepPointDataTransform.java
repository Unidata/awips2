package gov.noaa.nws.ncep.common.dataplugin.ncairep;

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

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import gov.noaa.nws.ncep.common.dataplugin.ncairep.dao.NcAirepDao;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.time.DataTime;

/**
 * Provides a transform from NcAirepRecords to PointDataContainer and vice versa.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/27/2011				F. J. Yen	Initial creation from MetarPointDataTransform
 * 06/28/2011				F. J. Yen	Updated for OB11.5
 * 09/19/2011    286       Q.Zhou      Modified populateRecord to add 8 new fields for TB, IC and SK.
 * 10/18/2011    286       Q.Zhou      Fixed datarui in db
 * </pre>
 * 
 * @author f j yen
 * @version 1.0
 */

public class NcAirepPointDataTransform {

    private static final String WIND_SPEED = "windSpeed";

    private static final String WIND_DIR = "windDir";

    // WIND_SPEED, WIND_DIR
    // ------------------
    private static final String TEMPERATURE = "temperature";

    // TEMPERATURE
    // ------------------
    private static final String FLIGHT_HAZARD = "flightHazard";

    private static final String FLIGHT_WEATHER = "flightWeather";
    
    private static final String FLIGHT_CONDITIONS = "flightConditions";
    
    // FLIGHT_HAZARD, FLIGHT_WEATHER, FLIGHT_CONDITIONS
    // ------------------
    private static final String WMO_HEADER = "wmoHeader";

    private static final String STATION_ID = "stationId";

    private static final String REPORT_TYPE = "reportType";

    private static final String TIME_OBS = "timeObs";
    
    private static final String OBS_ID = "obsId";

    // WMO_HEADER, STATION_ID, REPORT_TYPE, TIME_OBS, OBS_ID
    // ------------------
    private static final String LONGITUDE = "longitude";

    private static final String LATITUDE = "latitude";
    
    private static final String FLIGHT_LEVEL = "flightLevel";
    
    // LONGITUDE, LATITUDE, FLIGHT_LEVEL
    // ------------------
    private static final String CORRECTION_CODE = "correctionCode";

    private static final String DATAURI = "dataURI";

    private static final String TURB_INTEN = "turbInten";
    private static final String TURB_TYPE = "turbType";
    private static final String TURB_FREQ = "turbFreq";
    private static final String ICE_INTEN = "iceInten";
    private static final String ICE_TYPE = "iceType";
    private static final String SKY_COVER = "skyCover";
    private static final String SKY_BASE_HEIGHT = "skyBaseHeight";
    private static final String SKY_TOP_HEIGHT = "skyTopHeight";
    private static final String SUSPECT_TIME_FLAG = "suspectTimeFlag";
    // CORRECTION_CODE, DATAURI,
    // ------------------

    /**
     * It is important to keep this up to date or risk breaking backwards
     * compatibility
     */
    private static final String[] ALL_PARAMS = { DATAURI, TIME_OBS, OBS_ID, REPORT_TYPE, 
            STATION_ID, WMO_HEADER, FLIGHT_HAZARD, FLIGHT_WEATHER,
            FLIGHT_CONDITIONS, LONGITUDE, LATITUDE, FLIGHT_LEVEL,
            CORRECTION_CODE, TEMPERATURE, WIND_SPEED, WIND_DIR,
            TURB_INTEN, TURB_TYPE, TURB_FREQ, ICE_INTEN, ICE_TYPE, SKY_COVER,
            SKY_BASE_HEIGHT, SKY_TOP_HEIGHT, SUSPECT_TIME_FLAG};

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

    private NcAirepDao dao;

    private PointDataDescription description;

    public NcAirepPointDataTransform() {
        try {
            this.dao = new NcAirepDao("ncairep");
            this.description = dao.getPointDataDescription(null);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public PluginDataObject[] toPointData(PluginDataObject[] pdo) {
    	
        long t0 =System.currentTimeMillis();
        System.out.println("===============>toPointData"); // in NcAirepPointDataTransform t0=" + t0);
        /* 999 */
        
        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();
            
            for (PluginDataObject p : pdo) {
            	
                if (!(p instanceof NcAirepRecord))
                    continue;
                
                File f = this.dao.getFullFilePath(p);
                
                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.description);
                    pointMap.put(f, pdc);
                    
                }
                
                NcAirepRecord nar = (NcAirepRecord) p;              
                PointDataView pdv = buildView(pdc, nar);              
                nar.setPointDataView(pdv);
                 
            }
        }
       
        long t1 =System.currentTimeMillis();
        
        return pdo;
    }

    private PointDataView buildView(PointDataContainer container,
            NcAirepRecord record) {
        PointDataView pdv = container.append();
//        pdv.setString(STATION_ID, record.getStationId());
        if (record.getCorIndicator() != null) {
        	pdv.setString(CORRECTION_CODE, record.getCorIndicator());     	
        }else{
        	pdv.setString(CORRECTION_CODE," ");
        }
       
//        pdv.setFloat(LATITUDE, (float) record.getLatitude());
//        pdv.setFloat(LONGITUDE, (float) record.getLongitude());
        pdv.setFloat(FLIGHT_LEVEL, record.getFlightLevel());
        pdv.setLong(TIME_OBS, record.getDataTime().getRefTime().getTime());
//        pdv.setString(DATAURI, record.getDataURI());
//        pdv.setString(REPORT_TYPE, record.getReportType());
        pdv.setFloat(TEMPERATURE,(float) record.getTemp());
        pdv.setFloat(WIND_DIR, (float) record.getWindDirection());
        pdv.setFloat(WIND_SPEED, (float) record.getWindSpeed());
        
        pdv.setString(TURB_INTEN, record.getTurbInten());
        pdv.setString(TURB_TYPE, record.getTurbType());
        pdv.setString(TURB_FREQ, record.getTurbFreq());
        pdv.setString(ICE_INTEN, record.getIceInten());
        pdv.setString(ICE_TYPE, record.getIceType());
        pdv.setString(SKY_COVER, record.getSkyCover());
        pdv.setInt(SKY_BASE_HEIGHT, record.getSkyBaseHeight());
        pdv.setInt(SKY_TOP_HEIGHT, record.getSkyTopHeight());
        pdv.setString(SUSPECT_TIME_FLAG, record.getSuspectTimeFlag());
        
        return pdv;
    }

    public static NcAirepRecord toNcAirepRecord(PointDataView pdv) {
        NcAirepRecord nar = new NcAirepRecord();
        nar.setObsId(pdv.getInt(OBS_ID));

        nar.setCorIndicator(pdv.getString(CORRECTION_CODE));
        nar.setDataTime(new DataTime(new Date(pdv.getNumber(TIME_OBS)
                .longValue())));

        AircraftObsLocation loc = new AircraftObsLocation(
                pdv.getString(STATION_ID));
        Double lat = pdv.getNumber(LATITUDE).doubleValue();
        Double lon = pdv.getNumber(LONGITUDE).doubleValue();
        loc.setLocation(lat, lon);
        loc.setFlightLevel(pdv.getNumber(FLIGHT_LEVEL).intValue());
        nar.setLocation(loc);
        nar.setDataURI(pdv.getString(DATAURI));
        nar.setReportType(pdv.getString(REPORT_TYPE));

        nar.setTemp(pdv.getNumber(TEMPERATURE).floatValue());

        nar.setPluginName("ncairep");
        nar.setFlightHazard(pdv.getInt(FLIGHT_HAZARD));
        nar.setFlightWeather(pdv.getInt(FLIGHT_WEATHER));
        nar.setFlightConditions(pdv.getInt(FLIGHT_CONDITIONS));

        // AIREP Wind data
        nar.setWindDirection(pdv.getNumber(WIND_DIR).floatValue());
        nar.setWindSpeed(pdv.getNumber(WIND_SPEED).floatValue());
        //nar.setSuspectTimeFlag(pdv.getString(SUSPECT_TIME_FLAG));
        
        return nar;
    }

    public static NcAirepRecord[] toNcAirepRecords(PointDataContainer container) {
        List<NcAirepRecord> records = new ArrayList<NcAirepRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);
            records.add(toNcAirepRecord(pdv));
        }
        return records.toArray(new NcAirepRecord[records.size()]);
    }
}

