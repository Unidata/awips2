package gov.noaa.nws.ncep.common.dataplugin.pirep;

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

import gov.noaa.nws.ncep.common.dataplugin.pirep.dao.PirepDao;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.Dimension;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.time.DataTime;

/**
 * Provides a transform from PirepRecords to PointDataContainer and vice versa.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/27/2011				F. J. Yen	Initial creation
 * 08/30/2011    286        qzhou       Added fields for TB, IC, SK. Remove general fields.
 * 08/31/2011    286        qzhou       Created project and moved this from ~edex.plugin.pirep    
 * 09/27/2011    286        qzhou       Make TB, IC, SK records all start from 0 in the arrays. 
 * 										Fixed visibility  .
 * Sep 05, 2013 2316        bsteffen    Unify pirep and ncpirep.
 * Jan 20, 2014             njensen     Fix storage of turbulence top height
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class PirepPointDataTransform {

    private static final String CORRECTION_CODE = "correctionCode";

    private static final String DATAURI = "dataURI";

    private static final String OBS_TEXT = "obsText";
    
    private static final String WMO_HEADER = "wmoHeader";

    private static final String STATION_ID = "stationId";

    private static final String REPORT_TYPE = "reportType";

    private static final String TIME_OBS = "timeObs";
    
    private static final String OBS_ID = "obsId";
    
    private static final String AIRCRAFT_TYPE = "aircraftType";

    private static final String LONGITUDE = "longitude";

    private static final String LATITUDE = "latitude";
    
    private static final String FLIGHT_LEVEL = "flightLevel";
    
    private static final String WIND_SPEED = "windSpeed";

    private static final String WIND_DIR = "windDir";

    private static final String TEMPERATURE = "temperature";

    private static final String HORZ_VISIBILITY = "horzVisibility";

    private static final String WEATHER_GROUP = "weatherGroup";
   
    private static final String NUM_LAYER = "numLayer";
    
    private static final String NCPIREP_LAYER_DATA = "ncPirepLayerData";
    
    private static final String HAZARD_TYPE = "hazardType";

//    private static final String DATA_TYPE = "dataType";
//    private static final String BASE_LAYER_HEIGHT = "baseLayerHeight";
//    private static final String TOP_LAYER_HEIGHT = "topLayerHeight";   
//    private static final String FIRST_VALUE = "firstValue";    
//    private static final String SECOND_VALUE = "secondValue";

    private static final String TURB_INTEN = "turbInten";
    private static final String TURB_TYPE = "turbType";
    private static final String TURB_FREQ = "turbFreq";
    private static final String ICE_INTEN = "iceInten";
    private static final String ICE_TYPE = "iceType";
    private static final String SKY_COVER1 = "skyCover1";
    private static final String SKY_COVER2 = "skyCover2";
    
    private static final String TURB_BASE_HEIGHT = "turbBaseHeight";
    private static final String TURB_TOP_HEIGHT = "turbTopHeight";
    private static final String ICE_BASE_HEIGHT = "iceBaseHeight";
    private static final String ICE_TOP_HEIGHT = "iceTopHeight";
    private static final String SKY_BASE_HEIGHT = "skyBaseHeight";
    private static final String SKY_TOP_HEIGHT = "skyTopHeight";
    private static final String SUSPECT_TIME_FLAG = "suspectTimeFlag";
   

    /**
     * It is important to keep this up to date or risk breaking backwards
     * compatibility
     */
    private static final String[] ALL_PARAMS = { CORRECTION_CODE, DATAURI,
    	 	OBS_TEXT, WMO_HEADER, STATION_ID, REPORT_TYPE, TIME_OBS, OBS_ID,
            AIRCRAFT_TYPE, LONGITUDE, LATITUDE, FLIGHT_LEVEL, TEMPERATURE, 
            WIND_SPEED, WIND_DIR, HORZ_VISIBILITY, WEATHER_GROUP, NUM_LAYER,
            NCPIREP_LAYER_DATA, HAZARD_TYPE, TURB_INTEN, TURB_TYPE, 
            TURB_FREQ, ICE_INTEN, ICE_TYPE, SKY_COVER1, SKY_COVER2, TURB_BASE_HEIGHT,
            TURB_TOP_HEIGHT, ICE_BASE_HEIGHT, ICE_TOP_HEIGHT, SKY_BASE_HEIGHT, SKY_TOP_HEIGHT,
            SUSPECT_TIME_FLAG};

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

    private PirepDao dao;

    private PointDataDescription description;

    public PirepPointDataTransform() {
        try {
            this.dao = new PirepDao("pirep");
            this.description = dao.getPointDataDescription();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public PluginDataObject[] toPointData(PluginDataObject[] pdo) {
    	System.out.println("===============>toPointData");
        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();
            for (PluginDataObject p : pdo) {
                if (!(p instanceof PirepRecord))
                    continue;
                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.description);
                    pointMap.put(f, pdc);
                }
                PirepRecord npr = (PirepRecord) p;
                PointDataView pdv = buildView(pdc, npr);
                npr.setPointDataView(pdv);
            }
        }
        return pdo;
    }

    private PointDataView buildView(PointDataContainer container,
            PirepRecord record) {
        PointDataView pdv = container.append();
        pdv.setString(STATION_ID, record.getStationId());
        Dimension [] dims = description.dimensions;
    	int maxLayer = -1;
    	for(Dimension d : dims) {
    		if ("maxLayer".equals(d.getDimensionName())) {
    			maxLayer = d.getDimensionLength();
    		}
    	}
        if (record.getCorIndicator() != null) {
        	pdv.setString(CORRECTION_CODE, record.getCorIndicator());     	
        }else{
        	pdv.setString(CORRECTION_CODE," ");
        }
        pdv.setFloat(LATITUDE, (float) record.getLatitude());
        pdv.setFloat(LONGITUDE, (float) record.getLongitude());
        pdv.setFloat(FLIGHT_LEVEL, record.getFlightLevel());
        pdv.setLong(TIME_OBS, record.getDataTime().getRefTime().getTime());
        //pdv.setString(DATAURI, record.getDataURI());
        pdv.setString(REPORT_TYPE, "PIREP");  //record.getReportType());
        pdv.setFloat(TEMPERATURE,(float) record.getTemp());
        pdv.setFloat(WIND_DIR, (float) record.getWindDirection());
        pdv.setFloat(WIND_SPEED, (float) record.getWindSpeed());
        pdv.setString(OBS_TEXT, record.getObsText());
        pdv.setInt(HORZ_VISIBILITY, record.getHorzVisibility());
        pdv.setString(WEATHER_GROUP, record.getWeatherGroup());
        pdv.setString(SUSPECT_TIME_FLAG, record.getSuspectTimeFlag());
        
        if (record.getAncPirepData() != null) {
        	Iterator<PirepLayerData> pldIterator = record.getAncPirepData().iterator();
            int i = 0;
            int iTb = -1;
            int iIc = -1;
            int iSk = -1;
            
            while (pldIterator.hasNext()) {
                // TODO: storing duplicate info like this, needs to be resolved
                PirepLayerData pld = pldIterator.next();
                
                if (pld.getLayerType() != null && i < maxLayer) {
                    StringBuffer pldBuffer = new StringBuffer();                 
                    pldBuffer.append(pld.getLayerType());
//                    //may not need in cave
//                    if (pld.getBaseLayerHeight() != null){
//                       pldBuffer.append(" ");
//                       pldBuffer.append(pld.getBaseLayerHeight().toString());
//                    }

                    pdv.setString(NCPIREP_LAYER_DATA, pldBuffer.toString(), i);
                	/*      +++ end of Needed? +++ */
                    if (pld.getLayerType() != null) {
                        pdv.setString(HAZARD_TYPE, pld.getLayerType(), i);
                    }

                    if (pld.getLayerType().equalsIgnoreCase("TURBC"))
                    	iTb ++;
                    else if (pld.getLayerType().equalsIgnoreCase("ICING"))
                		iIc ++;
                    else if (pld.getLayerType().equalsIgnoreCase("CLOUD"))
                		iSk ++;
                    
                    if (pld.getTurbInten() != null) {
                        pdv.setString(TURB_INTEN, pld.getTurbInten(), iTb);
                    }
                    if (pld.getTurbInten() != null) {
                        pdv.setString(TURB_TYPE, pld.getTurbType(), iTb);
                    }
                    if (pld.getTurbInten() != null) {
                        pdv.setString(TURB_FREQ, pld.getTurbFreq(), iTb);
                    }
                    if (pld.getIceInten() != null) {
                        pdv.setString(ICE_INTEN, pld.getIceInten(), iIc);
                    }
                    if (pld.getIceType() != null) {
                        pdv.setString(ICE_TYPE, pld.getIceType(), iIc);
                    }
                    if (pld.getSkyInten1() != null) {
                        pdv.setString(SKY_COVER1, pld.getSkyInten1(), iSk);
                    }
                    if (pld.getSkyInten2() != null) {
                        pdv.setString(SKY_COVER2, pld.getSkyInten2(), iSk);
                    }
                    
                    if (pld.getTurbBaseHeight() != null) {
                        pdv.setInt(TURB_BASE_HEIGHT, pld.getTurbBaseHeight(), iTb);
                    }
                    if (pld.getTurbTopHeight() != null) {
                        pdv.setInt(TURB_TOP_HEIGHT, pld.getTurbTopHeight(), iTb);
                    }
                    if (pld.getIceBaseHeight() != null) {
                        pdv.setInt(ICE_BASE_HEIGHT, pld.getIceBaseHeight(), iIc);
                    }
                    if (pld.getIceTopHeight() != null) {
                        pdv.setInt(ICE_TOP_HEIGHT, pld.getIceTopHeight(), iIc);
                    }
                    if (pld.getSkyBaseHeight() != null) {
                        pdv.setInt(SKY_BASE_HEIGHT, pld.getSkyBaseHeight(), iSk);
                    }
                    if (pld.getSkyTopHeight() != null) {
                        pdv.setInt(SKY_TOP_HEIGHT, pld.getSkyTopHeight(), iSk);
                    }
                }
                i++;
            }
            pdv.setInt("numLayer", i);
        }

        return pdv;
    }

    public static PirepRecord toPirepRecord(PointDataView pdv) {
        PirepRecord npr = new PirepRecord();
        npr.setObsId(pdv.getInt(OBS_ID));

        npr.setCorIndicator(pdv.getString(CORRECTION_CODE));
        npr.setDataTime(new DataTime(new Date(pdv.getNumber(TIME_OBS)
                .longValue())));

        AircraftObsLocation loc = new AircraftObsLocation(
                pdv.getString(STATION_ID));
        Double lat = pdv.getNumber(LATITUDE).doubleValue();
        Double lon = pdv.getNumber(LONGITUDE).doubleValue();
        loc.setLocation(lat, lon);
        loc.setFlightLevel(pdv.getNumber(FLIGHT_LEVEL).intValue());
        npr.setLocation(loc);
        npr.setDataURI(pdv.getString(DATAURI));
        npr.setObsText(pdv.getString(OBS_TEXT));
        npr.setReportType("PIREP"); //pdv.getString(REPORT_TYPE));

        npr.setTemp(pdv.getNumber(TEMPERATURE).floatValue());

        npr.setHorzVisibility(pdv.getNumber(HORZ_VISIBILITY).intValue());
        npr.setWeatherGroup(pdv.getString(WEATHER_GROUP));

        npr.setWindDirection(pdv.getNumber(WIND_DIR).floatValue());
        npr.setWindSpeed(pdv.getNumber(WIND_SPEED).floatValue());
        Number numLayer = pdv.getNumber("numLayer");
//        Number[] levels = pdv.getNumberAllLevels(BASE_LAYER_HEIGHT);
        String[] pldLayerType = pdv.getStringAllLevels(HAZARD_TYPE);
        String[] pldTurbInten = pdv.getStringAllLevels(TURB_INTEN);
        String[] pldIceInten = pdv.getStringAllLevels(ICE_INTEN);
        String[] pldSkyInten1 = pdv.getStringAllLevels(SKY_COVER1);
        String[] pldSkyInten2 = pdv.getStringAllLevels(SKY_COVER2);
        int[] pldTurbBaseHeight = pdv.getIntAllLevels(TURB_BASE_HEIGHT);
        int[] pldTurbTopHeight = pdv.getIntAllLevels(TURB_TOP_HEIGHT);
        int[] pldIceBaseHeight = pdv.getIntAllLevels(ICE_BASE_HEIGHT);
        int[] pldIceTopHeight = pdv.getIntAllLevels(ICE_TOP_HEIGHT);
        int[] pldSkyBaseHeight = pdv.getIntAllLevels(SKY_BASE_HEIGHT);
        int[] pldSkyTopHeight = pdv.getIntAllLevels(SKY_BASE_HEIGHT);
        
        Set<PirepLayerData> pldList = new HashSet<PirepLayerData>();
        if (numLayer !=null && numLayer.intValue() > 0) {
        	for (int i = 0; i < numLayer.intValue(); i ++) {
            	PirepLayerData pirepLayerData = new PirepLayerData();
				pirepLayerData.setLayerType(pldLayerType[i]);            	
            	pirepLayerData.setTurbInten(pldTurbInten[i]);
            	pirepLayerData.setIceInten(pldIceInten[i]);
            	pirepLayerData.setSkyInten1(pldSkyInten1[i]);
            	pirepLayerData.setSkyInten2(pldSkyInten2[i]);
            	pirepLayerData.setTurbBaseHeight(pldTurbBaseHeight[i]);
            	pirepLayerData.setTurbTopHeight(pldTurbTopHeight[i]);
            	pirepLayerData.setIceBaseHeight(pldIceBaseHeight[i]);
            	pirepLayerData.setIceTopHeight(pldIceTopHeight[i]);
            	pirepLayerData.setSkyBaseHeight(pldSkyBaseHeight[i]);
            	pirepLayerData.setSkyTopHeight(pldSkyTopHeight[i]);
//                //may not need in cave
//                if ((levels[i].intValue()) != PointDataDescription.FILL_VALUE_INT) {
                // PirepLayerData.setBaseLayerHeight(levels[i].intValue());
//                }

                pldList.add(pirepLayerData);
            
        	}
    	}
        npr.setAncPirepData(pldList);

        return npr;
    }

    public static PirepRecord[] toPirepRecords(PointDataContainer container) {
        List<PirepRecord> records = new ArrayList<PirepRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
            PointDataView pdv = container.readRandom(i);
            records.add(toPirepRecord(pdv));
        }
        return records.toArray(new PirepRecord[records.size()]);
    }
}
