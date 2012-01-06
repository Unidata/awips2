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
package com.raytheon.uf.edex.plugin.mesowest.decoder;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.mesowest.common.MESOWestRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class MESOWestParser {

    private Log logger = LogFactory.getLog(getClass());

    private Map<Integer, String> paramMap = null;

    private Map<String, MESOWestElement> valuesMap = null;

    private SimpleDateFormat dateFormat = new SimpleDateFormat(
            MESOWestConstants.D_DATEFMT);

    /**
     * 
     * @param parameters
     */
    public MESOWestParser(String parameters) {

        paramMap = MESOWestConstants.getParmMap();
        valuesMap = MESOWestConstants.getValuesMap();
        parameters = parameters.trim();
        if (parameters.startsWith(MESOWestConstants.D_PARMLEADER)) {

            parameters = parameters.substring(
                    MESOWestConstants.D_PARMLEADER.length()).trim();

            String[] parms = parameters.split(MESOWestConstants.D_PARM_DELIMIT,
                    -1);

            int index = MESOWestConstants.getParmPos();
            for (String s : parms) {
                paramMap.put(index, s);
                valuesMap.put(s, new MESOWestElement(s));

                index++;
            }
        }
    }

    /**
     * 
     * @param report
     * @return
     */
    public MESOWestRecord decode(String report) {
        // Clear out the old data!
        clearValueMap();
        String[] parts = report.split(",", -1);

        if ((parts != null) && (parts.length > 5)) {
            int index = 0;
            for (String s : parts) {
                String elementName = paramMap.get(index++);

                MESOWestElement element = valuesMap.get(elementName);
                if (element != null) {
                    element.setElementValue(s);
                }
            }
        }

        return getDecodedData();
    }

    /**
     * 
     * @return
     */
    private MESOWestRecord getDecodedData() {

        MESOWestRecord record = getLocationInfo(new MESOWestRecord());
        if (record != null) {
           // Now go get specific data elements
            Double val = getFloatValue(MESOWestConstants.P_SFCTEMP);
            val = DecoderTools.fahrenheitToCelsius(val);
            record.setTemp(DecoderTools.celsiusToKelvin(val));
            
            val = getFloatValue(MESOWestConstants.P_SFCDWPT);
            val = DecoderTools.fahrenheitToCelsius(val);
            record.setDwpt(DecoderTools.celsiusToKelvin(val));

            val = getFloatValue(MESOWestConstants.P_MAX_T_24H);
            val = DecoderTools.fahrenheitToCelsius(val);
            record.setMaxT24(DecoderTools.celsiusToKelvin(val));
            
            val = getFloatValue(MESOWestConstants.P_MIN_T_24H);
            val = DecoderTools.fahrenheitToCelsius(val);
            record.setMinT24(DecoderTools.celsiusToKelvin(val));
            
            record.setHumidity(getFloatValue(MESOWestConstants.P_HUMIDITY));

            val = getFloatValue(MESOWestConstants.P_WINDSPD);
            record.setWindSpeed(DecoderTools.ktsToMSec(val));

            val = getFloatValue(MESOWestConstants.P_WINDGST);
            record.setWindGust(DecoderTools.ktsToMSec(val));

            record.setWindDirection(getFloatValue(MESOWestConstants.P_WINDDIR));

            val = getFloatValue(MESOWestConstants.P_PRESSURE);
            record.setPressure(DecoderTools.hPaToPa(val));

            val = getFloatValue(MESOWestConstants.P_ALTIMETER);
            record.setAltimeter(DecoderTools.inToPa(val));

            val = getFloatValue(MESOWestConstants.P_SEA_LVL_PRES);
            record.setSeaLevelPressure(DecoderTools.hPaToPa(val));

            
            record.setPrecip_01M(getFloatValue(MESOWestConstants.P_PRECIP_01M));
            record.setPrecip_05M(getFloatValue(MESOWestConstants.P_PRECIP_05M));
            record.setPrecip_10M(getFloatValue(MESOWestConstants.P_PRECIP_10M));
            record.setPrecip_15M(getFloatValue(MESOWestConstants.P_PRECIP_15M));
            record.setPrecip_30M(getFloatValue(MESOWestConstants.P_PRECIP_30M));
            record.setPrecip_01H(getFloatValue(MESOWestConstants.P_PRECIP_01H));
            record.setPrecip_03H(getFloatValue(MESOWestConstants.P_PRECIP_03H));
            record.setPrecip_06H(getFloatValue(MESOWestConstants.P_PRECIP_06H));
            record.setPrecip_24H(getFloatValue(MESOWestConstants.P_PRECIP_24H));
        }
        return record;
    }

    /**
     * Clear the parsed values from each element prior to accepting new data.
     */
    private void clearValueMap() {
        for (MESOWestElement element : valuesMap.values()) {
            element.setElementValue(null);
        }
    }

    /**
     * 
     * @param data
     * @return
     */
    private MESOWestRecord getLocationInfo(MESOWestRecord data) {

        Calendar obsTime = null;

        MESOWestElement m = valuesMap.get(MESOWestConstants.P_DATATIME);
        if (m != null) {
            String s = m.getElementValue();
            if (s != null) {
                try {
                    Date d = dateFormat.parse(s);
                    if (d != null) {
                        Calendar c = TimeTools.newCalendar(d.getTime());
                        if (c != null) {
                            obsTime = TimeTools.copy(c);
                            data.setTimeObs(obsTime);

                            DataTime t = new DataTime(TimeTools.copy(obsTime));
                            data.setDataTime(t);
                        }
                    }
                } catch (ParseException e) {
                    logger.error("Invalid date [" + s + "]");
                }
            }
        } else {
            logger.error("Date missing");
            data = null;
        }

        if (obsTime != null) {

            Double lat = getFloatValue(MESOWestConstants.P_LAT);
            Double lon = getFloatValue(MESOWestConstants.P_LON);
            Integer elev = getIntValue(MESOWestConstants.P_ELEV);

            if ((lat != null) && (lon != null) && (elev != null)) {
                SurfaceObsLocation location = new SurfaceObsLocation();
                location.assignLocation(lat, lon);
                location.setElevation(elev);

                String stnId = getStringValue(MESOWestConstants.P_STNID);
                if (stnId != null) {
                    location.setStationId(stnId);

                    data.setSpatialObject(location);
                    data.setNetworkType(getStringValue(MESOWestConstants.P_STNTYPE));
                } else {
                    data = null;
                    logger.error("Station Id missing");
                }
            } else {
                data = null;
                logger.error("Spatial information missing");
            }
        }

        return data;
    }

    /**
     * 
     * @param value
     * @return
     */
    private String getStringValue(String key) {
        String sValue = null;

        MESOWestElement value = valuesMap.get(key);
        if (value != null) {
            sValue = value.getElementValue();
        }
        return sValue;
    }

    /**
     * 
     * @param value
     * @return
     */
    private Double getFloatValue(String key) {
        Double floatValue = null;

        MESOWestElement value = valuesMap.get(key);
        if (value != null) {
            try {
                floatValue = Double.parseDouble(value.getElementValue());
            } catch (NumberFormatException nfe) {

            }
        }
        return floatValue;
    }

    /**
     * 
     * @param value
     * @return
     */
    private Integer getIntValue(String key) {
        Integer intValue = null;

        MESOWestElement value = valuesMap.get(key);
        if (value != null) {
            try {
                intValue = Integer.parseInt(value.getElementValue());
            } catch (NumberFormatException nfe) {

            }
        }
        return intValue;
    }

    public static final void main(String[] args) {

        String parms = " PARM =  TMPF;RELH;SKNT;GUST;DRCT;QFLG;DWPF;PRES;PMSL;ALTI;P03D;SOLR;WNUM;VSBY;CHC1;CHC2;CHC3;CIG;TLKE;FT;FM;HI6;LO6;PEAK;HI24;LO24;PREC;P01I;P03I;P06I;P24I;P05I;P10I;P15I;SNOW;PACM;SACM;WEQS;P30I;PWVP;TSOI;MSOI;STEN;TSRD;EVAP;TRD1;TRD2;TRD3;TRD4;TFZ1;TFZ2;TFZ3;TFZ4;RSS1;RSS2;RSS3;RSS4";

        String data1 = "KAAT,41.49139,-120.56444,4373,1,20090303/2009,32,92.96,18,27,230,2,30.2,,,29.81,,,740,0.5,36,73,194,700,,,,,,27,,,,0.01,,,,,,,,,,,,,,,,,,,,,,,,,,,,,";

        String data2 = "KRSP,,,,1,20090312/1254,28.94,53.05,7,19,340,,14,,1031.2,30.39,,,,10,1,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,";
        MESOWestParser parser = new MESOWestParser(parms);

        MESOWestRecord record = parser.decode(data2);

        System.out.println("******************************");
        if (record != null) {
            Calendar c = record.getTimeObs();
            if (c != null) {
                System.out.println(String.format(
                        "* time       | %1$tY%1$tm%1$td%1$tH%1$tM", record
                                .getTimeObs()));
            } else {
                System.out.println("* time       | null");
            }
            System.out.println("* latitude   | " + record.getLatitude());
            System.out.println("* longitude  | " + record.getLongitude());
            System.out.println("* station id | " + record.getStationId());
            System.out.println("* network    | " + record.getNetworkType());

            System.out.println("*************");
            System.out.println("* temperature    = " + record.getTemp());
            System.out.println("* dewpoint       = " + record.getDwpt());
            System.out.println("* humidity %     = " + record.getHumidity());
            System.out.println("* wind speed     = " + record.getWindSpeed());
            System.out.println("* wind gust      = " + record.getWindGust());
            System.out.println("* wind direction = "
                    + record.getWindDirection());
            System.out.println("* pressure       = " + record.getPressure());
            System.out.println("* slp            = "
                    + record.getSeaLevelPressure());
            System.out.println("* altimeter      = " + record.getAltimeter());
        } else {
            System.out.println("* No data decoded");
            System.out.println("*************");
            System.out.println("******************************");
        }
    }

}
