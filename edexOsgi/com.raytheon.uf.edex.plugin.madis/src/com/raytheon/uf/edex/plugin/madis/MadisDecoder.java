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

package com.raytheon.uf.edex.plugin.madis;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Pattern;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord.QCD;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Decoder implementation for Madis data type.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * Mar 27, 2013 1746        dhladky     Initial creation
 * Jun 17, 2013 2113        dhladky     QPID memory usage alleviation
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Dec 10, 2013 2616        mpduff      Set overwrite allowed on MadisRecord.
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class MadisDecoder extends AbstractDecoder {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MadisDecoder.class);

    public static final String[] typeDHeader = new String[] { "STAID",
            "OBDATE", "OBTIME", "PVDR", "SUBPVDR", "DS", "RES", "TD", "D",
            "RH", "QCD", "ALTSE", "D", "T", "QCD", "DD", "D", "FF", "QCD",
            "ELEV", "D", "LAT", "QCD", "LON", "D", "PCPRATE", "D", "FFGUST",
            "D", "PWV", "D", "P", "D" };

    public static final String[] typeFHeader = new String[] { "STAID",
            "OBDATE", "OBTIME", "PVDR", "SUBPVDR", "DS", "RES", "TD", "D",
            "QCA", "QCR", "RH", "D", "QCA", "QCR", "ALTSE", "D", "QCA", "QCR",
            "T", "D", "QCA", "QCR", "DD", "D", "QCA", "QCR", "FF", "D", "QCA",
            "QCR", "ELEV", "D", "QCA", "QCR", "LAT", "D", "QCA", "QCR", "LON",
            "QCA", "QCR", "D", "PCPRATE", "D", "QCA", "QCR", "FFGUST", "D",
            "QCA", "QCR", "PWV", "D", "QCA", "QCR", "P", "D", "QCA", "QCR" };

    private static final Pattern regex = Pattern.compile(",");

    private static final String BLANK = "";

    private static final float MISSING_FLOAT = -99999;

    private static final String STRING_NULL = "null";

    private static final int MISSING_INT = -99999;

    private static final Pattern QUOTES_PATTERN = Pattern.compile("\"");

    private final String PLUGIN_NAME;

    private static ThreadLocal<SimpleDateFormat> madisDateFormat = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat(MADIS_DATE_FORMAT);
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sdf;
        }

    };

    private static final String MADIS_DATE_FORMAT = "MM/dd/yyyy HH:mm";

    /**
     * Required constructor.
     */
    public MadisDecoder(String pluginName) {
        PLUGIN_NAME = pluginName;
    }

    /**
     * Decoder for MADIS
     * 
     * @param inputData
     * @return
     * @throws DecoderException
     */
    public PluginDataObject[] decode(String path) throws DecoderException {

        // de-serialize the object from the path
        long time = System.currentTimeMillis();
        MadisIngestObject mio = MadisSeparator.getObject(path);
        PluginDataObject[] retVal = new PluginDataObject[0];

        if (mio != null) {

            statusHandler
                    .handle(Priority.INFO, "Starting MADIS record decode.");
            List<PluginDataObject> retList = new ArrayList<PluginDataObject>();
            String[] headerType = mio.getHeader();
            int i = 0;
            try {
                for (String line : mio.getLines()) {

                    long time3 = System.currentTimeMillis();
                    MadisRecord rec = processMadis(headerType, line);
                    rec.setOverwriteAllowed(true);
                    long time4 = System.currentTimeMillis();
                    statusHandler.handle(Priority.DEBUG,
                            "MADIS record decode time: " + (time4 - time3)
                                    + " ms");

                    if (rec != null) {
                        retList.add(rec);
                    }

                    i++;
                }
                statusHandler.handle(Priority.INFO, "Processed " + i
                        + " rows. " + mio.getLines().size()
                        + " records to insert");
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Could not open MADIS Ingest Object!", e);
            } finally {
                // discard file
                File file = new File(path);
                if (file.exists()) {
                    file.delete();
                }
            }

            if (!retList.isEmpty()) {
                int size = retList.size();
                retVal = retList.toArray(new PluginDataObject[size]);
            }
        }

        if (retVal.length == 0) {
            statusHandler.handle(Priority.WARN,
                    "No MADIS data decoded!!!!  Bad file format!");
        } else {

            long time2 = System.currentTimeMillis();
            statusHandler.handle(Priority.INFO, "MADIS Decode time: "
                    + (time2 - time) + " ms");
        }

        return retVal;
    }

    /**
     * Process the CSV line into a MadisRecord
     * 
     * @param line
     * @return
     */
    private MadisRecord processMadis(String[] headerType, String line) {

        MadisRecord rec = null;

        if (!line.isEmpty() && (headerType != null)) {

            if (headerType.length == typeDHeader.length) {
                rec = processTypeD(line, headerType);
            } else if (headerType.length == typeFHeader.length) {
                rec = processTypeF(line, headerType);
            } else {
                throw new UnsupportedOperationException(
                        "Unknown Header format for MADIS file! " + headerType);
            }
        }

        return rec;

    }

    /**
     * Creates a MadisRecord from a type D file
     * 
     * @param line
     * @param String
     *            []
     * @return
     */
    private MadisRecord processTypeD(String line, String[] headerType) {

        MadisRecord rec = null;
        String stationId = STRING_NULL; // 0
        String obsDate = null; // 1
        String obsTime = null; // 2
        String provider = STRING_NULL; // 3
        String sub_provider = STRING_NULL; // 4
        int dataset = MISSING_INT; // 5
        int restriction = MISSING_INT; // 6
        float td = MISSING_FLOAT; // 7
        QCD td_qcd = QCD.MISSING; // 8
        float rh = MISSING_FLOAT; // 9
        QCD rh_qcd = QCD.MISSING; // 10
        float altimeter = MISSING_FLOAT; // 11
        QCD altimeter_qcd = QCD.MISSING; // 12
        float temperature = MISSING_FLOAT; // 13
        QCD temperature_qcd = QCD.MISSING; // 14
        int windDirection = MISSING_INT; // 15
        QCD windDirection_qcd = QCD.MISSING; // 16
        float windSpeed = MISSING_FLOAT; // 17
        QCD windSpeed_qcd = QCD.MISSING; // 18
        int elevation = MISSING_INT; // 19
        QCD elevation_qcd = QCD.MISSING; // 20
        float latitude = MISSING_FLOAT; // 21
        QCD latitude_qcd = QCD.MISSING; // 22
        float longitude = MISSING_FLOAT; // 23
        QCD longitude_qcd = QCD.MISSING; // 24
        float precipRate = MISSING_FLOAT; // 25
        QCD precipRate_qcd = QCD.MISSING; // 26
        float windGust = MISSING_FLOAT; // 27
        QCD windGust_qcd = QCD.MISSING; // 28
        float precipitalWater = MISSING_FLOAT;// 29
        QCD precipitalWater_qcd = QCD.MISSING;// 30
        float pressure = MISSING_FLOAT;// 31
        QCD pressure_qcd = QCD.MISSING;// 32

        String[] commaSepList = null;

        try {

            commaSepList = regex.split(line);

            if (commaSepList.length == typeDHeader.length) {

                rec = new MadisRecord();
                int i = 0;
                // get STAID
                stationId = trimQuotes(commaSepList[i++]).trim();

                // get OBSDATE
                obsDate = trimQuotes(commaSepList[i++]).trim();

                // get OBSTIME
                obsTime = trimQuotes(commaSepList[i++]).trim();

                // if these don't work, no use in going forward
                rec = setTimeObs(obsDate, obsTime, rec, headerType);
                // get PVDR
                provider = trimQuotes(commaSepList[i++]).trim();
                rec.setProvider(provider);

                // get SUBPVDR
                sub_provider = trimQuotes(commaSepList[i++]).trim();
                if (sub_provider.equals(BLANK)) {
                    rec.setSubProvider(STRING_NULL);
                } else {
                    rec.setSubProvider(sub_provider);
                }
                // the Dataset value
                dataset = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setDataset(dataset);
                // the Restriction value
                restriction = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setRestriction(restriction);
                // get TD
                td = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setDewpoint(td);
                // get TD QCD
                td_qcd = QCD.fromString(trimQuotes(commaSepList[i++]).trim());
                rec.setDewpoint_qcd(td_qcd);
                // get RH
                rh = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setRh(rh);
                // get RH QCD
                rh_qcd = QCD.fromString(trimQuotes(commaSepList[i++]).trim());
                rec.setRh_qcd(rh_qcd);
                // altimeter
                altimeter = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setAltimeter(altimeter);
                // get Altimeter QCD
                altimeter_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setAltimeter_qcd(altimeter_qcd);
                // get Temperature
                temperature = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setTemperature(temperature);
                // get Temperature QCD
                temperature_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setTemperature_qcd(temperature_qcd);
                // get Wind Direction
                windDirection = new Float(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setWindDirection(windDirection);
                // get Wind Direction QCD
                windDirection_qcd = QCD
                        .fromString(trimQuotes(commaSepList[i++]).trim());
                rec.setWindDirection_qcd(windDirection_qcd);
                // wind speed
                windSpeed = new Float(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setWindSpeed(windSpeed);
                // get Wind Speed QCD
                windSpeed_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setWindSpeed_qcd(windSpeed_qcd);
                // get Elevation
                elevation = new Float(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                // get Elevation QCD
                elevation_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setElevation_qcd(elevation_qcd);
                // get Latitude
                latitude = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                // get Latitude QCD
                latitude_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setLatitude_qcd(latitude_qcd);
                // get Longitude
                longitude = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                // get Longitude QCD
                longitude_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setLongitude_qcd(longitude_qcd);
                // get precipRate
                precipRate = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setPrecipRate(precipRate);
                // get precipRate QCD
                precipRate_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setPrecipRate_qcd(precipRate_qcd);
                // get Wind Gust
                windGust = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setWindGust(windGust);
                // get Wind Gust QCD
                windGust_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setWindGust_qcd(windGust_qcd);
                // get precipitalWater
                precipitalWater = new Float(trimQuotes(commaSepList[i++])
                        .trim()).floatValue();
                rec.setPrecipitalWater(precipitalWater);
                // get precipitalWater QCD
                precipitalWater_qcd = QCD.fromString(trimQuotes(
                        commaSepList[i++]).trim());
                rec.setPrecipitalWater_qcd(precipitalWater_qcd);
                // get pressure
                pressure = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setPressure(pressure);
                // get pressure QCD
                pressure_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setPressure_qcd(pressure_qcd);

                // Take care of creating the station
                rec = setObsLocation(stationId, latitude, longitude, elevation,
                        rec);
                rec = setRecordTime(rec);
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Couldn't parse D file row. " + stationId + e);
        }

        return rec;
    }

    /**
     * Creates a MadisRecord from a type F file
     * 
     * @param line
     * @param headerType
     * @return
     */
    private MadisRecord processTypeF(String line, String[] headerType) {

        MadisRecord rec = null;
        String stationId = STRING_NULL; // 0
        String obsDate = null; // 1
        String obsTime = null; // 2
        String provider = STRING_NULL; // 3
        String sub_provider = STRING_NULL; // 4
        int dataset = MISSING_INT; // 5
        int restriction = MISSING_INT; // 6
        float td = MISSING_FLOAT; // 7
        QCD td_qcd = QCD.MISSING; // 8
        int td_qca = MISSING_INT; // 9
        int td_qcr = MISSING_INT; // 10
        float rh = MISSING_FLOAT; // 11
        QCD rh_qcd = QCD.MISSING; // 12
        int rh_qca = MISSING_INT; // 13
        int rh_qcr = MISSING_INT; // 14
        float altimeter = MISSING_FLOAT; // 15
        QCD altimeter_qcd = QCD.MISSING; // 16
        int altimeter_qca = MISSING_INT; // 17
        int altimeter_qcr = MISSING_INT; // 18
        float temperature = MISSING_FLOAT; // 19
        QCD temperature_qcd = QCD.MISSING; // 20
        int temperature_qca = MISSING_INT; // 21
        int temperature_qcr = MISSING_INT; // 22
        int windDirection = MISSING_INT; // 23
        QCD windDirection_qcd = QCD.MISSING; // 24
        int windDirection_qca = MISSING_INT; // 25
        int windDirection_qcr = MISSING_INT; // 26
        float windSpeed = MISSING_FLOAT; // 27
        QCD windSpeed_qcd = QCD.MISSING; // 28
        int windSpeed_qca = MISSING_INT; // 29
        int windSpeed_qcr = MISSING_INT; // 30
        int elevation = MISSING_INT; // 31
        QCD elevation_qcd = QCD.MISSING; // 32
        int elevation_qca = MISSING_INT; // 33
        int elevation_qcr = MISSING_INT; // 34
        float latitude = MISSING_FLOAT; // 35
        QCD latitude_qcd = QCD.MISSING; // 36
        int latitude_qca = MISSING_INT; // 37
        int latitude_qcr = MISSING_INT; // 38
        float longitude = MISSING_FLOAT; // 39
        QCD longitude_qcd = QCD.MISSING; // 40
        int longitude_qca = MISSING_INT; // 41
        int longitude_qcr = MISSING_INT; // 42
        float precipRate = MISSING_FLOAT; // 43
        QCD precipRate_qcd = QCD.MISSING; // 44
        int precipRate_qca = MISSING_INT; // 45
        int precipRate_qcr = MISSING_INT; // 46
        float windGust = MISSING_FLOAT; // 47
        QCD windGust_qcd = QCD.MISSING; // 48
        int windGust_qca = MISSING_INT; // 49
        int windGust_qcr = MISSING_INT; // 50
        float precipitalWater = MISSING_FLOAT;// 51
        QCD precipitalWater_qcd = QCD.MISSING;// 52
        int precipitalWater_qca = MISSING_INT; // 53
        int precipitalWater_qcr = MISSING_INT; // 54
        float pressure = MISSING_FLOAT;// 55
        QCD pressure_qcd = QCD.MISSING;// 56
        int pressure_qca = MISSING_INT; // 57
        int pressure_qcr = MISSING_INT; // 58

        String[] commaSepList = null;

        try {

            commaSepList = regex.split(line);

            if (commaSepList.length == typeFHeader.length) {

                rec = new MadisRecord();
                int i = 0;
                // get STAID
                stationId = trimQuotes(commaSepList[i++]).trim();

                // get OBSDATE
                obsDate = trimQuotes(commaSepList[i++]).trim();

                // get OBSTIME
                obsTime = trimQuotes(commaSepList[i++]).trim();

                // if these don't work, no use in going forward
                rec = setTimeObs(obsDate, obsTime, rec, headerType);
                // get PVDR
                provider = trimQuotes(commaSepList[i++]).trim();
                rec.setProvider(provider);

                // get SUBPVDR
                sub_provider = trimQuotes(commaSepList[i++]).trim();
                if (sub_provider.equals(BLANK)) {
                    rec.setSubProvider(STRING_NULL);
                } else {
                    rec.setSubProvider(sub_provider);
                }

                // the Dataset value
                dataset = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setDataset(dataset);
                // the Restriction value
                restriction = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setRestriction(restriction);
                // get TD
                td = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setDewpoint(td);
                // get TD QCD
                td_qcd = QCD.fromString(trimQuotes(commaSepList[i++]).trim());
                rec.setDewpoint_qcd(td_qcd);
                // get TD QCA
                td_qca = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setDewpoint_qca(td_qca);
                // get TD QCR
                td_qcr = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setDewpoint_qcr(td_qcr);
                // get RH
                rh = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setRh(rh);
                // get RH QCD
                rh_qcd = QCD.fromString(trimQuotes(commaSepList[i++]).trim());
                rec.setRh_qcd(rh_qcd);
                // get RH QCA
                rh_qca = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setRh_qca(rh_qca);
                // get RH QCR
                rh_qcr = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setRh_qcr(rh_qcr);
                // altimeter
                altimeter = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setAltimeter(altimeter);
                // get Altimeter QCD
                altimeter_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setAltimeter_qcd(altimeter_qcd);
                // get Altimeter QCA
                altimeter_qca = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setAltimeter_qca(altimeter_qca);
                // get Altimeter QCR
                altimeter_qcr = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setAltimeter_qcr(altimeter_qcr);
                // get Temperature
                temperature = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setTemperature(temperature);
                // get Temperature QCD
                temperature_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setTemperature_qcd(temperature_qcd);
                // get Temperature QCA
                temperature_qca = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setTemperature_qca(temperature_qca);
                // get Temperature QCR
                temperature_qcr = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setTemperature_qcr(temperature_qcr);
                // get Wind Direction
                windDirection = new Float(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setWindDirection(windDirection);
                // get Wind Direction QCD
                windDirection_qcd = QCD
                        .fromString(trimQuotes(commaSepList[i++]).trim());
                rec.setWindDirection_qcd(windDirection_qcd);
                // get WindDirection QCA
                windDirection_qca = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setWindDirection_qca(windDirection_qca);
                // get WindDirection QCR
                windDirection_qcr = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setWindDirection_qcr(windDirection_qcr);
                // wind speed
                windSpeed = new Float(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setWindSpeed(windSpeed);
                // get Wind Speed QCD
                windSpeed_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setWindSpeed_qcd(windSpeed_qcd);
                // get WindSpeed QCA
                windSpeed_qca = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setWindSpeed_qca(windSpeed_qca);
                // get WindSpeed QCR
                windSpeed_qcr = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setWindSpeed_qcr(windSpeed_qcr);
                // get Elevation
                elevation = new Float(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                // get Elevation QCD
                elevation_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setElevation_qcd(elevation_qcd);
                // get Elevation QCA
                elevation_qca = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setElevation_qca(elevation_qca);
                // get Elevation QCR
                elevation_qcr = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setElevation_qcr(elevation_qcr);
                // get Latitude
                latitude = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                // get Latitude QCD
                latitude_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setLatitude_qcd(latitude_qcd);
                // get latitude QCA
                latitude_qca = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setElevation_qca(latitude_qca);
                // get latitude QCR
                latitude_qcr = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setElevation_qcr(latitude_qcr);
                // get Longitude
                longitude = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                // get Longitude QCD
                longitude_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setLongitude_qcd(longitude_qcd);
                // get longitude QCA
                longitude_qca = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setLongitude_qca(longitude_qca);
                // get longitude QCR
                longitude_qcr = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setLongitude_qcr(longitude_qcr);
                // get precipRate
                precipRate = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setPrecipRate(precipRate);
                // get precipRate QCD
                precipRate_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setPrecipRate_qcd(precipRate_qcd);
                // precipRate QCA
                precipRate_qca = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setPrecipRate_qca(precipRate_qca);
                // precipRate QCR
                precipRate_qca = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setPrecipRate_qca(precipRate_qcr);
                // get Wind Gust
                windGust = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setWindGust(windGust);
                // get Wind Gust QCD
                windGust_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setWindGust_qcd(windGust_qcd);
                // get Wind Gust QCA
                windGust_qca = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setPrecipRate_qca(windGust_qca);
                // get Wind Gust QCR
                windGust_qcr = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setPrecipRate_qcr(windGust_qcr);
                // get precipitalWater
                precipitalWater = new Float(trimQuotes(commaSepList[i++])
                        .trim()).floatValue();
                rec.setPrecipitalWater(precipitalWater);
                // get precipitalWater QCD
                precipitalWater_qcd = QCD.fromString(trimQuotes(
                        commaSepList[i++]).trim());
                rec.setPrecipitalWater_qcd(precipitalWater_qcd);
                // get precipitalWater QCA
                precipitalWater_qca = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setPrecipitalWater_qca(precipitalWater_qca);
                // get precipitalWater QCR
                precipitalWater_qcr = new Integer(trimQuotes(commaSepList[i++])
                        .trim()).intValue();
                rec.setPrecipitalWater_qcr(precipitalWater_qcr);
                // get pressure
                pressure = new Float(trimQuotes(commaSepList[i++]).trim())
                        .floatValue();
                rec.setPressure(pressure);
                // get pressure QCD
                pressure_qcd = QCD.fromString(trimQuotes(commaSepList[i++])
                        .trim());
                rec.setPressure_qcd(pressure_qcd);
                // get pressure QCA
                pressure_qca = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setPressure_qca(pressure_qca);
                // get pressure QCR
                pressure_qcr = new Integer(trimQuotes(commaSepList[i++]).trim())
                        .intValue();
                rec.setPressure_qcr(pressure_qcr);
                // Take care of creating the station
                rec = setObsLocation(stationId, latitude, longitude, elevation,
                        rec);
                rec = setRecordTime(rec);
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Couldn't parse F file row. " + stationId + e);
        }

        return rec;

    }

    /**
     * Remove silly quotes
     * 
     * @param val
     * @return
     */
    public String trimQuotes(String val) {

        return QUOTES_PATTERN.matcher(val).replaceAll(BLANK);
    }

    /**
     * Look for station info
     * 
     * @param icao
     * @return
     */
    private ObStation getStationInfo(String icao) {
        ObStation station = null;
        ObStationDao dao = new ObStationDao();
        if (dao != null) {
            try {
                station = dao.queryByIcao(icao);
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR, icao
                        + " -Could not create datauri in getStationInfo() ", e);
            }
        }

        return station;
    }

    /**
     * Sets the time if applicable
     * 
     * @param obsDate
     * @param obsTime
     * @param rec
     * @return
     */
    private MadisRecord setTimeObs(String obsDate, String obsTime,
            MadisRecord rec, String[] headerType) {

        if ((obsDate != null) && (obsTime != null)) {

            StringBuilder sb = new StringBuilder();
            sb.append(obsDate);
            sb.append(" ");
            sb.append(obsTime);
            sb.append(":01");

            try {

                Date time = getDateFormatter().get().parse(sb.toString());
                rec.setTimeObs(time);

            } catch (ParseException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Can't parse Madis date format!", e);
            }
        }

        return rec;
    }

    /**
     * Sets the station location info
     * 
     * @param stationId
     * @param latitude
     * @param longitude
     * @param elevation
     * @param rec
     * @return
     */
    private MadisRecord setObsLocation(String stationId, float latitude,
            float longitude, int elevation, MadisRecord rec) {

        ObStation station = getStationInfo(stationId);
        SurfaceObsLocation loc = new SurfaceObsLocation(stationId);

        try {

            if (station == null) {
                loc.assignLongitude(longitude);
                loc.assignLatitude(latitude);
                loc.setElevation(elevation);
            } else {
                // this takes care of mobile stations
                if (loc.getGeometry() != null) {
                    Coordinate coor = loc.getGeometry().getCoordinate();
                    if (coor.x != longitude) {
                        loc.assignLongitude(longitude);
                    }
                    if (coor.y != latitude) {
                        loc.assignLatitude(latitude);
                    }
                    if (elevation != MISSING_INT) {
                        if (loc.getElevation() != elevation) {
                            loc.setElevation(elevation);
                        }
                    }
                } else {
                    loc.assignLongitude(longitude);
                    loc.assignLatitude(latitude);
                    loc.setElevation(elevation);
                }
            }

            // set the location info
            rec.setLocation(loc);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Can't determine MADIS obs location! " + stationId, e);
        }

        return rec;

    }

    /**
     * Get the date formatter
     * 
     * @return
     */
    private ThreadLocal<SimpleDateFormat> getDateFormatter() {

        return madisDateFormat;
    }

    /**
     * Sets the time and basically finishes the record decode
     * 
     * @param rec
     * @return
     */
    private MadisRecord setRecordTime(MadisRecord rec) {
        try {
            rec.setDataTime(new DataTime(rec.getTimeObs()));
            return rec;
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Can't set Madis Record URI! "
                    + rec.getStationId(), e);
        }

        return null;
    }

}
