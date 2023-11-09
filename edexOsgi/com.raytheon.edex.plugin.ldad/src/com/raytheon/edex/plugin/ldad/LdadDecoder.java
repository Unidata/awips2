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

package com.raytheon.edex.plugin.ldad;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.time.DateTimeException;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.ldad.common.DecodedData;
import com.raytheon.edex.plugin.ldad.common.LdadField;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.ldad.LdadRecord;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.DataTime;

import net.sf.cglib.beans.BeanMap;
import tec.uom.se.format.SimpleUnitFormat;

/**
 * Decoder implementation for ldadmesonet plugin.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 04, 2009           vkorolev  Initial creation
 * May 15, 2013  1869     bsteffen  Remove DataURI column from ldadmesonet.
 * Aug 30, 2013  2298     rjpeter   Make getPluginName abstract
 * Jul 23, 2014  3410     bclement  location changed to floats
 * Aug 15, 2014  3530     bclement  no longer extends AbstractDecoder
 * Jul 08, 2016  5744     mapeters  Config file moved from edex_static to
 *                                  common_static
 * Dec 18, 2017  6897     tgurney   Handle date value in a Double field
 * Mar 06, 2018  6851     randerso  Added lookup table for time zones. Lots of
 *                                  code cleanup.
 * May 09, 2018  7288     randerso  Use stationId if available
 * Apr 15, 2019  7596     lsingh    Updated units framework to JSR-363.
 *                                  Handled unit conversion.
 *
 * </pre>
 *
 * @author vkorolev
 */

public class LdadDecoder {
    private static final Logger logger = LoggerFactory
            .getLogger(LdadDecoder.class);

    private static final String DATE_TIME_STRING_UNITS = "DATE_TIME_STRING";

    private static final String OBSERVATION_TIME_KEY = "observationTime";

    private static final String TIMEZONE_KEY = "_tz";

    private static final String PROVIDER_ID_KEY = "providerId";

    private static final String STATION_ID_KEY = "stationId";

    private static final String LATITUDE_KEY = "_lat";

    private static final String LONGITUDE_KEY = "_lon";

    private static final String ELEVATION_KEY = "_elev";

    private static final String BAD_PROPERTY_FMT = "NumberFormatException setting property %s.%s(%s %s)";

    private static final String DATE_FORMAT = "yy/MM/dd HH:mm:ss";

    private static final ThreadLocal<SimpleDateFormat> DateFormatter = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            return new SimpleDateFormat(DATE_FORMAT);
        }
    };

    private static final TimeZone UTC = TimeZone.getTimeZone("UTC");

    private static JAXBManager jaxb;

    private static Properties ldadUnitsMap = new Properties();

    private static Date ldadUnitsDate = new Date(0);

    private static String ldadUnitsChecksum = ILocalizationFile.NON_EXISTENT_CHECKSUM;

    private static Properties ldadTimeZoneMap = new Properties();

    private static Date ldadTimeZoneDate = new Date(0);

    private static String ldadTimeZoneChecksum = ILocalizationFile.NON_EXISTENT_CHECKSUM;

    private final Class<? extends LdadRecord> recordClass;

    private final String storageType;

    /**
     * Constructor
     *
     * @param recordClass
     *            LdadRecord subclass to be decoded
     * @param storageType
     *            storageType of LDAD files to accept. All others are ignored.
     * @throws JAXBException
     */
    public LdadDecoder(Class<? extends LdadRecord> recordClass,
            String storageType) throws JAXBException {
        this.recordClass = recordClass;
        this.storageType = storageType;
        synchronized (LdadDecoder.class) {
            if (jaxb == null) {
                jaxb = new JAXBManager(DecodedData.class);
            }
        }
    }

    /**
     * Decode the raw data in to PluginDataObjects
     *
     * @param data
     *            the raw data
     * @return the decoded records
     */
    public PluginDataObject[] decode(byte[] data) {
        PluginDataObject[] retVal = new PluginDataObject[0];
        if (data != null) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();

            String filePath = LocalizationUtil.join("ldad", "ldadUnitsMap.txt");
            ILocalizationFile lf = pathMgr.getStaticLocalizationFile(filePath);
            if (lf != null) {
                synchronized (ldadUnitsMap) {
                    // update the units map if necessary
                    if (loadPropertiesFile(ldadUnitsMap, lf, ldadUnitsDate,
                            ldadUnitsChecksum)) {
                        // update properties file info
                        ldadUnitsDate = lf.getTimeStamp();
                        ldadUnitsChecksum = lf.getCheckSum();
                    }
                }
            }

            filePath = LocalizationUtil.join("ldad", "ldadTimeZoneMap.txt");
            lf = pathMgr.getStaticLocalizationFile(filePath);
            if (lf != null) {
                synchronized (ldadTimeZoneMap) {
                    // update the time zone map if necessary
                    if (loadPropertiesFile(ldadTimeZoneMap, lf,
                            ldadTimeZoneDate, ldadTimeZoneChecksum)) {
                        // update properties file info
                        ldadTimeZoneDate = lf.getTimeStamp();
                        ldadTimeZoneChecksum = lf.getCheckSum();
                    }
                }
            }

            try {
                DecodedData dd = (DecodedData) jaxb.unmarshalFromInputStream(
                        new ByteArrayInputStream(data));

                // Storage type separator
                String currentFile = dd.fileName;
                if (!this.storageType.equals(dd.storageType)) {
                    logger.warn(String.format(
                            "LDAD decoder for %s received file %s of type %s. File ignored.",
                            this.storageType, currentFile, dd.storageType));
                    return retVal;
                }

                // Header
                String missingValue = dd.missingValue;

                // Number of records
                int numRecs = dd.fields.get(0).values.size();

                if (numRecs == 0) {
                    logger.info("No data in file.");
                    return retVal;
                }

                // Create a map of fields
                Map<String, LdadField> fieldMap = new HashMap<>(
                        dd.fields.size(), 1.0f);
                for (LdadField field : dd.fields) {
                    fieldMap.put(field.variableName, field);
                }
                Set<String> keySet = new HashSet<>(fieldMap.keySet());

                // Check for observation time
                if (!fieldMap.containsKey(OBSERVATION_TIME_KEY)) {
                    logger.error(String.format(
                            "No observation times present in file %s",
                            currentFile));
                    return retVal;
                }

                // Check for lat/lon
                if (!fieldMap.containsKey(LATITUDE_KEY)
                        || !fieldMap.containsKey(LONGITUDE_KEY)) {
                    logger.error(String.format("No location present in file %s",
                            currentFile));
                    return retVal;
                }

                // Assume UTC if no time zone specified in file
                if (!fieldMap.containsKey(TIMEZONE_KEY)) {
                    logger.warn(String.format(
                            "No time zone specified in file %s, assuming UTC",
                            currentFile));
                }

                // Loop through records
                BeanMap beanMap = BeanMap.create(recordClass.newInstance());
                List<PluginDataObject> records = new ArrayList<>(numRecs);
                for (int i = 0; i < numRecs; i++) {
                    LdadRecord record = recordClass.newInstance();

                    SurfaceObsLocation location = new SurfaceObsLocation();
                    record.setDataProvider(dd.provider);
                    record.setStationType(dd.type);
                    record.setReportTime(dd.reportTime);

                    // Set of all known keys remaining to be processed
                    keySet.addAll(fieldMap.keySet());

                    // Get time zone if present
                    TimeZone timeZone = UTC;
                    if (keySet.contains(TIMEZONE_KEY)) {
                        String tz = fieldMap.get(TIMEZONE_KEY).values.get(i);
                        keySet.remove(TIMEZONE_KEY);

                        synchronized (ldadTimeZoneMap) {
                            tz = ldadTimeZoneMap.getProperty(tz, tz);
                        }
                        try {
                            ZoneId zoneId = ZoneId.of(tz);
                            timeZone = TimeZone.getTimeZone(zoneId);
                        } catch (DateTimeException e) {
                            logger.error(String.format(
                                    "Unrecognized time zone: %s in record %d of file %s, assuming UTC",
                                    tz, i, currentFile), e);
                        }
                    }

                    // Get observation time
                    LdadField field = fieldMap.get(OBSERVATION_TIME_KEY);
                    keySet.remove(OBSERVATION_TIME_KEY);
                    String value = field.values.get(i);
                    if (missingValue.equals(value)) {
                        logMissingValue(field, i, currentFile);
                        continue;
                    }

                    try {
                        record.setObservationTime(parseDate(value, timeZone));
                    } catch (ParseException e) {
                        logValueError(field, i, currentFile, e);
                        continue;
                    }

                    // Get location
                    field = fieldMap.get(LATITUDE_KEY);
                    keySet.remove(LATITUDE_KEY);
                    value = field.values.get(i);
                    if (missingValue.equals(value)) {
                        logMissingValue(field, i, currentFile);
                        continue;
                    }

                    float latitude;
                    try {
                        latitude = Float.parseFloat(value);
                    } catch (NumberFormatException e) {
                        logValueError(field, i, currentFile, e);
                        continue;
                    }

                    field = fieldMap.get(LONGITUDE_KEY);
                    keySet.remove(LONGITUDE_KEY);
                    value = field.values.get(i);
                    if (missingValue.equals(value)) {
                        logMissingValue(field, i, currentFile);
                        continue;
                    }

                    float longitude;
                    try {
                        longitude = Float.parseFloat(value);
                    } catch (NumberFormatException e) {
                        logValueError(field, i, currentFile, e);
                        continue;
                    }

                    location.assignLocation(latitude, longitude);

                    if (keySet.contains(ELEVATION_KEY)) {
                        field = fieldMap.get(ELEVATION_KEY);
                        keySet.remove(ELEVATION_KEY);
                        value = field.values.get(i);
                        if (!missingValue.equals(value)) {
                            try {
                                // elevation in meter - integer in location
                                double elevation = Double.parseDouble(value);
                                location.setElevation(
                                        (int) Math.round(elevation));
                            } catch (NumberFormatException e) {
                                logValueError(field, i, currentFile, e);
                            }
                        }
                    }

                    // set station ID to provider ID in case station ID is not
                    // present
                    if (keySet.contains(PROVIDER_ID_KEY)) {
                        field = fieldMap.get(PROVIDER_ID_KEY);

                        // leave provider id in key set so providerId field is
                        // populated in the record

                        value = field.values.get(i);
                        location.setStationId(value);
                    }

                    if (keySet.contains(STATION_ID_KEY)) {
                        field = fieldMap.get(STATION_ID_KEY);
                        keySet.remove(STATION_ID_KEY);
                        value = field.values.get(i);
                        location.setStationId(value);
                    }

                    // Loop through remaining fields
                    beanMap.setBean(record);
                    for (String key : keySet) {
                        field = fieldMap.get(key);
                        String name = field.variableName;
                        if (beanMap.containsKey(name)) {
                            String units = field.units;
                            value = field.values.get(i);
                            if (!missingValue.equals(value)) {
                                try {
                                    // try setting field via reflection
                                    setProperty(name, beanMap, value, units,
                                            timeZone);
                                } catch (Throwable e) {
                                    logPropertyError(field, i, currentFile, e);
                                }
                            }
                        } else {
                            /*
                             * Some fields are not supported. Perfectly valid
                             * data can cause this exception so we log it as
                             * debug
                             */
                            logger.debug(String.format(
                                    "Unrecognized field: %s, will be ignored.",
                                    name));
                        }
                    }
                    record = (LdadRecord) beanMap.getBean();

                    // DataTime = Observation time
                    Date ot = record.getObservationTime();
                    if (ot != null) {
                        DataTime dt = new DataTime(ot);
                        record.setDataTime(dt);
                        record.setLocation(location);
                        record.setRawMessage(record.toMessage());
                        records.add(record);
                    }
                }

                retVal = records.toArray(new PluginDataObject[records.size()]);

            } catch (SerializationException e) {
                logger.error("Unable to unmarshall xml:", e);
            } catch (RuntimeException e) {
                logger.error("Error decoding ldad mesonet data:" + e);
            } catch (InstantiationException | IllegalAccessException e) {
                logger.error("Unable to instantiate class: "
                        + this.recordClass.getName(), e);
            }
        }

        return retVal;
    }

    /**
     * Loads properties from a localization file
     *
     * @param props
     *            properties instance to be updated
     * @param lf
     *            localization file from which to load properties
     * @return true if properties were successfully loaded
     */
    private boolean loadPropertiesFile(Properties props, ILocalizationFile lf,
            Date lastTimeStamp, String lastChecksum) {
        boolean status = false;
        if (lf.exists() && (!lastChecksum.equals(lf.getCheckSum())
                || !lastTimeStamp.equals(lf.getTimeStamp()))) {
            try (InputStream is = lf.openInputStream()) {
                Properties newProps = new Properties();
                newProps.load(is);
                props.clear();
                props.putAll(newProps);
                status = true;
            } catch (LocalizationException | IOException e) {
                logger.error("Error loading properites from: " + lf, e);
            }
        }
        return status;
    }

    private void logMissingValue(LdadField field, int index, String file) {
        logger.error(String.format(
                "Missing value in field: %s value: %s with units: %s for file: %s",
                field.variableName, field.values.get(index), field.units,
                file));
    }

    private void logValueError(LdadField field, int index, String file,
            Throwable e) {
        logger.error(String.format(
                "Invalid value in field: %s value: %s with units: %s for file: %s",
                field.variableName, field.values.get(index), field.units, file),
                e);
    }

    private void logPropertyError(LdadField field, int index, String file,
            Throwable e) {
        logger.error(String.format(
                "Unable to set property %s to value: %s with units: %s for file: %s",
                field.variableName, field.values.get(index), field.units, file),
                e);
    }

    private void setProperty(String name, BeanMap beanMap, String value,
            String units, TimeZone timeZone) throws ParseException {

        Object val = null;
        boolean abort = false;
        Class<?> clazz = beanMap.getPropertyType(name);

        // Type filter
        if (String.class == clazz) {
            val = value.trim();
        } else if (Calendar.class == clazz) {
            val = parseDate(value, timeZone);
        } else if (DATE_TIME_STRING_UNITS.equals(units)) {
            // String date/time in a Double field, convert to epoch seconds
            Date date = parseDate(value, timeZone);
            val = date.getTime() / 1000.0;
        } else {

            // Get rid of some troublesome data
            // TODO: find out what should be done with these values
            abort = "B".equals(value);
            abort |= "R".equals(value);
            abort |= "V".equals(value);
            abort |= "NAN0".equals(value);

            if (!abort) {
                Double tval = null;
                try {
                    tval = Double.parseDouble(value);
                } catch (NumberFormatException nfe) {
                    String msg = String.format(BAD_PROPERTY_FMT,
                            beanMap.getBean().getClass().getSimpleName(), name,
                            clazz.getSimpleName(), value);
                    logger.error(msg, nfe);
                    return;
                }
                synchronized (ldadUnitsMap) {
                    if (ldadUnitsMap.containsKey(units)) {
                        String translatedUnit = ldadUnitsMap.getProperty(units,
                                units);

                        Unit<?> inUnit = (Unit<?>) SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII)
                                .parseObject(translatedUnit, new ParsePosition(0));

                        String propUnit = ldadUnitsMap.getProperty(name);
                        if (propUnit == null) {
                            logger.error(String.format(
                                    "No units defined in ldadUnitsMap.txt for property: %s",
                                    name));
                        } else {
                            Unit<?> outUnit = (Unit<?>) SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII)
                                    .parseObject(propUnit, new ParsePosition(0));
                            try {
                                tval = inUnit.getConverterToAny(outUnit)
                                        .convert((tval).doubleValue());
                            } catch (IncommensurableException | UnconvertibleException e) {
                                logger.error(String.format(
                                        "Property[%s] Input unit %s not compatible with Output unit %s",
                                        name, units, outUnit), e);
                                return;
                            }
                        }
                    }
                }

                if (clazz == Integer.class) {
                    val = tval.intValue();
                } else if (clazz == Short.class) {
                    val = tval.shortValue();
                } else if (clazz == Float.class) {
                    val = tval.floatValue();
                } else {
                    val = tval;
                }
            }
        }
        if (!abort) {
            beanMap.put(name, val);
        }
    }

    private Date parseDate(String dateTime, TimeZone timeZone)
            throws ParseException {
        SimpleDateFormat sdf = DateFormatter.get();
        sdf.setTimeZone(timeZone);

        Date date = sdf.parse(dateTime);
        return date;
    }
}
