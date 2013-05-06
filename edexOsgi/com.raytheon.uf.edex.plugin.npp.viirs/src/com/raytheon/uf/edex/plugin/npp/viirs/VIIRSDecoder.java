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
package com.raytheon.uf.edex.plugin.npp.viirs;

import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import ucar.ma2.Array;
import ucar.nc2.Attribute;
import ucar.nc2.Group;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSSpatialCoverage;
import com.raytheon.uf.common.dataplugin.npp.viirs.projection.VIIRSMapProjection;
import com.raytheon.uf.common.geospatial.interpolation.GridDownscaler;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.plugin.npp.AbstractNPPDecoder;
import com.raytheon.uf.edex.plugin.npp.viirs.dao.VIIRSDao;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Decoder for VIIRS data and spatial files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2011             mschenke    Initial creation
 * Feb 21, 2012  #30       mschenke    Changed VIIRS decoder to read time attribute out of record
 *                                     instead of using WMO header
 * May 01, 2013 1962       bsteffen    Allow Viirs Decoder to accept numeric
 *                                     missing values.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDecoder extends AbstractNPPDecoder {

    private static final GeometryFactory gf = new GeometryFactory();

    private static final String SPLIT_STRING = "[@]";

    private static final String MISSING_VALUE_SPLIT_STRING = "[ ]";

    private static final String MISSING_VALUE_ID = "missing_value";

    private static final String LATITUDE_RING_ID = "g_ring_latitude";

    private static final String LONGITUDE_RING_ID = "g_ring_longitude";

    private static final String LATITUDE_DATASET_ID = "Latitude@";

    private static final String LONGITUDE_DATASET_ID = "Longitude@";

    private static final Map<String, String> FACTOR_MAP = new HashMap<String, String>();

    private static final Map<String, String> PARAMETER_MAP = new HashMap<String, String>();

    private static final String ALBEDO_ID = "Albedo";

    private static final String BRIGHTNESS_TEMP_ID = "BrightnessTemperature";

    private static final String REFLECTANCE_ID = "Reflectance";

    private static final String BRIGHTNESS_TEMP_OR_REF_ID = "BrightnessTemperatureOrReflectance";

    private static final String RADIANCE_ID = "Radiance";

    static {
        FACTOR_MAP.put(ALBEDO_ID, "AlbedoFactors");
        PARAMETER_MAP.put(ALBEDO_ID, "Ref");
        FACTOR_MAP.put(BRIGHTNESS_TEMP_ID, "BrightnessFactors");
        PARAMETER_MAP.put(BRIGHTNESS_TEMP_ID, "BT");
        FACTOR_MAP.put(REFLECTANCE_ID, "ReflectanceFactors");
        PARAMETER_MAP.put(REFLECTANCE_ID, "Ref");
        FACTOR_MAP.put(RADIANCE_ID, "RadianceFactors");
        PARAMETER_MAP.put(RADIANCE_ID, "Rad");

        FACTOR_MAP.put(BRIGHTNESS_TEMP_OR_REF_ID,
                "BrightnessTemperatureOrReflectanceFactors");
    }

    private static final String VIIRS_MAPPING_FILE = "viirs"
            + IPathManager.SEPARATOR + "viirsHeaderMapping.xml";

    private VIIRSDao dao;

    private VIIRSHeaderMapping mapping;

    /**
     * 
     */
    public VIIRSDecoder() {
        InputStream in = null;
        try {
            JAXBManager manager = new JAXBManager(VIIRSHeaderMapping.class);
            LocalizationFile mappingFile = PathManagerFactory.getPathManager()
                    .getStaticLocalizationFile(VIIRS_MAPPING_FILE);
            in = mappingFile.openInputStream();
            mapping = (VIIRSHeaderMapping) manager
                    .jaxbUnmarshalFromInputStream(in);
        } catch (Exception e) {
            throw new RuntimeException(
                    "Error deserializing VIIRS header mapping file", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    logger.warn("Error closing LocalizationFile InputStream", e);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.npp.AbstractNPPDecoder#decodeNetcdf(ucar.
     * nc2.NetcdfFile, com.raytheon.uf.common.time.DataTime,
     * com.raytheon.edex.esb.Headers)
     */
    @Override
    protected Object decodeNetcdf(NetcdfFile dataFile, DataTime dataTime,
            Headers headers) {
        PluginDataObject[] rval = new PluginDataObject[0];
        try {
            Group root = dataFile.getRootGroup();

            String header = (String) headers.get("header");
            String[] parts = StringUtils.split(header, ' ');
            if (parts == null || parts.length < 3) {
                logger.error("Could not parse wmo header (" + header
                        + ") properly");
                return rval;
            }
            String regionId = parts[0].substring(0, 4);
            String channelId = parts[0].substring(4);

            String region = mapping.getRegion(regionId);
            if (region == null) {
                region = "Unknown";
                logger.warn("Could not find region information for wmo header ("
                        + header + ")");
            }

            VIIRSChannelInfo channelInfo = mapping.getChannelInfo(channelId);
            if (channelInfo == null) {
                logger.error("Could not find channel information for wmo header ("
                        + header + ")");
                return rval;
            }

            VIIRSDataRecord record = null;
            VIIRSMessageData messageData = null;

            Double dx, dy;
            dx = dy = channelInfo.getResolution();
            Integer nx, ny;
            nx = ny = 0;
            String channelType = channelInfo.getChannelType();

            for (Variable var : root.getVariables()) {
                // Create VIIRSDataRecord and extract nx/ny info
                String fullName = var.getFullName().split(SPLIT_STRING)[0];
                String factorsId = FACTOR_MAP.get(fullName);
                if (factorsId != null) {
                    String parameterId = null;
                    if (BRIGHTNESS_TEMP_OR_REF_ID.equals(fullName)) {
                        if (channelInfo.getUnit() == null) {
                            parameterId = PARAMETER_MAP.get(REFLECTANCE_ID);
                        } else {
                            parameterId = PARAMETER_MAP.get(BRIGHTNESS_TEMP_ID);
                        }
                    } else {
                        parameterId = PARAMETER_MAP.get(fullName);
                    }

                    // Found data record dataset
                    record = new VIIRSDataRecord();
                    record.setDataTime(dataTime);

                    record.setParameter(parameterId);
                    record.setChannel(channelInfo.getChannel());
                    record.setWavelength(channelInfo.getWavelength());
                    record.setRegion(region);
                    record.setChannelType(channelType);

                    float offset = 0.0f, scale = 1.0f;
                    float[] missingValues = new float[] { 65535.0f };
                    Object rawData = var.read().copyTo1DJavaArray();
                    if (rawData == null) {
                        logger.warn("Could not find rawdata for dataset entry: "
                                + var.getFullName());
                        continue;
                    }
                    // Backwards!?
                    nx = var.getDimension(1).getLength();
                    ny = var.getDimension(0).getLength();

                    for (Attribute attr : var.getAttributes()) {
                        if (MISSING_VALUE_ID.equals(attr.getName())) {
                            if (attr.getDataType().isString()) {
                                String missing = attr.getStringValue();
                                String[] split = missing
                                        .split(MISSING_VALUE_SPLIT_STRING);
                                missingValues = new float[split.length];
                                for (int i = 0; i < split.length; ++i) {
                                    missingValues[i] = Float
                                            .parseFloat(split[i]);
                                }
                            } else if(attr.getDataType().isNumeric()){
                                if(attr.isArray()){
                                    missingValues = new float[attr.getLength()];
                                    for (int i = 0; i < missingValues.length; i += 1) {
                                        missingValues[i] = attr
                                                .getNumericValue(i)
                                                .floatValue();
                                    }
                                }else{
                                    missingValues = new float[] { attr
                                            .getNumericValue().floatValue() };
                                }
                            }
                            break;
                        }
                    }

                    // Search for factors dataset
                    for (Variable var2 : root.getVariables()) {
                        fullName = var2.getFullName().split(SPLIT_STRING)[0];
                        if (fullName.equals(factorsId)) {
                            Array factors = var2.read();
                            scale = factors.getFloat(0);
                            offset = factors.getFloat(1);
                            break;
                        }
                    }

                    messageData = new VIIRSMessageData();
                    messageData.setRawData(rawData);
                    messageData.setScale(scale);
                    messageData.setOffset(offset);
                    messageData.setMissingValues(missingValues);
                    messageData.setUnitString(channelInfo.getUnit());
                    break;
                }
            }

            if (record != null) {
                // Look for existing spatial coverage object]
                VIIRSSpatialCoverage spatialRecord = dao.lookupCoverage(
                        dataTime, dx.floatValue());
                if (spatialRecord == null) {
                    // No coverage found, create and persist!
                    spatialRecord = new VIIRSSpatialCoverage();
                    spatialRecord.setDataTime(dataTime);
                    spatialRecord.setDx(dx.floatValue());
                    spatialRecord.setDy(dy.floatValue());
                    spatialRecord.setNx(nx);
                    spatialRecord.setNy(ny);

                    int width = 3;
                    float[] latitudes = null, longitudes = null;
                    for (Variable var : root.getVariables()) {
                        String fullName = var.getFullName();
                        if (fullName.startsWith(LATITUDE_DATASET_ID)
                                && latitudes == null) {
                            latitudes = (float[]) var.read()
                                    .copyTo1DJavaArray();
                        } else if (fullName.startsWith(LONGITUDE_DATASET_ID)
                                && longitudes == null) {
                            longitudes = (float[]) var.read()
                                    .copyTo1DJavaArray();
                        }
                    }

                    if (latitudes == null || longitudes == null) {
                        throw new IOException(
                                "Error extracting lat/lons from data file");
                    }

                    // Mark valid height if missing values found
                    int validHeight = spatialRecord.getNy();
                    for (int i = 0; i < validHeight; i++) {
                        float latitude = latitudes[i * width];
                        if (latitude < -90.0f || latitude > 90.0f) {
                            validHeight = i;
                        }
                    }

                    float[] nadirLats = new float[validHeight];
                    float[] nadirLons = new float[validHeight];

                    float[] directions = new float[validHeight];

                    double PIO2 = Math.PI / 2;
                    double TWOPI = Math.PI * 2;

                    int centerOffset = width / 2;
                    for (int h = 0; h < validHeight; ++h) {
                        int idx = h * width;
                        float nadirLat = latitudes[idx + centerOffset];
                        float nadirLon = longitudes[idx + centerOffset];
                        float firstLat = latitudes[idx];
                        float firstLon = longitudes[idx];

                        float direction = (float) (VIIRSMapProjection.azm_sidb(
                                Math.toRadians(nadirLat),
                                Math.toRadians(nadirLon),
                                Math.toRadians(firstLat),
                                Math.toRadians(firstLon))[0] - PIO2);

                        if (direction < 0) {
                            direction += TWOPI;
                        }

                        directions[h] = direction;
                        nadirLats[h] = nadirLat;
                        nadirLons[h] = nadirLon;
                    }

                    spatialRecord.setCenterLatitudes(nadirLats);
                    spatialRecord.setCenterLongitudes(nadirLons);
                    spatialRecord.setDirections(directions);

                    double[] latRing = null;
                    double[] lonRing = null;

                    for (Attribute attr : root.getAttributes()) {
                        if (LATITUDE_RING_ID.equals(attr.getName())) {
                            latRing = (double[]) attr.getValues()
                                    .copyTo1DJavaArray();
                            if (lonRing != null) {
                                break;
                            }
                        } else if (LONGITUDE_RING_ID.equals(attr.getName())) {
                            lonRing = (double[]) attr.getValues()
                                    .copyTo1DJavaArray();
                            if (latRing != null) {
                                break;
                            }
                        }
                    }

                    Geometry boundary = null;
                    if (latRing != null && lonRing != null) {
                        Coordinate[] coords = new Coordinate[latRing.length];
                        for (int i = 0; i < latRing.length; ++i) {
                            coords[i] = new Coordinate(lonRing[i], latRing[i]);
                        }
                        boundary = gf.createPolygon(
                                gf.createLinearRing(coords), null);
                    }
                    spatialRecord.setEnvelope(boundary.getEnvelope());
                    // Persist the coverage
                    dao.saveOrUpdate(spatialRecord);
                }

                record.setLevels(GridDownscaler.getDownscaleSizes(spatialRecord
                        .getGridGeometry()).length);
                record.setCoverage(spatialRecord);
                record.setMessageData(messageData);
            }

            Calendar insertCal = Calendar.getInstance();
            if (record != null) {
                record.setInsertTime(insertCal);
                record.constructDataURI();
                rval = new PluginDataObject[] { record };
            } else {
                logger.error("Error decoding netcdf file in to PluginDataObjects");
            }
        } catch (Exception e) {
            logger.error("Error decoding viirs data file", e);
        }

        return rval;
    }

    /**
     * @param dao
     *            the dao to set
     */
    public void setDao(VIIRSDao dao) {
        this.dao = dao;
    }

}
