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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.apache.commons.lang.StringUtils;

import ucar.nc2.Attribute;
import ucar.nc2.Group;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSCommonData;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSSpatialRecord;
import com.raytheon.uf.common.time.DataTime;

/**
 * Decoder for VIIRS data and spatial files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDecoder extends AbstractDecoder {

    private static final String TIME_FORMAT = "ddHHmm";

    private static final Map<String, String> regionMap = new HashMap<String, String>();

    private static final Map<String, VIIRSChannelInfo> channelMap = new HashMap<String, VIIRSChannelInfo>();

    static {
        regionMap.put("IPUS", "CONUS");
        regionMap.put("IPAK", "Alaska");
        regionMap.put("IPPA", "Pacific");
        regionMap.put("IPCA", "Caribbean");

        channelMap.put("41", new VIIRSChannelInfo("Imagery", 0.64, 1));
        channelMap.put("42", new VIIRSChannelInfo("Imagery", 0.865, 2));
        channelMap.put("43", new VIIRSChannelInfo("Imagery", 1.61, 3));
        channelMap.put("44", new VIIRSChannelInfo("Imagery", 3.74, 4));
        channelMap.put("45", new VIIRSChannelInfo("Imagery", 11.45, 5));
        channelMap.put("81", new VIIRSChannelInfo("Imagery", null, null));
        channelMap.put("56", new VIIRSChannelInfo("Moderate", 0.746, 6));
        channelMap.put("59", new VIIRSChannelInfo("Moderate", 1.378, 9));
        channelMap.put("63", new VIIRSChannelInfo("Moderate", 4.05, 13));
        channelMap.put("65", new VIIRSChannelInfo("Moderate", 10.763, 15));
        channelMap.put("66", new VIIRSChannelInfo("Moderate", 12.013, 16));
        channelMap.put("82", new VIIRSChannelInfo("Moderate", null, null));
        channelMap.put("67", new VIIRSChannelInfo("DayNight", 0.7, null));
        channelMap.put("83", new VIIRSChannelInfo("DayNight", null, null));
    }

    private static final String NETCDF4StartSequence = new String(new byte[] {
            (byte) 0x89, 'H', 'D', 'F', (byte) 0x0d });

    private static final long TWENTY_FIVE_DAYS = 1000 * 60 * 60 * 24 * 25;

    private File tmpDataDir;

    public VIIRSDecoder(String tmpDataDir) {
        this.tmpDataDir = new File(tmpDataDir);
        if (this.tmpDataDir.exists() == false) {
            this.tmpDataDir.mkdirs();
        }
    }

    public PluginDataObject[] decode(File viirsFile, Headers headers) {
        PluginDataObject[] rval = new PluginDataObject[0];
        File tmpFile = new File(tmpDataDir, viirsFile.getName());
        try {
            FileInputStream fin = new FileInputStream(viirsFile);
            FileOutputStream fout = new FileOutputStream(tmpFile);
            byte[] bytes = new byte[1024];
            int read = 0;
            boolean startFound = false;
            while ((read = fin.read(bytes)) > 0) {
                byte[] readIn = Arrays.copyOf(bytes, read);
                if (!startFound) {
                    String txt = new String(readIn);
                    int idx = txt.indexOf(NETCDF4StartSequence, 0);
                    if (idx >= 0) {
                        startFound = true;
                        fout.write(readIn, idx, readIn.length - idx);
                    }
                } else {
                    fout.write(readIn);
                }
            }
            fout.flush();
            fout.close();
            fin.close();
        } catch (Exception e) {
            logger.error("Error extracting header from file", e);
        }

        try {
            NetcdfFile dataFile = NetcdfFile.open(tmpFile.getAbsolutePath());
            Group root = dataFile.getRootGroup();

            String wmoHeader = (String) headers.get("header");
            String[] parts = StringUtils.split(wmoHeader, ' ');
            if (parts.length != 3) {
                logger.error("Could not parse wmo header (" + wmoHeader
                        + ") properly");
                return rval;
            }
            String regionId = parts[0].substring(0, 4);
            String channelId = parts[0].substring(4);
            String time = parts[2];

            VIIRSChannelInfo channelInfo = channelMap.get(channelId);
            if (channelInfo == null) {
                logger.error("Could not find channel information for wmo header ("
                        + wmoHeader + ")");
                return rval;
            }
            String region = regionMap.get(regionId);
            if (region == null) {
                logger.error("Could not find region information for wmo header ("
                        + wmoHeader + ")");
                return rval;
            }
            VIIRSCommonData commonData = new VIIRSCommonData();
            commonData.setRegion(region);
            commonData.setChannelType(channelInfo.getChannelType());

            SimpleDateFormat sdf = new SimpleDateFormat(TIME_FORMAT);
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            Date date = sdf.parse(time);
            Calendar dataCal = Calendar
                    .getInstance(TimeZone.getTimeZone("GMT"));
            dataCal.setTime(date);
            Calendar currCal = Calendar
                    .getInstance(TimeZone.getTimeZone("GMT"));
            dataCal.set(Calendar.YEAR, currCal.get(Calendar.YEAR));
            dataCal.set(Calendar.MONTH, currCal.get(Calendar.MONTH));
            DataTime dataTime = new DataTime(dataCal.getTime());

            if (currCal.getTimeInMillis() - dataCal.getTimeInMillis() > TWENTY_FIVE_DAYS) {
                // This check is for around midnight on last day of month, data
                // might come in ahead of time and we don't want to jump
                // backwards in time
                dataCal.add(Calendar.MONTH, 1);
            }

            PluginDataObject pdo = null;
            if (channelInfo.getChannel() == null
                    && channelInfo.getWavelength() == null) {
                pdo = decodeSpatialFile(root, commonData, dataTime);
            } else {
                pdo = decodeDataFile(root, commonData, channelInfo, dataTime);
            }

            if (pdo != null) {
                pdo.setInsertTime(currCal);
                pdo.constructDataURI();
                rval = new PluginDataObject[] { pdo };
            } else {
                logger.error("Error decoding netcdf file (" + tmpFile
                        + ") to PluginDataObject");
            }
        } catch (Exception e) {
            logger.error("Error decoding viirs data file: " + tmpFile, e);
        } finally {
            if (tmpFile != null) {
                tmpFile.delete();
            }
        }
        return rval;
    }

    private static final String BRIGHTNESS_TEMP_REFLECTANCE_ID = "BrightnessTemperatureOrReflectance@";

    private static final String RADIANCE_ID = "Radiance@";

    /**
     * @param root
     * @param commonData
     * @param channelInfo
     * @return
     * @throws PluginException
     */
    private PluginDataObject decodeDataFile(Group root,
            VIIRSCommonData commonData, VIIRSChannelInfo channelInfo,
            DataTime dataTime) throws IOException, PluginException {
        VIIRSDataRecord record = new VIIRSDataRecord();
        record.setDataTime(dataTime);
        record.setCommonData(commonData);
        record.setLevels(1);

        record.setChannel(channelInfo.getChannel());
        record.setWavelength(channelInfo.getWavelength());

        int width = 0, height = 0;
        float offset = 0.0f, scale = 1.0f;
        int[] missingValues = null;
        short[] rawData = null;
        for (Variable var : root.getVariables()) {
            if (var.getFullName().startsWith(BRIGHTNESS_TEMP_REFLECTANCE_ID)
                    || var.getFullName().startsWith(RADIANCE_ID)) {
                rawData = (short[]) var.read().copyTo1DJavaArray();
                // Backwards!?
                width = var.getDimension(1).getLength();
                height = var.getDimension(0).getLength();
                for (Attribute attr : var.getAttributes()) {
                    String name = attr.getName();
                    if (VIIRSDataRecord.MISSING_VALUE_ID.equals(name)) {
                        missingValues = (int[]) attr.getValues()
                                .copyTo1DJavaArray();
                    } else if (VIIRSDataRecord.OFFSET_ID.equals(name)) {
                        offset = attr.getNumericValue().floatValue();
                    } else if (VIIRSDataRecord.SCALE_ID.equals(name)) {
                        scale = attr.getNumericValue().floatValue();
                    }
                }
                break;
            }
        }

        if (rawData == null) {
            throw new IOException("Error extracting rawData from data file");
        }

        record.setWidth(width);
        record.setHeight(height);

        VIIRSMessageData messageData = new VIIRSMessageData();
        messageData.rawData = rawData;
        messageData.scale = scale;
        messageData.offset = offset;
        messageData.missingValues = missingValues;

        record.setMessageData(messageData);
        // This will construct the spatialURI
        record.getSpatialURI();
        return record;
    }

    private static final String LATITUDE_ID = "Latitude@";

    private static final String LONGITUDE_ID = "Longitude@";

    /**
     * @param root
     * @param commonData
     * @return
     */
    private PluginDataObject decodeSpatialFile(Group root,
            VIIRSCommonData commonData, DataTime dataTime) throws IOException {
        VIIRSSpatialRecord record = new VIIRSSpatialRecord();
        record.setDataTime(dataTime);
        record.setCommonData(commonData);
        record.setLevels(1);

        int width = 0, height = 0;
        float[] latitudes = null, longitudes = null;
        double[] missingValues = null;
        for (Variable var : root.getVariables()) {
            if (var.getFullName().startsWith(LATITUDE_ID) && latitudes == null) {
                latitudes = (float[]) var.read().copyTo1DJavaArray();
            } else if (var.getFullName().startsWith(LONGITUDE_ID)
                    && longitudes == null) {
                width = var.getDimension(1).getLength();
                height = var.getDimension(0).getLength();
                longitudes = (float[]) var.read().copyTo1DJavaArray();
                for (Attribute attr : var.getAttributes()) {
                    if (VIIRSDataRecord.MISSING_VALUE_ID.equals(attr.getName())) {
                        missingValues = (double[]) attr.getValues()
                                .copyTo1DJavaArray();
                    }
                }
            }
        }

        if (latitudes == null || longitudes == null) {
            throw new IOException("Error extracting lat/lons from spatial file");
        }

        record.setWidth(width);
        record.setHeight(height);

        VIIRSSpatialMessageData messageData = new VIIRSSpatialMessageData();
        messageData.latitudes = latitudes;
        messageData.longitudes = longitudes;

        if (missingValues != null) {
            messageData.missingValues = new float[missingValues.length];
            for (int i = 0; i < missingValues.length; ++i) {
                messageData.missingValues[i] = (float) missingValues[i];
            }
            // Find first row with bad data
            int badRow = height;
            for (int h = 0; h < height && badRow == height; ++h) {
                float value = longitudes[h * width];
                for (float missingVal : messageData.missingValues) {
                    if (missingVal == value) {
                        badRow = h;
                        break;
                    }
                }
            }

            messageData.validHeight = badRow;
        } else {
            // No missing values present
            messageData.validHeight = height;
        }
        record.setMessageData(messageData);
        return record;
    }

}
