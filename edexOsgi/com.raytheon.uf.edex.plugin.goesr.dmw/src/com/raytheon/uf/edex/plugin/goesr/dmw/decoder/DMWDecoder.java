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
package com.raytheon.uf.edex.plugin.goesr.dmw.decoder;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ucar.ma2.Array;
import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.goesr.dmw.DMWRecord;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;

/**
 * Decoder for Derived Motion Wind products.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2015  4334       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public class DMWDecoder {
    /**
     * The only Data Quality Flag indicating valid data. Any non-zero value
     * indicates an invalid point.
     */
    private static final int VALID_DQF = 0;

    private static final String MESOSCALE = "Mesoscale";

    /** Pattern to extract mesoscale scene number from the dataset name. */
    private static final Pattern MESO_SCENE_PATTERN = Pattern
            .compile("DMWM(?<mesoscene>\\d)");

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DMWDecoder.class);

    private static final Calendar epoch;

    private static final String COVERAGE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.S'Z'";

    static {
        epoch = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        epoch.clear();
        epoch.setTimeZone(TimeZone.getTimeZone("GMT"));
        epoch.set(Calendar.YEAR, 2000);
        epoch.set(Calendar.MONTH, Calendar.JANUARY);
        epoch.set(Calendar.DAY_OF_MONTH, 1);
        epoch.set(Calendar.HOUR_OF_DAY, 12);
        epoch.set(Calendar.MINUTE, 0);
        epoch.set(Calendar.SECOND, 0);
        epoch.set(Calendar.MILLISECOND, 0);
    }

    /**
     * Constructor.
     */
    public DMWDecoder() {
        super();
    }

    /**
     * Decode a GOES-R DMW file.
     *
     * @param data
     *            The file data.
     * @param headers
     *            The headers.
     * @return The decoded objects.
     */
    public PluginDataObject[] decode(byte[] data, Headers headers) {

        String traceId = "";

        List<PluginDataObject> records = new ArrayList<>();

        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        try {
            if (data != null && data.length > 0) {
                try {
                    decodeData(data, traceId, records);
                } catch (Exception e) {
                    statusHandler.error(traceId + "-Error in decode", e);
                } finally {
                    if (records.isEmpty()) {
                        statusHandler.info(String.format("%s - Decoded no obs",
                                traceId));
                    } else {
                        statusHandler.info(String.format("%s - Decoded %d obs",
                                traceId, records.size()));
                    }
                }
            } else {
                statusHandler.info(traceId + "- No data in file");
            }
        } catch (Exception e) {
            statusHandler.error(traceId + "- Decoder error", e);
        }

        return records.toArray(new PluginDataObject[0]);
    }

    /**
     * Decodes the file, adding valid obs to the records list.
     *
     * @param messageData
     *            The data.
     * @param traceId
     *            The traceId.
     * @param records
     *            The list to add valid records to.
     * @throws Exception
     *             if there's any unrecoverable issues with the file.
     */
    private void decodeData(byte[] messageData, String traceId,
            List<PluginDataObject> records) throws Exception {
        NetcdfFile dataFile = NetcdfFile.openInMemory(UUID.randomUUID()
                .toString(), messageData);

        Variable latVar = dataFile.findVariable("lat");
        Variable lonVar = dataFile.findVariable("lon");
        Variable spdVar = dataFile.findVariable("wind_speed");
        Variable dirVar = dataFile.findVariable("wind_direction");
        Variable dqfVar = dataFile.findVariable("DQF"); // quality factor

        // Full Disk, CONUS, or Mesoscale.
        String scene = dataFile.findGlobalAttribute("scene_id")
                .getStringValue();
        if (MESOSCALE.equals(scene)) {
            // determine which mesoscale scene from dataset name.
            String dataset = dataFile.findGlobalAttribute("dataset_name")
                    .getStringValue();
            Matcher matcher = MESO_SCENE_PATTERN.matcher(dataset);
            if (matcher.find()) {
                String mesoScene = matcher.group("mesoscene");
                scene = scene + mesoScene;
            } else {
                statusHandler.info(traceId
                        + " - Using default Mesoscale Scene.");
                scene = scene + "1";
            }
        }
        String orbitalSlot = dataFile.findGlobalAttribute("orbital_slot")
                .getStringValue();
        int channel = dataFile.findVariable("band_id").readScalarInt();

        if (latVar == null || lonVar == null || spdVar == null
                || dirVar == null || dqfVar == null) {
            throw new IllegalArgumentException(
                    "File does not contain a valid derived motion wind product.");
        }
        Array lats = latVar.read();
        Array lons = lonVar.read();
        Array spds = spdVar.read();
        Array dirs = dirVar.read();
        Array dqfs = dqfVar.read();

        long latsSize = lats.getSize();
        if (latsSize != lons.getSize() || latsSize != spds.getSize()
                || latsSize != dirs.getSize() || latsSize != dqfs.getSize()) {
            throw new IllegalArgumentException(
                    "File data is not of the same length.");
        }

        Calendar datetime = getDateTime(dataFile, traceId);

        double lat;
        double lon;
        float speed;
        float direction;
        byte quality;
        DMWRecord record;
        SurfaceObsLocation location;
        while (lats.hasNext() && lons.hasNext() && spds.hasNext()
                && dirs.hasNext() && dqfs.hasNext()) {
            lat = lats.nextDouble();
            lon = lons.nextDouble();
            speed = spds.nextFloat();
            direction = dirs.nextFloat();
            quality = dqfs.nextByte();

            if (quality == VALID_DQF) {
                record = new DMWRecord();
                record.setScene(scene);
                record.setChannel(channel);
                record.setOrbitalSlot(orbitalSlot);

                location = new SurfaceObsLocation();
                location.assignLocation((float) lat, (float) lon);
                location.generateCoordinateStationId();
                record.setLocation(location);

                record.setWindSpd(Float.valueOf(speed));
                record.setWindDir(Float.valueOf(direction));

                record.setDataTime(new DataTime(datetime));

                records.add(record);
            }
        }
    }

    /**
     * @param dataFile
     *            The NetCDF File
     * @return The mid-point between the start and end image scan.
     * @throws Exception
     *             If the dates are invalid.
     */
    private Calendar getDateTime(NetcdfFile dataFile, String traceId) throws Exception {
        /*
         * TODO: Using the time variable doesn't match the coverage start/end
         * dates. Not sure if I'm wrong or it's the data. Also, some time values
         * are not filled, so for now I'm parsing the coverage date attributes
         */
        // Variable timeVar = dataFile.findVariable("time");
        // double timeOffset = timeVar.readScalarDouble();
        // int timeSec = (int) (timeOffset);

        // Calendar date = (Calendar) epoch.clone();
        // date.add(Calendar.SECOND, timeSec);
        // date.add(Calendar.MILLISECOND, (int) ((timeOffset - timeSec) *
        // 1000));

        // return date;

        Calendar date;

        SimpleDateFormat sdf = new SimpleDateFormat(COVERAGE_FORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        Attribute startAttr = dataFile
                .findGlobalAttribute("time_coverage_start");
        Attribute endAttr = dataFile.findGlobalAttribute("time_coverage_end");

        Date start;
        Date end;
        if (startAttr != null && endAttr != null) {
            start = sdf.parse(startAttr.getStringValue());
            end = sdf.parse(endAttr.getStringValue());

            Calendar startCal = (Calendar) epoch.clone();
            startCal.setTime(start);
            Calendar endCal = (Calendar) epoch.clone();
            endCal.setTime(end);

            long midpointMs = (startCal.getTimeInMillis() + endCal.getTimeInMillis()) / 2;

            date = (Calendar) epoch.clone();
            date.setTimeInMillis(midpointMs);
        } else if (endAttr != null) {
            end = sdf.parse(endAttr.getStringValue());
            date = (Calendar) epoch.clone();
            date.setTime(end);
            statusHandler.warn(traceId
                    + " - Using coverage end date as reftime.");
        } else if (startAttr != null) {
            start = sdf.parse(startAttr.getStringValue());
            date = (Calendar) epoch.clone();
            date.setTime(start);
            statusHandler.warn(traceId
                    + " - Using coverage start date as reftime.");
        } else {
            throw new IllegalArgumentException("Coverage period not found.");
        }

        return date;
    }
}
