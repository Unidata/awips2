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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXB;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.goesr.dmw.DMWRecord;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.plugin.goesr.dmw.description.ProductDescription;
import com.raytheon.uf.edex.plugin.goesr.dmw.description.ProductDescriptions;

import ucar.ma2.Array;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

/**
 * Decoder for Derived Motion Wind products. Updated to handle both GOES-R and
 * Himawari DMWs through Product Descriptions files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2015  4334       nabowle     Initial creation
 * Sep 28, 2015 4872       bsteffen    Decode File instead of byte[]
 * July 13, 2016  19051     mcomerford      Enhanced DMW plugin update (DCS 19051)
 * 
 * </pre>
 * 
 * @author nabowle
 * @version 1.0
 */

public class DMWDecoder {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DMWDecoder.class);

    /*
     * Used to parse the ProductDescription-defined epoch (handling the
     * dataTime).
     */
    private static final String DATE_STRING = "yyyy-MM-dd HH:mm:ss";

    /*
     * The ProductDescriptions that will be loaded from the description file.
     */
    private ProductDescriptions descriptions;

    /**
     * Default constructor.
     */
    public DMWDecoder() {
        super();
    }

    /**
     * Decode a GOES-R/Himawari DMW file.
     *
     * @param data
     *            The file data.
     * @param headers
     *            The headers.
     * @return The decoded objects.
     */
    public PluginDataObject[] decode(File file, Headers headers) {

        String traceId = "";

        List<PluginDataObject> records = new ArrayList<>();

        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        try {
            if (file != null && file.length() > 0) {
                try {
                    decodeData(file, traceId, records);
                } catch (Exception e) {
                    statusHandler.error(traceId + "-Error in decode", e);
                } finally {
                    if (records.isEmpty()) {
                        statusHandler.info(
                                String.format("%s - Decoded no obs", traceId));
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
     * @param file
     *            The file.
     * @param traceId
     *            The traceId.
     * @param records
     *            The list to add valid records to.
     */
    private void decodeData(File file, String traceId,
            List<PluginDataObject> records) throws IOException {

        NetcdfFile dataFile = NetcdfFile.open(file.getAbsolutePath());

        /* Number of descriptions that have generated legimate records. */
        int descMatch = 0;

        /*
         * Loop through the Product Descriptions file(s) to generate DMWRecords.
         */
        for (ProductDescription description : descriptions.getDescriptions()) {
            try {
                processDescription(description, dataFile, records);
                /*
                 * If we make it here, then the description was valid (although
                 * no records may have been generated).
                 */
                descMatch++;
            } catch (InvalidDescriptionException e) {
                if (description.isDebugStatus()) {
                    statusHandler.info(
                            "ProductDescription \"" + description.getName()
                                    + "\" -- " + e.getMessage());
                }
            } catch (Exception e) {
                /* Any Exceptions not thrown as InvalidDescriptionExceptions. */
                statusHandler
                        .info("An uncaught error occurred while processing description "
                                + description.getName() + ": "
                                + e.getMessage());
            }
        }

        dataFile.close();

        if (descMatch == 0) {
            statusHandler
                    .info("No descriptions provided are valid for the file "
                            + file.getName());
        }
    }

    /**
     * Processes a ProductDescription to decode file and populate the records
     * list
     *
     * @param description
     *            instance being processed.
     * @param file
     *            The file to be decoded.
     * @param records
     *            The list of PluginDataObject to populate w/ valid obs.
     * @throws Exception
     *             To log the which product description throws the error and
     *             where it is thrown.
     */
    private void processDescription(ProductDescription description,
            NetcdfFile dataFile, List<PluginDataObject> records)
                    throws IOException, InvalidDescriptionException {

        // Date handling
        String epochStr = description.getDataTime().getEpoch();
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_STRING);
        Date timeDate;
        try {
            timeDate = sdf.parse(epochStr);
        } catch (ParseException e) {
            throw new InvalidDescriptionException(
                    "The \"epoch\" attribute of the \"<dateTime>\" bean must be of format \"yyyy-MM-dd HH:mm:ss\".");
        }
        Calendar epochCal = TimeUtil.newGmtCalendar(timeDate);

        /* Initialize the variable(s) given the ProductDescriptions. */
        Variable latVar, lonVar, spdVar, dirVar, dqfVar, filVar, epochOffsetVar;
        try {
            latVar = dataFile.findVariable(description.getLat().getName());
            lonVar = dataFile.findVariable(description.getLon().getName());
            spdVar = dataFile.findVariable(description.getWspd().getName());
            dirVar = dataFile.findVariable(description.getWdir().getName());
            dqfVar = dataFile.findVariable(description.getDQF().getName());
            filVar = dataFile.findVariable(description.getFilter().getName());
            epochOffsetVar = dataFile.findVariable(
                    description.getDataTime().getDelegate().getName());
        } catch (Exception e) {
            throw new InvalidDescriptionException(
                    "An error occurred while assigning the NetCDF Variables");
        }

        /* Quick check to see if a valid winds product may exist. */
        if (latVar == null || lonVar == null || spdVar == null || dirVar == null
                || dqfVar == null) {
            throw new InvalidDescriptionException(
                    "The Product Description could not load the necessary Variables from the file.");
        }

        /*
         * Determine the percentage of valid DQF's in the file (no DMW Records
         * are created if this is present in the description and its value is <=
         * 0 in the NetCDF File.
         */
        float percentGoodDQFVal = 0.0f;
        if (description.getPercentGoodDQF() != null) {
            percentGoodDQFVal = description.getPercentGoodDQF().getNumber(dataFile).floatValue();

            if (percentGoodDQFVal <= 0.0f) {
                throw new InvalidDescriptionException(
                        "\"<percentGoodDQF>\" field shows no valid winds in the file");
            }
        }

        String scene = "";
        if (description.getScene() != null) {
            scene = description.getScene().getString(dataFile);
        }

        String orbitalSlot = "";
        if (description.getOrbitalSlot() != null) {
            orbitalSlot = description.getOrbitalSlot().getString(dataFile);
        }

        int channel = dataFile.findVariable(description.getChannel().getName())
                .readScalarInt();

        Array lats = latVar.read();
        Array lons = lonVar.read();
        Array spds = spdVar.read();
        Array dirs = dirVar.read();
        Array dqfs = dqfVar.read();
        Array filters = filVar.read();

        long latsSize = lats.getSize();
        if (latsSize != lons.getSize() || latsSize != spds.getSize()
                || latsSize != dirs.getSize() || latsSize != dqfs.getSize()) {
            throw new InvalidDescriptionException(
                    "The required data arrays within the file are not of the same length");
        }

        /*
         * Handle setting the epoch/timestamp of the data (GOES-R : Scalar,
         * Himawari : Array).
         */
        int epochOffsetVal = 0;
        Array epochOffsets = null;
        if (epochOffsetVar.isScalar()) {
            epochOffsetVal = epochOffsetVar.readScalarInt();
        } else {
            epochOffsets = epochOffsetVar.read();
        }

        double lat;
        double lon;
        float speed;
        float direction;
        byte quality;
        DMWRecord record;
        SurfaceObsLocation location;
        Float filter;

        while (lats.hasNext() && lons.hasNext() && spds.hasNext()
                && dirs.hasNext() && dqfs.hasNext()) {

            lat = lats.nextDouble();
            lon = lons.nextDouble();
            speed = spds.nextFloat();
            direction = dirs.nextFloat();
            quality = dqfs.nextByte();

            /*
             * Get the next float in the "filters" Array. If there is no next
             * float in the Array, then default the value.
             */
            if (filters.hasNext()) {
                try {
                    filter = filters.nextFloat();
                } catch (Exception e) {
                    filter = (Float) null;
                }
            } else {
                filter = (Float) null;
            }

            /* Now to do the same only for the epoch. */
            if (!epochOffsetVar.isScalar()) {
                if (epochOffsets.hasNext()) {
                    try {
                        Number epochOffset = (Number) epochOffsets.next();
                        epochOffsetVal = epochOffset.intValue();
                    } catch (Exception e) {
                        throw new InvalidDescriptionException(
                                "An error occurred while decoding \"<dataTime> <variable>\" field; ",
                                e);
                    }
                } else {
                    throw new InvalidDescriptionException(
                            "Uncaught error occurred while decoding \"<dataTime>\" field");
                }
            }

            /* Only create records for the description's valid DQF variable. */
            if (quality == description.getValidDQF()) {

                record = new DMWRecord();

                record.setScene(scene);
                record.setChannel(channel);
                record.setOrbitalSlot(orbitalSlot);

                location = new SurfaceObsLocation();
                location.assignLocation((float) lat, (float) lon);
                location.generateCoordinateStationId();
                record.setLocation(location);

                record.setWindSpd(speed);
                record.setWindDir(direction);

                record.setFilter(filter);

                Calendar datetime = (Calendar) epochCal.clone();
                datetime.add(Calendar.SECOND, epochOffsetVal);
                record.setDataTime(new DataTime(datetime));

                records.add(record);
            }
        }
    }

    /**
     * The {@link IPathManager} is used to look up descriptions files.
     */
    public void setPathManager(IPathManager pathManager) {
        LocalizationFile[] files = pathManager.listStaticFiles(
                "satellite/dmw/descriptions", new String[] { ".xml" }, true,
                true);
        ProductDescriptions descriptions = new ProductDescriptions();
        for (LocalizationFile file : files) {
            statusHandler.info(
                    "Loading DMW data description(s) from " + file.getPath());

            try (InputStream inputStream = file.openInputStream()) {
                ProductDescriptions unmarshalled = JAXB.unmarshal(inputStream,
                        ProductDescriptions.class);
                descriptions.addDescriptions(unmarshalled);
            } catch (LocalizationException | IOException e) {
                statusHandler.error("Unable to load product descriptions from "
                        + file.getPath(), e);
            }
        }
        this.descriptions = descriptions;
    }
}
