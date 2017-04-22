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
package com.raytheon.edex.plugin.bufrua;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ucar.ma2.StructureDataIterator;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Sequence;
import ucar.nc2.Variable;

import com.raytheon.edex.plugin.bufrua.dao.BufrUADao;
import com.raytheon.edex.plugin.bufrua.decoder.BufrStructure;
import com.raytheon.edex.plugin.bufrua.decoder.BufrStructureIterator;
import com.raytheon.edex.plugin.bufrua.decoder.BufruaLevelDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.bufrua.LayerTools;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * 
 * Decode bufrua data. This decoder handles data following 3 distinct templates.
 * 
 * 1) B/C25 Regulations for reporting TEMP, TEMP SHIP and TEMP MOBIL data in
 * TDCF. This template is extensively documented by the WMO and details can be
 * found at their website
 * http://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI2/TemplateExamples.html
 * 
 * 2) B/C20 Regulations for reporting PILOT, PILOT SHIP and PILOT MOBIL data in
 * TDCF. This template is also documented by the WMO and is very similar to
 * B/C25 and the WMO documentation is available from the same website.
 * 
 * 3) BUFR encoded Text Upper Air(TUA) format. This format contains the same
 * information as the text products it is created from(TEMP and PILOT reports).
 * There does not seem to be any formal documentation on this format so the
 * decoder is based off the BUFR metadata with some modifications based off
 * previous decoders.
 * 
 * The three formats are very similar, the major difference is that TUA encodes
 * a single sounding in 6 records and the WMO data may arrive in 2 records. The
 * six TUA records correspond to the 6 text product types: TTAA TTBB TTCC TTDD
 * PPBB and PPDD. The WMO products send all data in a single BUFR message when
 * the radiosonde reaches 100MB. A second message may be sent after data
 * collection is complete which will contain all the data, including any data
 * above 100MB. To make the WMO product compatible with TUA data the WMO record
 * is split into separate messages to match the message types of the TUA data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jul 12, 2016  5736     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class BufrUADecoder {

    private static final Pattern COR_PTRN = Pattern
            .compile("^(CC[A-Z])($| ).*");

    private static final Pattern LEGACY_TTAAII = Pattern
            .compile("^IUS((Z[0-9])|(Y4))[123468]$");

    /* BUFR descriptor 0 01 011 */
    private static final String BUFR_STATION_ID = "Ship or mobile land station identifier";

    /* BUFR descriptor 0 05 001 */
    private static final String BUFR_LONGITUDE = "Longitude (high accuracy)";

    /* BUFR descriptor 0 06 001 */
    private static final String BUFR_LATITUDE = "Latitude (high accuracy)";

    /* BUFR descriptor 0 07 030 */
    private static final String BUFR_ELEVATION = "Height of station ground above mean sea level";

    /* BUFR descriptor 0 01 001 */
    private static final String BUFR_WMO_BLOCK_NUMBER = "WMO block number";

    /* BUFR descriptor 0 01 002 */
    private static final String BUFR_WMO_STATION_NUMBER = "WMO station number";

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    private final BufrUADao dao;

    public BufrUADecoder() throws PluginException {
        this("bufrua");
    }

    public BufrUADecoder(String pluginName) throws PluginException {
        dao = new BufrUADao(pluginName);
    }

    public PluginDataObject[] decode(File bufrFile) {
        byte[] wmoData = new byte[100];
        try (InputStream is = new FileInputStream(bufrFile)) {
            is.read(wmoData);
        } catch (IOException e) {
            logger.error(
                    "Discarding bufrua data due to an unexpected IO problems while processing "
                            + bufrFile.getName(), e);
            return new PluginDataObject[0];
        }
        WMOHeader wmoHeader = new WMOHeader(wmoData, bufrFile.getName());
        NetcdfFile ncfile = null;
        try {
            ncfile = NetcdfFile.open(bufrFile.getAbsolutePath());
            Variable obsVariable = ncfile.findVariable("obs");
            if (!(obsVariable instanceof Sequence)) {
                logger.error("Discarding bufrua data that does not contain an 'obs' Sequence: "
                        + bufrFile.getName());
                return new PluginDataObject[0];
            }
            Sequence obsSequence = (Sequence) obsVariable;

            StructureDataIterator obsDataIterator = obsSequence
                    .getStructureIterator();
            BufrStructureIterator obsIterator = new BufrStructureIterator(
                    obsSequence, obsDataIterator);

            Map<File, PointDataContainer> containerMap = new HashMap<>();
            List<UAObs> resultList = new ArrayList<>();
            while (obsIterator.hasNext()) {
                BufrStructure obsData = obsIterator.next();
                try {
                    UAObs uaObs = decodeObs(wmoHeader, containerMap, obsData);
                    if (uaObs != null) {
                        resultList.addAll(split(containerMap, uaObs));
                    }
                } catch (Exception e) {
                    logger.error("Bad Data encontered while processing "
                            + bufrFile.getName(), e);
                }
            }
            return resultList.toArray(new PluginDataObject[0]);
        } catch (IOException e) {
            logger.error(
                    "Discarding bufrua data due to an unexpected IO problems while processing "
                            + bufrFile.getName(), e);
            return new PluginDataObject[0];
        } finally {
            if (ncfile != null) {
                try {
                    ncfile.close();
                } catch (IOException e) {
                    logger.error(
                            "Failed to close bufrua file: "
                                    + bufrFile.getName(), e);
                }
            }
        }
    }

    /**
     * Decode a single UAObs from a {@link BufrStructure}.
     * 
     * @param wmoHeader
     *            The WMOHeader for the entire bufr file.
     * @param containerMap
     *            used to optimize {@link #initializePointDataView(Map, UAObs)}
     * 
     * @param obsStructure
     *            The BufrStrcture which contains the data for the UAObs.
     * @return A newly created UAObs or null if there is not enough data to
     *         create a UAObs.
     * @throws IOException
     *             if there is a problem reading the data from the bufr file.
     */
    protected UAObs decodeObs(WMOHeader wmoHeader,
            Map<File, PointDataContainer> containerMap,
            BufrStructure obsStructure) throws IOException {
        UAObs uaObs = new UAObs();
        uaObs.setCorIndicator(isCor(wmoHeader));
        uaObs.setWmoHeader(wmoHeader.getWmoHeader());

        DataTime time = decodeDataTime(obsStructure);
        if (time == null) {
            return null;
        }
        uaObs.setDataTime(time);
        initializePointDataView(containerMap, uaObs);
        if (!decodeLocation(uaObs, obsStructure)) {
            return null;
        }

        decodeLevels(uaObs, obsStructure);

        processReportType(wmoHeader, uaObs);
        return uaObs;
    }

    /**
     * Attempt to determine and set the reportType for a fully populated UAObs
     * record.
     * 
     * @param wmoHeader
     *            WMOHeader for the bufr file. Some information in the header
     *            can be used to determine report type
     * @param uaObs
     *            The observation that needs a reportType.
     */
    protected void processReportType(WMOHeader wmoHeader, UAObs uaObs) {
        String ttaaii = wmoHeader.getTtaaii();
        if (ttaaii != null && LEGACY_TTAAII.matcher(ttaaii).matches()) {
            int ii = wmoHeader.getIi() % 10;
            if (ii == 1) {
                uaObs.setReportType(LayerTools.MANLVL_LO);
            } else if (ii == 2) {
                uaObs.setReportType(LayerTools.SIGTLVL_LO);
            } else if (ii == 3) {
                uaObs.setReportType(LayerTools.MANLVL_HI);
            } else if (ii == 4) {
                uaObs.setReportType(LayerTools.SIGTLVL_HI);
            } else if (ii == 6) {
                uaObs.setReportType(LayerTools.SIGWLVL_LO);
            } else if (ii == 8) {
                uaObs.setReportType(LayerTools.SIGWLVL_HI);
            }
            return;
        }

        PointDataView view = uaObs.getPointDataView();
        int tempIdx = view.getInt(LayerTools.NUM_SIGT);
        int windIdx = view.getInt(LayerTools.NUM_SIGW);
        int manIdx = view.getInt(LayerTools.NUM_MAND);
        if (manIdx > 0) {
            float pressure = view.getFloat(LayerTools.PR_MAN, 0);
            if (pressure < 10000) {
                uaObs.setReportType(LayerTools.MANLVL_HI);
            } else {
                uaObs.setReportType(LayerTools.MANLVL_LO);
            }
        } else if (windIdx > 0) {
            float height = view.getFloat(LayerTools.HT_SIGW, 0);
            float pressure = view.getFloat(LayerTools.PR_SIGW, 0);
            // The height cutoff is not well defined. Some sources claim it is
            // 50000 feet while others claim it is 53000. We use 50000 feet
            // because any lower data should still be well below that. We store
            // height in meters so 15240 meters is 50000 feet.
            if (height > 15240.0 || pressure >= 10000) {
                uaObs.setReportType(LayerTools.SIGWLVL_HI);
            } else {
                uaObs.setReportType(LayerTools.SIGWLVL_LO);
            }
        } else if (tempIdx > 0) {
            float pressure = view.getFloat(LayerTools.PR_SIGT, 0);
            if (pressure < 10000 && pressure > 0) {
                uaObs.setReportType(LayerTools.SIGTLVL_HI);
            } else {
                uaObs.setReportType(LayerTools.SIGTLVL_LO);
            }
        } else {
            uaObs.setReportType(LayerTools.MANLVL_LO);
        }
    }

    /**
     * Determine if a UAObs record contains data that is applicable to multiple
     * report types and if it is split the data into separate records for each
     * report type. Specifically this is intedended to catch WMO formatted bufr
     * records and split them into the distinct report types that are normally
     * used by TUA.
     * 
     * @param containerMap
     *            used to optimize {@link #initializePointDataView(Map, UAObs)}
     * @param uaObs
     *            an observation that may need to be split.
     * @return A list of UAObs which will contain all the records split from the
     *         input record. If no split is necessary then the list just
     *         contains the input record.
     */
    protected List<UAObs> split(Map<File, PointDataContainer> containerMap,
            UAObs uaObs) {
        int reportType = uaObs.getReportType();
        if (reportType != LayerTools.MANLVL_LO
                && reportType != LayerTools.MANLVL_HI) {
            return Collections.singletonList(uaObs);
        }

        PointDataView view = uaObs.getPointDataView();
        int tempIdx = view.getInt(LayerTools.NUM_SIGT);
        int windIdx = view.getInt(LayerTools.NUM_SIGW);

        List<UAObs> result = new ArrayList<>(3);

        result.add(uaObs);

        if (tempIdx > 0) {
            UAObs tempObs = new UAObs();
            tempObs.setCorIndicator(uaObs.getCorIndicator());
            tempObs.setDataTime(uaObs.getDataTime());
            tempObs.setLocation(uaObs.getLocation());
            tempObs.setStationName(uaObs.getStationName());
            tempObs.setWmoHeader(uaObs.getWmoHeader());
            if (uaObs.getReportType().equals(LayerTools.MANLVL_LO)) {
                tempObs.setReportType(LayerTools.SIGTLVL_LO);
            } else {
                tempObs.setReportType(LayerTools.SIGTLVL_HI);
            }
            initializePointDataView(containerMap, tempObs);
            PointDataView tempView = tempObs.getPointDataView();
            for (int i = 0; i < tempIdx; i += 1) {
                tempView.setFloat(LayerTools.PR_SIGT,
                        view.getFloat(LayerTools.PR_SIGT, i), i);
                tempView.setFloat(LayerTools.TP_SIGT,
                        view.getFloat(LayerTools.TP_SIGT, i), i);
                tempView.setFloat(LayerTools.TD_SIGT,
                        view.getFloat(LayerTools.TD_SIGT, i), i);
            }
            tempView.setInt(LayerTools.NUM_SIGT, tempIdx);

            result.add(tempObs);
        }

        if (windIdx > 0) {
            UAObs windObs = new UAObs();
            windObs.setCorIndicator(uaObs.getCorIndicator());
            windObs.setDataTime(uaObs.getDataTime());
            windObs.setLocation(uaObs.getLocation());
            windObs.setStationName(uaObs.getStationName());
            windObs.setWmoHeader(uaObs.getWmoHeader());
            if (uaObs.getReportType().equals(LayerTools.MANLVL_LO)) {
                windObs.setReportType(LayerTools.SIGWLVL_LO);
            } else {
                windObs.setReportType(LayerTools.SIGWLVL_HI);
            }
            initializePointDataView(containerMap, windObs);
            PointDataView windView = windObs.getPointDataView();
            for (int i = 0; i < windIdx; i += 1) {
                windView.setFloat(LayerTools.PR_SIGW,
                        view.getFloat(LayerTools.PR_SIGW, i), i);
                windView.setFloat(LayerTools.HT_SIGW,
                        view.getFloat(LayerTools.HT_SIGW, i), i);
                windView.setFloat(LayerTools.WD_SIGW,
                        view.getFloat(LayerTools.WD_SIGW, i), i);
                windView.setFloat(LayerTools.WS_SIGW,
                        view.getFloat(LayerTools.WS_SIGW, i), i);
            }
            windView.setInt(LayerTools.NUM_SIGW, windIdx);
            result.add(windObs);
        }

        return result;

    }

    /**
     * Iterate over the levels in a {@link BufrStructure} and add the data to an
     * {@link UAObs}.
     * 
     * @param uaObs
     *            the obs where the levels should be stored
     * @param obsStructure
     *            the structure containing the information for the levels.
     * @throws IOException
     *             if there is a problem reading the data from the bufr file.
     */
    protected void decodeLevels(UAObs uaObs, BufrStructure obsStructure)
            throws IOException {
        BufrStructureIterator struct1 = obsStructure
                .getStructureIterator("struct1");
        /*
         * TUA records encode the levels as 2 structs but B/C25 and B/C20 data
         * encodes the data as 2 sequences.
         */
        if (struct1 == null) {
            BufrStructureIterator seq1 = obsStructure
                    .getSequenceIterator("seq1");
            while (seq1.hasNext()) {
                BufruaLevelDecoder.decodeLevel(uaObs, seq1.next());
            }
            /*
             * The second sequence only contains wind shear data, which is
             * currently not used.
             */
        } else {
            while (struct1.hasNext()) {
                BufruaLevelDecoder.decodeLevel(uaObs, struct1.next());
            }

            /*
             * TUA data comes in 3 flavors
             * 
             * 1) Mandatory records contain SURFACE, STANDARD, and TROPOPAUSE
             * data in the first struct and MAX_WIND data in the second struct.
             * For this data the second struct is important.
             * 
             * 2) SigT records contains SIGNIFICANT_TEMPERATURE data in the
             * first struct and the second struct is marked as
             * SIGNIFICANT_TEMPERATURE but it uses a descriptor for wind on a
             * pressure level and does not contain any valid temperature data.
             * The previous version of the decoder ignored the second struct
             * which seems reasonable considering it's unusable unless you
             * ignore the significance.
             * 
             * 3) SigW records contain SIGNIFICANT_WIND data at heights in the
             * first struct and SIGNIFICANT_WIND data at pressure levels in the
             * second struct. The previous version of the decoder ignored the
             * second struct so this decoder also does. It may be reasonable in
             * the future to validate the data is useable and then enable the
             * second struct.
             */
            if (uaObs.getPointDataView().getInt(LayerTools.NUM_MAND) == 0) {
                return;
            }

            BufrStructureIterator struct2 = obsStructure
                    .getStructureIterator("struct2");
            while (struct2.hasNext()) {
                BufruaLevelDecoder.decodeLevel(uaObs, struct2.next());
            }
        }
    }

    /**
     * Create and set a new {@link PointDataView} for the provided {@link UAObs}
     * 
     * @param containerMap
     *            A map containing any existing {@link PointDataContainer}s that
     *            have been created while processing this bufr file. This allows
     *            multiple UAObs to share the same container which improves the
     *            performance of persist.
     * @param uaObs
     *            The Obs that the view will be created for.
     */
    protected void initializePointDataView(
            Map<File, PointDataContainer> containerMap, UAObs uaObs) {
        File file = dao.getFullFilePath(uaObs);
        PointDataContainer container = containerMap.get(file);
        if (container == null) {
            container = PointDataContainer.build(dao
                    .getPointDataDescription(null));
            containerMap.put(file, container);
        }
        PointDataView view = container.append();
        view.setInt(LayerTools.NUM_MAND, 0);
        view.setInt(LayerTools.NUM_TROP, 0);
        view.setInt(LayerTools.NUM_MWND, 0);
        view.setInt(LayerTools.NUM_SIGW, 0);
        view.setInt(LayerTools.NUM_SIGT, 0);
        uaObs.setPointDataView(view);
    }

    /**
     * Extract location information from a {@link BufrStructure} and put it in a
     * {@link UAObs}. Some of the information is set in a
     * {@link SurfaceObsLocation} and a few parameters are also stored in the
     * PointDataView.
     * 
     * @param uaObs
     *            the UAObs to put location data in
     * @param obsStructure
     *            the bufr structure to pull data from
     * @return true if the location information was successfully decoded. false
     *         if there wasn't enough information, in this case an error will be
     *         logged.
     */
    protected boolean decodeLocation(UAObs uaObs, BufrStructure obsStructure) {
        Integer wmoStaId = getWMOStationId(obsStructure);
        String stationId = null;
        if (wmoStaId != null) {
            uaObs.getPointDataView().setInt("wmoStaNum", wmoStaId);
            stationId = String.format("%05d", wmoStaId);
            queryStationInfo(uaObs, stationId);
        }
        if (!uaObs.getLocation().getLocationDefined()) {
            String stationName = obsStructure
                    .lookupStringValue(BUFR_STATION_ID);
            if (stationId == null && stationName == null) {
                logger.error("Bufrua data does not contain a valid station identifier");
                return false;
            } else if (stationId == null) {
                stationId = stationName;
            } else if (stationName == null) {
                stationName = stationId;
            }
            Number longitude = obsStructure.lookupNumericValue(BUFR_LONGITUDE);
            Number latitude = obsStructure.lookupNumericValue(BUFR_LATITUDE);
            if (longitude == null || latitude == null) {
                StringBuilder error = new StringBuilder(
                        "Bufrua data does not contain a valid station location for ");
                error.append(stationName);
                if (!stationId.equals(stationName)) {
                    error.append('(').append(stationId).append(')');
                }
                logger.error(error.toString());
                return false;
            }
            Number elevation = obsStructure.lookupNumericValue(BUFR_ELEVATION);
            SurfaceObsLocation location = new SurfaceObsLocation(stationId);
            location.setLongitude(longitude.floatValue());
            location.setLatitude(latitude.floatValue());
            if (elevation != null) {
                location.setElevation(elevation.intValue());
            }
            uaObs.getPointDataView().setString("staName", stationName);
            uaObs.setStationName(stationName);
            uaObs.setLocation(location);
        }
        return true;
    }

    /**
     * Query the common_obs_spatial data for information about the upper air
     * station with the provided stationId. Some BUFR files do not encode much
     * location information so it is necessary to look it up in the database. If
     * the query succeeds then the {@link UAObs} is populated with the
     * applicable location information. If the station isn't found in the
     * database or isn't valid then a warning is logged and the location is left
     * uninitialized.
     * 
     * @param uaObs
     *            The obs to set the location in.
     * @param staId
     *            The stationId to query for.
     * @see #getWMOStationId(BufrStructure)
     */
    protected void queryStationInfo(UAObs uaObs, String staId) {
        ObStationDao stationDao = new ObStationDao();
        String gid = ObStation.createGID(ObStation.CAT_TYPE_SFC_RAOB, staId);
        ObStation station = null;
        try {
            station = stationDao.queryByGid(gid);
        } catch (DataAccessLayerException e) {
            logger.warn("Station database query failed for " + gid, e);
            return;
        }
        if (station == null) {
            logger.warn("No station information was found in the database for "
                    + gid);
            return;
        }
        SurfaceObsLocation location = new SurfaceObsLocation(staId);
        location.setGeometry(station.getUpperAirGeometry());
        location.setElevation(station.getUpperAirElevation());
        location.setLocationDefined(true);

        if (location.getLongitude() == null || location.getLatitude() == null) {
            logger.warn("Invalid location in the database for station " + gid);
            return;
        }

        String stationName = station.getIcao();
        if (stationName == null || stationName.length() < 4) {
            stationName = staId;
        }
        uaObs.getPointDataView().setString("staName", stationName);
        uaObs.setStationName(stationName);
        uaObs.setLocation(location);
    }


    /**
     * Decode the time information. If there is no valid time information then
     * an error is logged and null is returned.
     * 
     * @param obsStructure
     *            the structure containing information about the observation.
     * @return a DataTime, or null if the data does not contain a valid time.
     */
    protected DataTime decodeDataTime(BufrStructure obsStructure) {
        Number year = obsStructure.lookupNumericValue("Year");
        Number month = obsStructure.lookupNumericValue("Month");
        Number day = obsStructure.lookupNumericValue("Day");
        Number hour = obsStructure.lookupNumericValue("Hour");
        Number minute = obsStructure.lookupNumericValue("Minute");
        Number second = obsStructure.lookupNumericValue("Second");

        if (year == null) {
            logger.error("Obs discarded because year is missing.");
            return null;
        } else if (year.intValue() < 100) {
            /*
             * Handle 2 digit year, data is assumed to be in the 100 year period
             * starting 90 years ago up until 10 years in the future.
             */
            int currentYear = TimeUtil.newGmtCalendar().get(Calendar.YEAR);
            int century = (currentYear - year.intValue() + 10) / 100;
            year = century * 100 + year.intValue();
        }
        if (month == null) {
            logger.error("Obs discarded because month is missing.");
            return null;
        } else if (day == null) {
            logger.error("Obs discarded because day is missing.");
            return null;
        } else if (hour == null) {
            logger.error("Obs discarded because hour is missing.");
            return null;
        } else if (minute == null) {
            logger.error("Obs discarded because minute is missing.");
            return null;
        }

        Calendar time = TimeUtil.newGmtCalendar(year.intValue(),
                month.intValue(), day.intValue());
        time.set(Calendar.HOUR_OF_DAY, hour.intValue());
        time.set(Calendar.MINUTE, minute.intValue());
        if (second != null) {
            time.set(Calendar.SECOND, second.intValue());
        }

        Calendar maxFutureTime = Calendar.getInstance();
        maxFutureTime.add(Calendar.HOUR, 12);
        if (time.after(maxFutureTime)) {
            logger.error("Obs discarded because " + time.getTime()
                    + " is too far in the future.");
            return null;
        }

        return new DataTime(time);
    }

    /**
     * Determine if a WMO header indicates the message is a correction to a
     * previous message.
     * 
     * @param header
     *            a WMOHeader to process
     * @return The COR indicator from the WMO header if there is one or null if
     *         the message is not a correction.
     */
    protected static String isCor(WMOHeader header) {
        String cor = null;
        if (header != null) {
            String bbb = header.getBBBIndicator();
            if (bbb != null) {
                Matcher m = COR_PTRN.matcher(bbb);
                if (m.find()) {
                    cor = m.group(1);
                }
            }
        }
        return cor;
    }

    /**
     * Return the WMO station number for an observation. This will return null
     * if the number is not specified, which is expected for ship or mobile land
     * stations.
     * 
     * @param obsStructure
     *            The {@link BufrStructure} containing data for the observation
     * @return A unique integer for the station or null if there is no station
     *         id.
     */
    protected static Integer getWMOStationId(BufrStructure obsStructure) {
        Number block = obsStructure.lookupNumericValue(BUFR_WMO_BLOCK_NUMBER);
        Number station = obsStructure
                .lookupNumericValue(BUFR_WMO_STATION_NUMBER);
        if (block != null && station != null) {
            return (block.intValue() * 1000) + station.intValue();
        } else {
            return null;
        }
    }


}
