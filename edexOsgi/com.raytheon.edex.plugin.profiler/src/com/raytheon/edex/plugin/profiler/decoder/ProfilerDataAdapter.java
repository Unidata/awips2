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
package com.raytheon.edex.plugin.profiler.decoder;

import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.RepSubList;

import java.io.File;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.profiler.ProfilerObs;
import com.raytheon.uf.common.dataplugin.profiler.ProfilerSite;
import com.raytheon.uf.common.dataplugin.profiler.Profilers;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * This class contains several utility methods that construct a ProfilerObs
 * instance from the BUFR decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 03, 2008  969      jkorman     Initial implementation.
 * May 09, 2013  1869     bsteffen    Modified D2D time series of point data to
 *                                    work without dataURI.
 * Dec 03, 2013  2537     bsteffen    Switch logger to ufstatus.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class ProfilerDataAdapter {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(ProfilerDataAdapter.class);

    private static final String PROFILER_SITES = "profilerSites.xml";

    private static Profilers profilers = populateProfilerData();

    /**
     * Construct a ProfilerObs instance from the BUFR decoded data contained in
     * the specified separator.
     * 
     * @param iterator
     *            An iterator containing decoded BUFR data.
     * @param wmoHeader
     *            a wmoHeader
     * @return A ProfilerObs instance, or null in the event of an error.
     */
    @SuppressWarnings("unchecked")
    public static ProfilerObs createProfilerData(
            Iterator<BUFRDataDocument> iterator, WMOHeader wmoHeader,
            PointDataContainer container, String traceId) {

        int stationHeight = IDecoderConstants.VAL_MISSING;

        ProfilerObs obsData = null;

        BUFRDataDocument dataDoc = iterator.next();
        if (dataDoc != null) {
            // Get the primary data list.
            List<IBUFRDataPacket> dataList = dataDoc.getList();
            PointDataView view = container.append();
            // Extract the header data.
            obsData = getHeaderData(dataList, view, traceId);
            if (obsData != null) {
                obsData.setWmoHeader(wmoHeader.getWmoHeader());

                // Station height will be incremented by all height increments
                // currently in force.
                stationHeight = obsData.getElevation();

                boolean haveRequiredData = (obsData.getStationId() != null);

                // If we don't have required data, null out the instance.
                if (haveRequiredData) {
                    int levelHeight = stationHeight;
                    Integer hIncrement = 0;

                    levelHeight += getHeightIncrement(dataList, 18);

                    hIncrement = getHeightIncrement(dataList, 20);

                    int index = 0;
                    // get the replication sublist
                    IBUFRDataPacket p = dataList.get(21);
                    if ((p instanceof BUFRSublistPacket)
                            && (RepSubList.getPacketType().equals(p.getUnits()))) {

                        List<IBUFRDataPacket> subList = (List<IBUFRDataPacket>) p
                                .getValue();

                        for (IBUFRDataPacket pList : subList) {
                            List<IBUFRDataPacket> sList = (List<IBUFRDataPacket>) pList
                                    .getValue();

                            levelHeight += hIncrement;
                            if (createLevel(sList, levelHeight, view, index)) {
                                index++;
                            }
                        }
                    }

                    hIncrement = getHeightIncrement(dataList, 22);
                    p = dataList.get(23);
                    if ((p instanceof BUFRSublistPacket)
                            && (RepSubList.getPacketType().equals(p.getUnits()))) {
                        List<IBUFRDataPacket> subList = (List<IBUFRDataPacket>) p
                                .getValue();

                        for (IBUFRDataPacket pList : subList) {
                            List<IBUFRDataPacket> sList = (List<IBUFRDataPacket>) pList
                                    .getValue();
                            levelHeight += hIncrement;
                            if (createLevel(sList, levelHeight, view, index)) {
                                index++;
                            }
                        }
                    }
                    view.setInt("numProfLvls", index);
                    obsData.setPointDataView(view);
                } else {
                    obsData = null;
                }
            }
        }
        return obsData;
    }

    /**
     * Extract all header data from the "main" observation list.
     * 
     * @param dataList
     *            List of data packets to get data from.
     * @return The ProfilerObs with primary data populated.
     */
    private static ProfilerObs getHeaderData(List<IBUFRDataPacket> dataList,
            PointDataView view, String traceId) {

        int wmoBlock = IDecoderConstants.VAL_MISSING;
        int wmoStation = IDecoderConstants.VAL_MISSING;
        int stationHeight = IDecoderConstants.VAL_MISSING;
        Double lat = IDecoderConstants.VAL_MISSING.doubleValue();
        Double lon = IDecoderConstants.VAL_MISSING.doubleValue();

        ProfilerObs obsData = null;

        if (dataList != null) {

            obsData = new ProfilerObs();
            obsData.setReportType(IDecoderConstants.PROFILER_DATA);
            SurfaceObsLocation location = new SurfaceObsLocation();

            lat = getDouble(dataList.get(2), IDecoderConstants.VAL_MISSING);
            lon = getDouble(dataList.get(3), IDecoderConstants.VAL_MISSING);

            stationHeight = getInt(dataList.get(4),
                    IDecoderConstants.VAL_MISSING);
            setViewData("windSpeedSfc", view, dataList.get(12));
            setViewData("windDirSfc", view, dataList.get(13));
            setViewData("pressure", view, dataList.get(14));
            setViewData("temperature", view, dataList.get(15));
            setViewData("rainRate", view, dataList.get(16));
            setViewData("relHumidity", view, dataList.get(17));
            setViewData("submode", view, dataList.get(19));

            wmoBlock = ((Long) dataList.get(0).getValue()).intValue();
            wmoStation = ((Long) dataList.get(1).getValue()).intValue();
            int wmoBSN = createWMOId(wmoBlock, wmoStation);
            if (wmoBSN >= 0) {

                ProfilerSite site = profilers.get(wmoBSN);
                if (site != null) {
                    location.setStationId(site.getStationId());
                    location.assignLocation(lat, lon);
                    location.setLocationDefined(false);
                    if (stationHeight != IDecoderConstants.VAL_MISSING) {
                        location.setElevation(stationHeight);
                    } else {
                        location.setElevation(site.getElevation().intValue());
                    }

                    obsData.setProfilerId(site.getProfilerId());

                    view.setString("profilerName", site.getProfilerName());
                    obsData.setLocation(location);
                    Calendar baseTime = getTimeInfo(dataList);
                    if (baseTime != null) {
                        obsData.setTimeObs(TimeTools.copy(baseTime));
                        DataTime dt = new DataTime(TimeTools.copy(baseTime));
                        obsData.setDataTime(dt);
                    } else {
                        logger.error(traceId
                                + "-Time information missing or incorrect");
                        obsData = null;
                    }
                } else {
                    logger.error(traceId
                            + "-Could not find location data for [" + wmoBSN
                            + "]");
                    obsData = null;
                }
            } else {
                logger.error(traceId
                        + "- Could not find WMO Block Station Number");
                obsData = null;
            }
        }

        return obsData;
    }

    /**
     * Creates individual level data from level "sublists" that were decoded.
     * 
     * @param levelList
     *            A sublist containing level data.
     * @param elev
     *            The current elevation to use.
     * @return The populated level data.
     */
    private static boolean createLevel(List<IBUFRDataPacket> levelList,
            Integer elev, PointDataView view, int index) {

        boolean goodLevel = false;
        if (levelList != null) {
            if (levelList.get(0).getValue() != null) {
                view.setInt("height", elev, index);
                setViewData("levelMode", view, levelList.get(0), index);
                setViewData("uvQualityCode", view, levelList.get(1), index);
                setViewData("consensusNum", view, levelList.get(2), index);
                setViewData("uComponent", view, levelList.get(3), index);
                setViewData("vComponent", view, levelList.get(4), index);
                setViewData("HorizSpStdDev", view, levelList.get(5), index);
                setViewData("peakPower", view, levelList.get(7), index);
                setViewData("wComponent", view, levelList.get(8), index);
                setViewData("VertSpStdDev", view, levelList.get(9), index);

                goodLevel = true;
            }
        }
        return goodLevel;
    }

    /**
     * Create a wmo block station number from the block and station numbers
     * specified.
     * 
     * @param wmoBlockNumber
     *            A WMO block number (0..99).
     * @param wmoStationNumber
     *            A WMO station number (0..999).
     * @return The created block station number or IDecoderConstants.VAL_MISSING
     *         if the values failed validation.
     */
    private static int createWMOId(int wmoBlockNumber, int wmoStationNumber) {
        int wmoBSN = IDecoderConstants.VAL_MISSING;

        if ((wmoBlockNumber >= 0) && (wmoBlockNumber < 100)) {
            if ((wmoStationNumber >= 0) && (wmoStationNumber < 1000)) {
                wmoBSN = (wmoBlockNumber * 1000) + wmoStationNumber;
            }
        }
        return wmoBSN;
    }

    private static Calendar getTimeInfo(List<IBUFRDataPacket> dataList) {

        int year = getInt(dataList.get(5), IDecoderConstants.VAL_MISSING);
        ;
        int month = getInt(dataList.get(6), IDecoderConstants.VAL_MISSING);
        ;
        int day = getInt(dataList.get(7), IDecoderConstants.VAL_MISSING);
        ;
        int hour = getInt(dataList.get(8), IDecoderConstants.VAL_MISSING);
        ;
        int minute = getInt(dataList.get(9), IDecoderConstants.VAL_MISSING);
        ;
        // int timeSig = IDecoderConstants.VAL_MISSING;
        // int timeDisplacement =
        // getInt(dataList.get(11),IDecoderConstants.VAL_MISSING);;

        Calendar baseTime = null;

        // Ensure that we have all of the time info and create the
        // date-time and datatime info.
        if ((year > 0) && (month > 0) && (day > 0) && (hour >= 0)) {
            baseTime = TimeTools.getBaseCalendar(year, month, day);
            baseTime.set(Calendar.HOUR_OF_DAY, hour);
            baseTime.set(Calendar.MINUTE, minute);
        }
        return baseTime;
    }

    /**
     * 
     * @param packet
     * @param defaultValue
     * @return
     */
    private static double getDouble(IBUFRDataPacket packet, double defaultValue) {
        double retValue = defaultValue;
        if (packet != null) {
            Object o = packet.getValue();
            if (o instanceof Double) {
                retValue = ((Double) o).doubleValue();
            } else if (o instanceof Long) {
                retValue = ((Long) o).doubleValue();
            }
        }
        return retValue;
    }

    /**
     * 
     * @param packet
     * @param defaultValue
     * @return
     */
    private static int getInt(IBUFRDataPacket packet, int defaultValue) {
        int retValue = defaultValue;
        if (packet != null) {
            Object o = packet.getValue();
            if (o instanceof Double) {
                retValue = ((Double) o).intValue();
            } else if (o instanceof Long) {
                retValue = ((Long) o).intValue();
            }
        }
        return retValue;
    }

    /**
     * 
     * @param dataList
     * @param index
     * @return
     */
    private static int getHeightIncrement(List<IBUFRDataPacket> dataList,
            int index) {
        int hIncrement = 0;
        IBUFRDataPacket packet = dataList.get(index);
        if (packet != null) {
            hIncrement = ((Double) packet.getValue()).intValue();
        }
        return hIncrement;
    }

    /**
     * 
     * @param parmName
     * @param view
     * @param packet
     */
    private static void setViewData(String parmName, PointDataView view,
            IBUFRDataPacket packet) {
        setViewData(parmName, view, packet, 0);
    }

    /**
     * YES I KNOW THIS IS DUPLICATED!
     * 
     * @param parmName
     * @param view
     * @param packet
     */
    private static void setViewData(String parmName, PointDataView view,
            IBUFRDataPacket packet, int index) {
        if (packet != null) {
            Type t = view.getType(parmName);
            Object o = packet.getValue();
            if (o != null) {
                switch (t) {
                case STRING: {
                    if (o instanceof String) {
                        view.setString(parmName, (String) o, index);
                    }
                }
                case INT: {
                    if (o instanceof Double) {
                        view.setInt(parmName, ((Double) o).intValue(), index);
                    } else if (o instanceof Long) {
                        view.setInt(parmName, ((Long) o).intValue(), index);
                    }
                }
                case LONG: {
                    if (o instanceof Double) {
                        view.setLong(parmName, ((Double) o).longValue(), index);
                    } else if (o instanceof Long) {
                        view.setLong(parmName, (Long) o, index);
                    }
                }
                case FLOAT: {
                    if (o instanceof Double) {
                        view.setFloat(parmName, ((Double) o).floatValue(),
                                index);
                    } else if (o instanceof Long) {
                        view.setFloat(parmName, ((Long) o).floatValue(), index);
                    }
                }
                }
            }
        }
    }

    /**
     * 
     * @return
     */
    private static Profilers populateProfilerData() {
        Profilers profilers = null;

        PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();

        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String site = ctx.getContextName();

        logger.info("Loading " + PROFILER_SITES + " for site [" + site + "]");

        File siteDir = pathMgr.getFile(ctx, "profiler");

        File srcFile = new File(siteDir, PROFILER_SITES);

        profilers = Profilers.loadProfilers(srcFile);
        if (profilers.isLoaded()) {
            logger.info("Loading " + PROFILER_SITES + " Successful");
        } else {
            logger.error("Loading " + PROFILER_SITES + " failed");
        }

        return profilers;
    }

}
