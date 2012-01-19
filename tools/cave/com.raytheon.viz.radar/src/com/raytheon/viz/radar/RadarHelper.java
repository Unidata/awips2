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
package com.raytheon.viz.radar;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.CorrelatedShearPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.HdaHailPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.LinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.MesocyclonePacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SCITDataPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SpecialGraphicSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.StormIDPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TVSPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedContourVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.UnlinkedVectorPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.WindBarbPacket;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.awipstools.common.StormTrackData;

/**
 * Gets information from classes and processes these values to figure values for
 * SCAN and other
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2009            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarHelper {

    public static final RGB LIGHT_GREEN = new RGB(0, 251, 144);

    public static final RGB MED_GREEN = new RGB(0, 187, 0);

    public static final RGB GREEN = new RGB(0, 255, 0);

    public static final RGB LIGHT_YELLOW = new RGB(255, 255, 112);

    public static final RGB YELLOW = new RGB(255, 255, 0);

    public static final RGB DARK_YELLOW = new RGB(208, 208, 96);

    public static final RGB LIGHT_RED = new RGB(255, 96, 96);

    public static final RGB BRIGHT_RED = new RGB(255, 0, 0);

    public static final RGB MED_RED = new RGB(218, 0, 0);

    public static final RGB DARK_RED = new RGB(174, 0, 0);

    public static final RGB LIGHT_BLUE = new RGB(0, 224, 255);

    public static final RGB PURPLE = new RGB(231, 0, 255);

    public static final RGB MED_PURPLE = new RGB(255, 112, 255);

    public static final RGB BLACK = new RGB(0, 0, 0);

    public static final RGB MED_GRAY = new RGB(119, 119, 144);

    public static final RGB DARK_GRAY = new RGB(118, 118, 118);

    public static final RGB LIGHT_PINK = new RGB(255, 170, 170);

    public static final RGB MED_PINK = new RGB(238, 140, 140);

    public static final RGB DARK_PINK = new RGB(201, 112, 112);

    public static final RGB BLUE = new RGB(0, 0, 255);

    public static final RGB WHITE = new RGB(255, 255, 255);

    public static final int VWP_CODE = 48;

    public static final int VAD_CODE = 84;

    public static final int GSM_CODE = 2;

    public static final RGB TVS_COLOR = BRIGHT_RED;

    public static final RGB MESO_COLOR = YELLOW;

    public static final int DMD_MIN_RADIUS = 4;

    public static RGB color = YELLOW;

    private static char DEGREE_SYMBOL = 0xB0;

    public static final String[] rdaOpStatusStr = {
            "Automatic Calibration Disabled", "Online",
            "Maintenance Action Required", "Maintenance Action Mandatory",
            "Command Shutdown", "Inoperable", "Missing", "Wideband Disconnect" };

    public static final String[] rpgNarrowbandStatus = {
            "Commanded Disconnect", "Narrowband Loadshed" };

    public static final String[] rdaStatusStr = { null, "Startup", "Standby",
            "Restart", "Operate", "Missing", "Off-line Operate" };

    public static final String[] rdaAlarmStr = { "Indeterminate",
            "Tower/Utilities", "Pedestal", "Transmitter",
            "Receiver/Signal Processor", "RDA Control", "RDA Communications" };

    public static final String[] dteStr = { null, "None", "Reflectivity",
            "Velocity", "Spectrum Width", "Dual Polarization" };

    public static final String[] rpgOpStr = { "Loadshed", "Online",
            "Maintenance Action Required", "Maintenance Action Mandatory",
            "Commanded Shutdown" };

    public static final String[] rpgAlarmStr = { "No Alarms",
            "Node Connectivity", null, "RPG Control Task Failure",
            "Data Base Failure", null, "RPG Input Buffer Loadshed (Wideband)",
            null, "Product Storage Loadshed", null, null, null,
            "RPG/RPG Intercomputer Link Failure", "Redundant Channel Error",
            "Task Failure", "Media Failure" };

    public static final String[] rpgStatusStr = { "Restart", "Operate",
            "Standby", null, "Test Mode" };

    public static final String[] productAvailStr = { "Product Availability",
            "Degraded Availability", "Not Available" };

    public static final String[] rdaChannelStr = { "NWS Single Thread",
            "RDA 1", "RDA 2" };

    /**
     * Returns the information in the packet to be processed
     * 
     * @param sp
     * @return
     */
    public static List<?> getItems(SymbologyPacket sp) {
        List<?> items = null;
        if (sp instanceof UnlinkedVectorPacket) {
            UnlinkedVectorPacket uvp = (UnlinkedVectorPacket) sp;
            items = uvp.getVectors();
        } else if (sp instanceof UnlinkedContourVectorPacket) {
            UnlinkedContourVectorPacket uvp = (UnlinkedContourVectorPacket) sp;
            items = uvp.getVectors();
        } else if (sp instanceof LinkedVectorPacket) {
            LinkedVectorPacket lvp = (LinkedVectorPacket) sp;
            items = lvp.getVectors();
        } else if (sp instanceof LinkedContourVectorPacket) {
            LinkedContourVectorPacket lvp = (LinkedContourVectorPacket) sp;
            items = lvp.getVectors();
        } else if (sp instanceof HdaHailPacket) {
            HdaHailPacket hdp = (HdaHailPacket) sp;
            items = hdp.getPoints();
        } else if (sp instanceof TVSPacket) {
            TVSPacket tvp = (TVSPacket) sp;
            items = Arrays.asList(tvp.getPoints());
        } else if (sp instanceof WindBarbPacket) {
            WindBarbPacket wbp = (WindBarbPacket) sp;
            items = Arrays.asList(wbp.getPoints());
        } else if (sp instanceof SCITDataPacket) {
            SCITDataPacket sdp = (SCITDataPacket) sp;
            items = sdp.getPoints();
        } else if (sp instanceof StormIDPacket) {
            StormIDPacket id = (StormIDPacket) sp;
            items = Arrays.asList(id.getPoints());
        } else if (sp instanceof MesocyclonePacket) {
            MesocyclonePacket mp = (MesocyclonePacket) sp;
            items = Arrays.asList(mp.getPoints());
        } else if (sp instanceof CorrelatedShearPacket) {
            CorrelatedShearPacket csp = (CorrelatedShearPacket) sp;
            items = Arrays.asList(csp.getPoints());
        } else if (sp instanceof SpecialGraphicSymbolPacket) {
            SpecialGraphicSymbolPacket sgsp = (SpecialGraphicSymbolPacket) sp;
            items = sgsp.getPoints();
        }
        return items;
    }

    /**
     * Returns any other parameters needed from the packet, depending upon the
     * packet type
     * 
     * @param sp
     * @return
     */
    public static Object getOther(SymbologyPacket sp) {
        return null;
    }

    /*
     * Following functions contain nothing right now, but will in the future.
     */
    public void getVil() {

    }

    public void getEchoTops() {

    }

    public void getDigitalEchoTops() {

    }

    public static RGB getColor() {
        return color;
    }

    public static void setColor(RGB rgb) {
        color = rgb;
    }

    public static String formatBits(short bits, String[] strings) {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < 16; i++) {
            if ((bits & (1 << i)) != 0) {
                if (result.length() > 0)
                    result.append(',');
                String s = null;
                if (i < strings.length)
                    s = strings[i];
                if (s == null)
                    s = "Unknown " + Integer.toString(15 - i);
                result.append(s);
            }
        }
        return result.toString();
    }

    public static String getRdaChannelName(int channel) {
        if (channel >= 0 && channel < rdaChannelStr.length)
            return rdaChannelStr[channel];
        else
            return "";
    }

    /**
     * Get the latest STI data for use with Warngen and SRM8
     * 
     * @param icao
     *            The ICAO whose STI data is to be returned
     * @return The STI data or null if there isn't any STI data in the database
     *         for the specified ICAO
     * @throws VizException
     */
    public static StormTrackData getSTIData(String icao) throws VizException {
        long t0 = System.currentTimeMillis();
        StormTrackData rval = null;
        try {
            RadarRecord record = null;
            DbQueryRequest request = new DbQueryRequest();
            request.setEntityClass(RadarRecord.class.getName());
            request.setLimit(1);
            request.setOrderByField("dataTime.refTime");
            request.addConstraint("productCode", new RequestConstraint("58"));
            request.addConstraint("icao", new RequestConstraint(icao));
            request.addConstraint("pluginName", new RequestConstraint("radar"));

            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            for (Map<String, Object> result : response.getResults()) {
                record = (RadarRecord) result.get(null);
                break;
            }

            if (record != null) {
                // Populate the radar record with its data
                File loc = HDF5Util.findHDF5Location(record);
                IDataStore dataStore = DataStoreFactory.getDataStore(loc);
                RadarDataRetriever.populateRadarRecord(dataStore, record);

                // Get the Tabular data from the record
                Map<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>> stiMap = record
                        .getMapRecordVals();

                String stiDir = "";
                String stiSpeed = "";
                double direction = 0;
                double speed = 0;

                // Get the speed and dir from the record
                if (stiMap != null) {
                    stiDir = stiMap.get(RadarConstants.MapValues.STI_TYPE).get(
                            RadarConstants.MapValues.STI_AVG_DIRECTION);
                    stiSpeed = stiMap.get(RadarConstants.MapValues.STI_TYPE)
                            .get(RadarConstants.MapValues.STI_AVG_SPEED);
                }

                if (stiDir.isEmpty() || stiSpeed.isEmpty()) {
                    direction = 0;
                    speed = 0;
                } else {
                    direction = Double.parseDouble(stiDir);
                    speed = Double.parseDouble(stiSpeed);
                }

                // Populate the return data with the STI data
                rval = new StormTrackData();
                rval.setDate(record.getDataTime().getRefTime());
                rval.setMotionDirection(direction);
                rval.setMotionSpeed(speed);
            }
        } catch (FileNotFoundException f) {
            f.printStackTrace();
        } catch (StorageException g) {
            g.printStackTrace();
        }

        System.out.println("Time to get STI data: "
                + (System.currentTimeMillis() - t0) + "ms");

        return rval;
    }
}
