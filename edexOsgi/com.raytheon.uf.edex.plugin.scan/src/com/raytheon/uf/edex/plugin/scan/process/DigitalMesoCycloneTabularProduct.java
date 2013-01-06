package com.raytheon.uf.edex.plugin.scan.process;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.regex.Pattern;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.dataplugin.radar.util.TiltAngleBin;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableData;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.ScanAlarmXML;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * 
 * Process incoming Digital Meso Cyclone product
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/07/2009   2037      dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class DigitalMesoCycloneTabularProduct extends RadarProduct {

    // radar server sends messages from edex to cave, handle that here
    private final String SCAN = "SCAN";

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** DMD prod ID */
    public static String dmd = "149";

    private UnitConverter kmToFt = SI.KILOMETER.getConverterTo(NonSI.FOOT);

    private UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND
            .getConverterTo(NonSI.KNOT);

    private static Date previousTime = null;

    /** Note: Conversion from IMU to English units in ScanUtils class */

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public DigitalMesoCycloneTabularProduct(String uri, ScanTables tableType,
            ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    @Override
    public void process() throws Exception {

        RadarRecord rec = null;
        try {
            rec = getRecord();
        } catch (Exception pe) {
            pe.printStackTrace();
        }

        ScanTableData<ScanTableDataRow> table = getTableData();

        /**
         * Sets elevation angle
         */
        if (rec.getTrueElevationAngle() != null) {
            filter.setDmdTilt(TiltAngleBin.getPrimaryElevationAngle(rec
                    .getTrueElevationAngle()));
            table.setTrueAngle(rec.getTrueElevationAngle().doubleValue());
        }

        // Special way of setting DMD times
        filter.setValidTime(rec.getDataTime().getRefTime());
        table.setVolScanTime(rec.getVolScanTime());
        table.setVcp(rec.getVolumeCoveragePattern());

        // DMD table attribute
        if (getTableType().equals(ScanTables.DMD)) {

            ArrayList<String> dmdKeys = (ArrayList<String>) RadarRecordUtil
                    .getDMDFeatureIDs(rec);

            if (dmdKeys != null) {
                table.setLastElevationAngle(rec.isLastElevationAngle());
                table.setFeatureIds(dmdKeys);
                int initialSize = table.getTableData().keySet().size();

                if (initialSize > 0) {

                    if (rec.isLastElevationAngle()) {
                        // Start from scratch with an end-of-volume product.
                        for (String fid : table.getTableData().keySet()) {
                            table.removeRow(fid);
                        }
                    } else {
                        for (String fid : getDeletions(dmdKeys, table)) {
                            table.removeRow(fid);
                            // System.out.println("Removed DMD fid: " + fid);
                        }

                        for (String fid : getUpdates(dmdKeys, table)) {
                            table.updateRow(fid,
                                    write(table.getRow(fid), rec, fid));
                            // System.out.println("Updating DMD fid: " + fid);
                        }
                    }
                }

                ScanAlarmXML alarms = filter.getAlarmData();
                boolean processAlarms = false;
                StringBuffer alarmString = null;

                ArrayList<String> newIds = getAdditions(dmdKeys, table);
                if (previousTime == null) {
                    previousTime = rec.getDataTime().getRefTime();
                }

                if (alarms != null && newIds.size() > 0) {
                    if (((alarms.getDmdAlarmTime() * 60 * 1000) <= (rec
                            .getDataTime().getRefTime().getTime() - previousTime
                            .getTime()))) {
                        processAlarms = true;
                        alarmString = new StringBuffer();
                        alarmString.append("NEW DMDcirculation for "
                                + filter.icao + " over the last "
                                + alarms.getDmdAlarmTime() + " minutes.");
                        previousTime = rec.getDataTime().getRefTime();
                    }
                }

                for (String fid : newIds) {
                    table.addRow(
                            fid,
                            write(new DMDTableDataRow(rec.getDataTime()), rec,
                                    fid));
                }

                // send out an alertViz message
                if (processAlarms) {
                    EDEXUtil.sendMessageAlertViz(Priority.CRITICAL,
                            RadarConstants.PLUGIN_ID, SCAN, "RADAR",
                            alarmString.toString(), null, null);

                }
            }
        }

        if (rec != null) {
            filter.getRadarData().setRadarRecord(dmd, rec);
        }
    }

    @Override
    public ScanTableDataRow write(ScanTableDataRow srow,
            PersistablePluginDataObject pdo, String key) {
        RadarRecord rec = (RadarRecord) pdo;
        GenericDataComponent gen = RadarRecordUtil.getFeatureValues(rec, key);

        DMDTableDataRow row = (DMDTableDataRow) setSpatial(srow, key, pdo);

        row.setIdent(key);

        String featureVal = gen.getValue(DMDAttributeIDs.ASSOCIATE_STORM_ID
                .getName());
        if ((featureVal != null) && !(featureVal).equals(BLANK)) {
            row.setStrmID(featureVal);
        }

        featureVal = gen.getValue(DMDAttributeIDs.AGE.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setAge(new Integer(featureVal) / ScanUtils.MIN_TO_SEC);
        }

        featureVal = gen.getValue(DMDAttributeIDs.MSI.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setMsi(new Integer(featureVal));
        }

        featureVal = gen.getValue(DMDAttributeIDs.STRENGTH_RANK_TYPE.getName());
        if (featureVal != null) {
            row.setRankType(featureVal);
        }

        featureVal = gen.getValue(DMDAttributeIDs.STRENGTH_RANK.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setRank(featureVal);
            String fullRank = row.getRank() + row.getRankType();
            updateCellTableMDA(
                    gen.getValue(DMDAttributeIDs.ASSOCIATE_STORM_ID.getName()),
                    fullRank);
        }

        featureVal = gen.getValue(DMDAttributeIDs.STRENGTH_RANK_TYPE.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setRankType(featureVal);
        }

        featureVal = gen.getValue(DMDAttributeIDs.ASSOCIATED_TVS.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setTvs(featureVal);
        }

        featureVal = gen.getValue(DMDAttributeIDs.BASE_HEIGHT.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setBase(kmToFt.convert(new Double(featureVal)) / 1000);
        }

        featureVal = gen.getValue(DMDAttributeIDs.DEPTH.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setDepth(kmToFt.convert(new Double(featureVal)) / 1000);
        }

        featureVal = gen.getValue(DMDAttributeIDs.STORM_RELATIVE_DEPTH
                .getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setRelDepth(new Double(featureVal));
        }

        featureVal = gen.getValue(DMDAttributeIDs.BASE_DIAMETER.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setLlDiam(new Double(featureVal) / ScanUtils.NMI_TO_KM);
        }

        featureVal = gen
                .getValue(DMDAttributeIDs.BASE_ROTATIONAL_VEL.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setLlVr(metersPerSecondToKnots.convert(new Double(featureVal)));
        }

        featureVal = gen.getValue(DMDAttributeIDs.MAX_ROTATIONAL_VEL.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setMaxVr(metersPerSecondToKnots.convert(new Double(featureVal)));
        }

        featureVal = gen.getValue(DMDAttributeIDs.HEIGHT_MAX_ROTATIONAL_VEL
                .getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setHtMxVr(kmToFt.convert(new Double(featureVal)) / 1000);
        }

        featureVal = gen.getValue(DMDAttributeIDs.BASE_SHEAR.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setLlShear(new Double(featureVal));
        }

        featureVal = gen.getValue(DMDAttributeIDs.BASE_GTG_VEL_DIFF.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setLlgtg(metersPerSecondToKnots.convert(new Double(featureVal)));
        }

        featureVal = gen.getValue(DMDAttributeIDs._0_2KM_ARL_CONVERGENCE
                .getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setLlConv(metersPerSecondToKnots
                    .convert(new Double(featureVal)));
        }

        featureVal = gen.getValue(DMDAttributeIDs._2_4KM_ARL_CONVERGENCE
                .getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setMlConv(metersPerSecondToKnots
                    .convert(new Double(featureVal)));
        }

        featureVal = gen.getValue(DMDAttributeIDs.ELEVATION_INDEX.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setElev0(featureVal);
        }

        featureVal = gen.getValue(DMDAttributeIDs.DETECTION_STATUS.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setStatus(featureVal);
        }

        featureVal = gen.getValue(DMDAttributeIDs.FCST_LAT.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values1 = featureVal.split(",");
            ArrayList<Double> points1 = new ArrayList<Double>();
            for (String val : values1) {
                points1.add(new Double(val));
            }
            row.setFcstLat(points1);
        }

        featureVal = gen.getValue(DMDAttributeIDs.FCST_LON.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> points2 = new ArrayList<Double>();
            for (String val : values2) {
                points2.add(new Double(val));
            }
            row.setFcstLon(points2);
        }

        /*
         * The num_fcst_pos attribute may be present (and set to zero) even if
         * fcst_{lat,lon} attributes are not.
         */
        featureVal = gen.getValue(DMDAttributeIDs.NUM_FCST_POSITIONS.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            int num_fcst_positions = Integer.parseInt(featureVal);
            if (num_fcst_positions < 1) {
                row.setFcstLat(new ArrayList<Double>());
                row.setFcstLon(new ArrayList<Double>());
            }
        }

        featureVal = gen.getValue(DMDAttributeIDs.PAST_LAT.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values1 = featureVal.split(",");
            ArrayList<Double> points1 = new ArrayList<Double>();
            for (String val : values1) {
                points1.add(new Double(val));
            }
            row.setPastLat(points1);
        }

        featureVal = gen.getValue(DMDAttributeIDs.PAST_LON.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> points2 = new ArrayList<Double>();
            for (String val : values2) {
                points2.add(new Double(val));
            }
            row.setPastLon(points2);
        }

        featureVal = gen.getValue(DMDAttributeIDs.NUM_PAST_POSITIONS.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            int num_past_positions = Integer.parseInt(featureVal);
            if (num_past_positions < 1) {
                row.setPastLat(new ArrayList<Double>());
                row.setPastLon(new ArrayList<Double>());
            }
        }

        featureVal = gen.getValue(DMDAttributeIDs._2D_HEIGHT.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> heights2 = new ArrayList<Double>();
            for (String val : values2) {
                heights2.add(new Double(val));
            }
            row.setTimeHeightHeight(heights2);
        }

        featureVal = gen.getValue(DMDAttributeIDs._2D_DIAMETER.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> diams2 = new ArrayList<Double>();
            for (String val : values2) {
                diams2.add(new Double(val));
            }
            row.setTimeHeightDiam(diams2);
        }

        featureVal = gen.getValue(DMDAttributeIDs._2D_ROTATIONAL_VEL.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> rotvel2 = new ArrayList<Double>();
            for (String val : values2) {
                rotvel2.add(new Double(val));
            }
            row.setTimeHeightRotvel(rotvel2);
        }

        featureVal = gen.getValue(DMDAttributeIDs._2D_SHEAR.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> shear2 = new ArrayList<Double>();
            for (String val : values2) {
                shear2.add(new Double(val));
            }
            row.setTimeHeightShear(shear2);
        }

        featureVal = gen.getValue(DMDAttributeIDs._2D_GTG_VEL_DIFF.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> gtgMax2 = new ArrayList<Double>();
            for (String val : values2) {
                gtgMax2.add(new Double(val));
            }
            row.setTimeHeightGtgMax(gtgMax2);
        }

        featureVal = gen.getValue(DMDAttributeIDs._2D_STRENGTH_RANK.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> rank2 = new ArrayList<Double>();
            for (String val : values2) {
                rank2.add(new Double(val));
            }
            row.setTimeHeightRank(rank2);
        }

        featureVal = gen.getValue(DMDAttributeIDs.ELEVATION_INDEX.getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> indexes2 = new ArrayList<Double>();
            for (String val : values2) {
                indexes2.add(new Double(val));
            }
            row.setTimeHeightElevationIndexes(indexes2);
        }

        featureVal = rec.getRecordVals(MapValues.DMD_TYPE,
                MapValues.DMD_ELEV_ANGLES);
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> angles2 = new ArrayList<Double>();
            for (String val : values2) {
                angles2.add(new Double(val));
            }
            row.setTimeHeightElevationAngles(angles2);
        }

        featureVal = rec.getRecordVals(MapValues.DMD_TYPE,
                MapValues.DMD_ELEV_TIMES);
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            String[] values2 = featureVal.split(",");
            ArrayList<Double> times2 = new ArrayList<Double>();
            for (String val : values2) {
                times2.add(new Double(val));
            }
            row.setTimeHeightTimes(times2);
        }

        featureVal = gen.getValue(DMDAttributeIDs.OVERLAPS_LOWER_FEATURE
                .getName());
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            if (featureVal.equalsIgnoreCase("Y")) {
                row.setOverlap(true);
            }
        }

        updateCellTableTop(row.getStrmID(), row.getBase(), row.getDepth());

        return row;
    }

    /**
     * Updates the Cell Table with the MDASr value
     * 
     * @param ident
     * @param mdaSR
     */
    private void updateCellTableMDA(String ident, String mdaSR) {
        if ((ident != null) && (mdaSR != null)) {
            CellTableData<?> ctable = (CellTableData<?>) filter
                    .getData(ScanTables.CELL);

            if ((ctable != null) && (ctable.getTableData().size() > 0)) {
                if (ctable.getTableData().contains(ident)) {
                    CellTableDataRow ctdr = (CellTableDataRow) ctable
                            .getRow(ident);
                    ctdr.setMdaSR(mdaSR);
                }
            }
        }
    }

    /**
     * Updates the Cell Table with the Base value
     * 
     * @param ident
     * @param mdaSR
     */
    private void updateCellTableTop(String ident, Double base, Double depth) {
        if ((ident != null) && (base > 0.0) && (depth > 0.0)) {
            CellTableData<?> ctable = (CellTableData<?>) filter
                    .getData(ScanTables.CELL);
            if ((ctable != null) && (ctable.getTableData().size() > 0)) {
                if (ctable.getTableData().contains(ident)) {
                    CellTableDataRow ctdr = (CellTableDataRow) ctable
                            .getRow(ident);
                    ctdr.setTop(base + depth);
                }
            }
        }
    }

    @Override
    public boolean getAllowNew() {
        return true;
    }

    /**
     * DMD URI Pattern, Meso Cyclone Detection Tabular
     * 
     * @return
     */
    public static Pattern getPattern(String icao) {
        return Pattern.compile("^" + uriSeparator + RADAR + uriSeparator
                + wildCard + uriSeparator + icao + uriSeparator + dmd
                + uriSeparator + wildCard + uriSeparator + wildCard);
    }
}
