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

import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.dataplugin.radar.util.TiltAngleBin;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableData;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableData;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.TVSTableDataRow;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.ScanAlarmXML;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * 
 * Process incoming TVS Tabular data
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

public class TVSTabularProduct extends RadarProduct {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** TVS Tabular prod ID */
    public static String tvs = "61";

    // radar server sends messages from edex to cave, handle that here
    private final String SCAN = "SCAN";

    private static Date previousTime = null;

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public TVSTabularProduct(String uri, ScanTables tableType,
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

        filter.setValidTime(rec.getDataTime().getRefTime());
        table.setVolScanTime(rec.getDataTime().getRefTime());
        table.setVcp(rec.getVolumeCoveragePattern());

        /**
         * Sets elevation angle
         */
        if (rec.getTrueElevationAngle() != null) {
            filter.setTvsTilt(TiltAngleBin.getPrimaryElevationAngle(rec
                    .getTrueElevationAngle()));
            table.setTrueAngle(rec.getTrueElevationAngle().doubleValue());
        }

        if (getTableType().equals(ScanTables.TVS)) {

            ArrayList<String> tvsKeys = (ArrayList<String>) rec
                    .getIds(MapValues.TVS_TYPE);

            // first check for alarms
            if (tvsKeys != null) {

                ScanAlarmXML alarms = filter.getAlarmData();
                StringBuffer alarmString = null;

                if (previousTime == null) {
                    previousTime = rec.getDataTime().getRefTime();
                }

                if (tvsKeys.size() > 0 && alarms != null) {
                    if ((alarms.getTvsAlarmTime() * 60 * 1000) <= (rec
                            .getDataTime().getRefTime().getTime() - previousTime
                            .getTime())) {

                        alarmString = new StringBuffer();
                        alarmString.append("NEW tvs for " + filter.icao
                                + " over the last " + alarms.getTvsAlarmTime()
                                + " minutes.");

                        // TVS always gets displayed regardless, no checks
                        // necessary
                        EDEXUtil.sendMessageAlertViz(Priority.CRITICAL,
                                RadarConstants.PLUGIN_ID, SCAN, "RADAR",
                                alarmString.toString(), null, null);

                        previousTime = rec.getDataTime().getRefTime();
                    }
                }
            }

            // remove all rows regardless of age, we need it to blank
            for (String fid : table.getTableData().keySet()) {
                // System.out.println("Removing TVS FID: " + fid);
                table.removeRow(fid);
            }
            // add all rows in tvs product
            if (tvsKeys != null) {

                table.setFeatureIds(tvsKeys);

                for (String fid : tvsKeys) {
                    // System.out.println("Adding TVS FID: " + fid);
                    table.addRow(
                            fid,
                            write(new TVSTableDataRow(rec.getDataTime()), rec,
                                    fid));
                }
            }
        }

        if (rec != null) {
            filter.getRadarData().setRadarRecord(tvs, rec);
        }
    }

    @Override
    public ScanTableDataRow write(ScanTableDataRow row,
            PersistablePluginDataObject rec, String key) {

        row = setSpatial(row, key, rec);
        row.setIdent(key);

        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.STORM_ID) != null) {
            ((TVSTableDataRow) row).setStrmID(((RadarRecord) rec)
                    .getProductVals(RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.STORM_ID));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_FEATURE_TYPE) != null) {
            ((TVSTableDataRow) row).setType(((RadarRecord) rec).getProductVals(
                    RadarConstants.MapValues.TVS_TYPE, key,
                    RadarConstants.MapValues.TVS_FEATURE_TYPE));
            updateTables(((RadarRecord) rec).getProductVals(
                    RadarConstants.MapValues.TVS_TYPE, key,
                    RadarConstants.MapValues.STORM_ID),
                    ((RadarRecord) rec).getProductVals(
                            RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_FEATURE_TYPE));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_BASE) != null) {
            String base = ((RadarRecord) rec).getProductVals(
                    RadarConstants.MapValues.TVS_TYPE, key,
                    RadarConstants.MapValues.TVS_BASE);
            if (base.startsWith("<") || base.startsWith(">")) {
                base = base.substring(1, base.length());
            }
            ((TVSTableDataRow) row).setBase(new Double(base));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_TOP) != null) {
            String top = ((RadarRecord) rec).getProductVals(
                    RadarConstants.MapValues.TVS_TYPE, key,
                    RadarConstants.MapValues.TVS_TOP);
            if (top.startsWith("<") || top.startsWith(">")) {
                top = top.substring(1, top.length());
            }
            ((TVSTableDataRow) row).setTop(new Double(top));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_DEPTH) != null) {
            String depth = ((RadarRecord) rec).getProductVals(
                    RadarConstants.MapValues.TVS_TYPE, key,
                    RadarConstants.MapValues.TVS_DEPTH);
            if (depth.startsWith("<") || depth.startsWith(">")) {
                depth = depth.substring(1, depth.length());
            }
            ((TVSTableDataRow) row).setDepth(new Double(depth));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_LLDV) != null) {
            ((TVSTableDataRow) row).setLlDV((new Double(((RadarRecord) rec)
                    .getProductVals(RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_LLDV))));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_AVGDV) != null) {
            ((TVSTableDataRow) row).setAvgDv((new Double(((RadarRecord) rec)
                    .getProductVals(RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_AVGDV))));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_MXDV) != null) {
            ((TVSTableDataRow) row).setMaxDV((new Double(((RadarRecord) rec)
                    .getProductVals(RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_MXDV))));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_DVHGT) != null) {
            ((TVSTableDataRow) row).setMaxDvHt(new Double(((RadarRecord) rec)
                    .getProductVals(RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_DVHGT)));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_MXSHR) != null) {
            ((TVSTableDataRow) row).setShear(new Double(((RadarRecord) rec)
                    .getProductVals(RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_MXSHR)));
        }
        if (((RadarRecord) rec).getProductVals(
                RadarConstants.MapValues.TVS_TYPE, key,
                RadarConstants.MapValues.TVS_SHRHGT) != null) {
            ((TVSTableDataRow) row).setShrHt(new Double(((RadarRecord) rec)
                    .getProductVals(RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_SHRHGT)));
        }

        return row;
    }

    @Override
    public boolean getAllowNew() {
        return true;
    }

    /**
     * TVS URI Pattern, Tornadoes
     * 
     * @return
     */
    public static Pattern getPattern(String icao, double tiltAngle) {
        return Pattern.compile("^" + uriSeparator + RADAR + uriSeparator
                + wildCard + uriSeparator + icao + uriSeparator + tvs
                + uriSeparator + tiltAngle + uriSeparator + layer);
    }

    /**
     * Gets the SQL for lookup
     * 
     * @param icao
     * @return
     */
    public static String getSQL(String icao, double tiltAngle, int interval) {
        return "select datauri from radar where icao = \'" + icao
                + "\' and productcode = " + tvs
                + " and primaryelevationangle = " + tiltAngle
                + " and reftime > (now()- interval \'" + interval
                + " minutes\')";
    }

    /**
     * Updates the Cell & DMD Tables with the tvs value
     * 
     * @param ident
     * @param tvs
     */
    private void updateTables(String ident, String tvs) {
        if ((ident != null) && (tvs != null)) {
            CellTableData<?> ctable = (CellTableData<?>) filter
                    .getData(ScanTables.CELL);
            if (ctable != null) {
                if (ctable.getTableData().contains(ident)) {
                    ((CellTableDataRow) ctable.getRow(ident)).setTvs(tvs);
                }
            }

            DMDTableData<?> dtable = (DMDTableData<?>) filter
                    .getData(ScanTables.DMD);
            if (dtable != null) {
                for (Object id : dtable.getTableData().keySet()) {
                    if (((DMDTableDataRow) dtable.getRow((String) id))
                            .getStrmID().equals(ident)) {
                        ((DMDTableDataRow) dtable.getRow((String) id))
                                .setTvs(tvs);
                    }
                }
            }
        }
    }

}