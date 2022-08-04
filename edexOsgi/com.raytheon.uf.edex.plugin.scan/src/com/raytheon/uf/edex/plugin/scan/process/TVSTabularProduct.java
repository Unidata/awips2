package com.raytheon.uf.edex.plugin.scan.process;

import java.util.List;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
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
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;

/**
 *
 * Process incoming TVS Tabular data
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * May 07, 2009  2037     dhladky    Initial Creation.
 * Nov 13, 2012  14368    Xiaochuan  Required to set alarm time in a quiet time
 *                                   period from the last event to new event
 *                                   (new storm come in).
 * Aug 31, 2017  6408     njensen    Keep product's specific valid time
 * Apr 04, 2018  6696     randerso   Code cleanup
 * Aug 01, 2018  6685     randerso   Use super class methods to send alarms
 *
 * </pre>
 *
 * @author dhladky
 */

public class TVSTabularProduct extends RadarProduct {

    private static final long serialVersionUID = 1L;

    /** TVS Tabular prod ID */
    public static final String TVS = "61";

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
        RadarRecord rec = getRecord();
        ScanTableData<ScanTableDataRow> table = getTableData();

        // DR 6408: Keep the times for each product separate
        filter.setTvsValidTime(rec.getDataTime().getRefTime());
        table.setVolScanTime(rec.getDataTime().getRefTime());
        table.setVcp(rec.getVolumeCoveragePattern());

        /**
         * Sets elevation angle
         */
        if (rec.getTrueElevationAngle() != null) {
            filter.setTvsTilt(TiltAngleBin
                    .getPrimaryElevationAngle(rec.getTrueElevationAngle()));
            table.setTrueAngle(rec.getTrueElevationAngle().doubleValue());
        }

        if (getTableType().equals(ScanTables.TVS)) {

            List<String> tvsKeys = rec.getIds(MapValues.TVS_TYPE);

            // generate alarm string
            String alarmString = null;
            if (tvsKeys != null) {
                if (!tvsKeys.isEmpty()) {
                    alarmString = generateAlarmString(
                            rec.getDataTime().getRefTime());
                }
            }

            // remove all rows regardless of age, we need it to blank
            for (String fid : table.getTableData().keySet()) {
                table.removeRow(fid);
            }
            // add all rows in TVS product
            if (tvsKeys != null) {

                table.setFeatureIds(tvsKeys);

                for (String fid : tvsKeys) {
                    table.addRow(fid, write(
                            new TVSTableDataRow(rec.getDataTime()), rec, fid));
                }
            }

            // send out the alertViz alarm
            sendAlarm(alarmString);
        }

        filter.getRadarData().setRadarRecord(TVS, rec);
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
            updateTables(
                    ((RadarRecord) rec).getProductVals(
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
     * @param icao
     * @param tiltAngle
     *
     * @return the dataURI pattern
     */
    public static Pattern getPattern(String icao, double tiltAngle) {
        return Pattern.compile("^" + DataURI.SEPARATOR + RADAR
                + DataURI.SEPARATOR + wildCard + DataURI.SEPARATOR + icao
                + DataURI.SEPARATOR + TVS + DataURI.SEPARATOR + tiltAngle
                + DataURI.SEPARATOR + layer);
    }

    /**
     * Gets the SQL for lookup
     *
     * @param icao
     * @param tiltAngle
     * @param interval
     * @return the SQL query string
     */
    public static String getSQL(String icao, double tiltAngle, int interval) {
        return "select datauri from radar where icao = \'" + icao
                + "\' and productcode = " + TVS
                + " and primaryelevationangle = " + tiltAngle
                + " and reftime > (now()- interval \'" + interval
                + " minutes\')";
    }

    /**
     * Updates the Cell & DMD Tables with the TVS value
     *
     * @param ident
     * @param TVS
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