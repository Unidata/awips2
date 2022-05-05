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
package com.raytheon.uf.edex.plugin.scan.process;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.GraphicBlockValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.TVSTableDataRow;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.ScanAlarmXML;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.plugin.scan.ScanDao;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;
import org.locationtech.jts.geom.Coordinate;

/**
 *
 * abstract processing of RadarRecords
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 07, 2009  2037     dhladky   Initial Creation.
 * Apr 04, 2018  6696     randerso  Code cleanup
 * Aug 01, 2018  6685     randerso  Added generateAlarmString() and sendAlarm()
 *                                  methods
 * Jan 31, 2019  7655     tgurney   Get radar-specific alarm time
 *
 * </pre>
 *
 * @author dhladky
 */

public abstract class RadarProduct extends ScanProduct {

    private static final long serialVersionUID = 1L;

    protected static final String RADAR = "radar";

    protected static final String SVRWX = "svrwx";

    protected static final String POLH = "polh";

    protected static final String HVYPR = "hvypr";

    protected static final double layer = 0.0;

    protected static final Map<ScanTables, String> EVENT_TYPE;
    static {
        EVENT_TYPE = new HashMap<>(ScanTables.values().length, 1.0f);
        EVENT_TYPE.put(ScanTables.CELL, "cell");
        EVENT_TYPE.put(ScanTables.MESO, "meso");
        EVENT_TYPE.put(ScanTables.TVS, "TVS");
        EVENT_TYPE.put(ScanTables.DMD, "DMD circulation");

    }

    protected final int alarmTime;

    /**
     *
     * @param uri
     * @param tableType
     * @param filter
     */
    public RadarProduct(String uri, ScanTables tableType,
            ScanURIFilter filter) {
        super(uri, tableType, filter);

        ScanAlarmXML alarms = filter.getAlarmData();
        alarmTime = alarms.getAlarmTime(tableType, filter.getIcao());
    }

    @Override
    public RadarRecord getRecord() throws Exception {

        RadarRecord radRec = null;
        try {
            radRec = ScanCommonUtils.getRadarRecord(uri);
        } catch (Exception e) {
            statusHandler.error("Error getting radar record", e);
        }
        return radRec;
    }

    @Override
    public abstract boolean getAllowNew();

    @Override
    public abstract void process() throws Exception;

    @Override
    public ScanTableDataRow setSpatial(ScanTableDataRow row, String key,
            PersistableDataObject<?> pdo) {

        Coordinate coor = null;

        if (row instanceof DMDTableDataRow) {

            coor = RadarRecordUtil.getDMDLonLatFromFeatureID((RadarRecord) pdo,
                    key);

            if (row instanceof DMDTableDataRow) {

                coor = RadarRecordUtil
                        .getDMDLonLatFromFeatureID((RadarRecord) pdo, key);

                String featureVal = RadarRecordUtil.getFeatureValue(
                        (RadarRecord) pdo, key,
                        DMDAttributeIDs.BASE_AZIMUTH.getName());
                if (featureVal != null && !featureVal.equals(BLANK)) {
                    row.setAzm(new Double(featureVal));
                }

                featureVal = RadarRecordUtil.getFeatureValue((RadarRecord) pdo,
                        key, DMDAttributeIDs.BASE_RANGE.getName());
                if (featureVal != null && !featureVal.equals(BLANK)) {
                    row.setRng(new Double(featureVal));
                }

                featureVal = RadarRecordUtil.getFeatureValue((RadarRecord) pdo,
                        key, DMDAttributeIDs.DIRECTION.getName());
                if (featureVal != null && !featureVal.equals(BLANK)) {
                    row.setDir(new Double(featureVal));
                }

                featureVal = RadarRecordUtil.getFeatureValue((RadarRecord) pdo,
                        key, DMDAttributeIDs.SPEED.getName());
                if (featureVal != null && !featureVal.equals(BLANK)) {
                    row.setSpd(new Double(featureVal)
                            * ScanUtils.M_PER_SEC_TO_KTS);
                }
            }
        } else if (row instanceof CellTableDataRow) {

            Map<GraphicBlockValues, String> stormMap = RadarRecordUtil
                    .parseGraphicBlock((RadarRecord) pdo).get(key);
            String dazm = null;
            String drng = null;
            // azimuth
            if (stormMap.get(GraphicBlockValues.AZIMUTH) != null
                    && !stormMap.get(GraphicBlockValues.AZIMUTH).equals(UNKNOWN)
                    && !stormMap.get(GraphicBlockValues.AZIMUTH).equals(NO_DATA)
                    && !stormMap.get(GraphicBlockValues.AZIMUTH)
                            .equals(BLANK)) {
                row.setAzm(
                        new Double(stormMap.get(GraphicBlockValues.AZIMUTH)));
                dazm = row.getAzm().toString();

            }
            // range
            if (stormMap.get(GraphicBlockValues.RANGE) != null
                    && !stormMap.get(GraphicBlockValues.RANGE).equals(UNKNOWN)
                    && !stormMap.get(GraphicBlockValues.RANGE).equals(NO_DATA)
                    && !stormMap.get(GraphicBlockValues.RANGE).equals(BLANK)) {
                row.setRng(new Double(stormMap.get(GraphicBlockValues.RANGE)));

                drng = Double
                        .toString(row.getRng() * ScanUtils.NMI_TO_KM * 1000);
            }

            if (dazm != null && drng != null) {
                coor = RadarRecordUtil.getAzRangeLatLon((RadarRecord) pdo, key,
                        ((RadarRecord) pdo).getSpatialObject().getLat(),
                        ((RadarRecord) pdo).getSpatialObject().getLon(), dazm,
                        drng);
            }

        } else if (row instanceof TVSTableDataRow) {

            String dazm = null;
            String drng = null;

            if (((RadarRecord) pdo).getProductVals(
                    RadarConstants.MapValues.TVS_TYPE, key,
                    RadarConstants.MapValues.TVS_AZIMUTH) != null
                    && !((RadarRecord) pdo)
                            .getProductVals(RadarConstants.MapValues.TVS_TYPE,
                                    key, RadarConstants.MapValues.TVS_AZIMUTH)
                            .equals(UNKNOWN)
                    && !((RadarRecord) pdo)
                            .getProductVals(RadarConstants.MapValues.TVS_TYPE,
                                    key, RadarConstants.MapValues.TVS_AZIMUTH)
                            .equals(NO_DATA)
                    && !((RadarRecord) pdo)
                            .getProductVals(RadarConstants.MapValues.TVS_TYPE,
                                    key, RadarConstants.MapValues.TVS_AZIMUTH)
                            .equals(BLANK)) {
                row.setAzm(new Double(((RadarRecord) pdo).getProductVals(
                        RadarConstants.MapValues.TVS_TYPE, key,
                        RadarConstants.MapValues.TVS_AZIMUTH)));
                dazm = row.getAzm().toString();
            }
            if (((RadarRecord) pdo).getProductVals(
                    RadarConstants.MapValues.TVS_TYPE, key,
                    RadarConstants.MapValues.TVS_RANGE) != null
                    && !((RadarRecord) pdo)
                            .getProductVals(RadarConstants.MapValues.TVS_TYPE,
                                    key, RadarConstants.MapValues.TVS_RANGE)
                            .equals(UNKNOWN)
                    && !((RadarRecord) pdo)
                            .getProductVals(RadarConstants.MapValues.TVS_TYPE,
                                    key, RadarConstants.MapValues.TVS_RANGE)
                            .equals(NO_DATA)
                    && !((RadarRecord) pdo)
                            .getProductVals(RadarConstants.MapValues.TVS_TYPE,
                                    key, RadarConstants.MapValues.TVS_RANGE)
                            .equals(BLANK)) {
                row.setRng(new Double(((RadarRecord) pdo).getProductVals(
                        RadarConstants.MapValues.TVS_TYPE, key,
                        RadarConstants.MapValues.TVS_RANGE)));
                drng = Double
                        .toString(row.getRng() * ScanUtils.NMI_TO_KM * 1000);
            }

            if (dazm != null && drng != null) {
                coor = RadarRecordUtil.getAzRangeLatLon((RadarRecord) pdo, key,
                        ((RadarRecord) pdo).getSpatialObject().getLat(),
                        ((RadarRecord) pdo).getSpatialObject().getLon(), dazm,
                        drng);
            }
        }

        if (coor != null) {
            row.setLat(coor.y);
            row.setLon(coor.x);
            row.setCounty(getCountyBySpatialQuery(coor));
            row.setCwa(getCWABySpatialQuery(coor));
        }

        return row;
    }

    @Override
    public abstract ScanTableDataRow write(ScanTableDataRow row,
            PersistablePluginDataObject rec, String key);

    @Override
    public void setDataType() {
        this.dataType = RADAR;
    }

    /**
     * Generate the alarm string given the time of the new record
     *
     * @param newRecTime
     *            refTime of the new record
     * @return the alarm string or null if no alarm generated
     * @throws PluginException
     */
    protected String generateAlarmString(Date newRecTime)
            throws PluginException {
        // get latest time from database
        Date previousTime = new ScanDao().getLatestTime(tableType);

        /*
         * if new record is at least alarmTime minutes after the most recent
         * record in the database
         */
        if ((previousTime == null) || (alarmTime
                * TimeUtil.MILLIS_PER_MINUTE <= (newRecTime.getTime()
                        - previousTime.getTime()))) {

            StringBuilder alarmString = new StringBuilder();
            alarmString.append("NEW ");
            alarmString.append(EVENT_TYPE.get(tableType));
            alarmString.append(" for ");
            alarmString.append(filter.getIcao());
            alarmString.append(" over the last ");
            alarmString.append(alarmTime);
            alarmString.append(" minutes.");

            return alarmString.toString();
        }

        // no alarm should be generated
        return null;
    }

    /**
     * Send alarm to alertViz if alarm string is not null
     *
     * @param alarmString
     *            alarm text or null if no alarm
     */
    protected void sendAlarm(String alarmString) {
        if (alarmString != null) {
            EDEXUtil.sendMessageAlertViz(Priority.CRITICAL,
                    RadarConstants.PLUGIN_ID, "SCAN", "RADAR", alarmString,
                    null, null);
        }
    }
}
