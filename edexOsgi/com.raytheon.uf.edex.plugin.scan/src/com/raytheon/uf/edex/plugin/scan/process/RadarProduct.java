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

import java.util.Map;

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
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * abstract processing of RadarRecords
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

public abstract class RadarProduct extends ScanProduct {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static double layer = 0.0;

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public RadarProduct(String uri, ScanTables tableType, ScanURIFilter filter) {
        super(uri, tableType, filter);
    }

    @Override
    public RadarRecord getRecord() throws Exception {

        RadarRecord radRec = null;
        try {
            radRec = ScanCommonUtils.getRadarRecord(uri);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return radRec;
    }

    @Override
    public abstract boolean getAllowNew();

    @Override
    public abstract void process() throws Exception;

    @Override
    public ScanTableDataRow setSpatial(ScanTableDataRow row, String key,
            PersistableDataObject pdo) {

        Coordinate coor = null;

        if (row instanceof DMDTableDataRow) {

            coor = RadarRecordUtil.getDMDLonLatFromFeatureID((RadarRecord) pdo,
                    key);

            if (row instanceof DMDTableDataRow) {

                coor = RadarRecordUtil.getDMDLonLatFromFeatureID(
                        (RadarRecord) pdo, key);

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
                    && !stormMap.get(GraphicBlockValues.AZIMUTH)
                            .equals(UNKNOWN)
                    && !stormMap.get(GraphicBlockValues.AZIMUTH)
                            .equals(NO_DATA)
                    && !stormMap.get(GraphicBlockValues.AZIMUTH).equals(BLANK)) {
                row.setAzm(new Double(stormMap.get(GraphicBlockValues.AZIMUTH)));
                dazm = row.getAzm().toString();

            }
            // range
            if (stormMap.get(GraphicBlockValues.RANGE) != null
                    && !stormMap.get(GraphicBlockValues.RANGE).equals(UNKNOWN)
                    && !stormMap.get(GraphicBlockValues.RANGE).equals(NO_DATA)
                    && !stormMap.get(GraphicBlockValues.RANGE).equals(BLANK)) {
                row.setRng(new Double(stormMap.get(GraphicBlockValues.RANGE)));

                drng = new Double(row.getRng() * ScanUtils.NMI_TO_KM * 1000)
                        .toString();
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
                    && !((RadarRecord) pdo).getProductVals(
                            RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_AZIMUTH).equals(
                            UNKNOWN)
                    && !((RadarRecord) pdo).getProductVals(
                            RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_AZIMUTH).equals(
                            NO_DATA)
                    && !((RadarRecord) pdo).getProductVals(
                            RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_AZIMUTH).equals(BLANK)) {
                row.setAzm(new Double(((RadarRecord) pdo).getProductVals(
                        RadarConstants.MapValues.TVS_TYPE, key,
                        RadarConstants.MapValues.TVS_AZIMUTH)));
                dazm = row.getAzm().toString();
            }
            if (((RadarRecord) pdo).getProductVals(
                    RadarConstants.MapValues.TVS_TYPE, key,
                    RadarConstants.MapValues.TVS_RANGE) != null
                    && !((RadarRecord) pdo).getProductVals(
                            RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_RANGE).equals(UNKNOWN)
                    && !((RadarRecord) pdo).getProductVals(
                            RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_RANGE).equals(NO_DATA)
                    && !((RadarRecord) pdo).getProductVals(
                            RadarConstants.MapValues.TVS_TYPE, key,
                            RadarConstants.MapValues.TVS_RANGE).equals(BLANK)) {
                row.setRng(new Double(((RadarRecord) pdo).getProductVals(
                        RadarConstants.MapValues.TVS_TYPE, key,
                        RadarConstants.MapValues.TVS_RANGE)));
                drng = new Double(row.getRng() * ScanUtils.NMI_TO_KM * 1000)
                        .toString();
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
        this.dataType = ScanProduct.RADAR;
    }

}
