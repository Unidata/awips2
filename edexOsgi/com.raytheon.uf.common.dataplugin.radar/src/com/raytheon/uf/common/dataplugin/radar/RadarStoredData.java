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
package com.raytheon.uf.common.dataplugin.radar;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.radar.level3.AlertAdaptationParameters;
import com.raytheon.uf.common.dataplugin.radar.level3.AlertMessage;
import com.raytheon.uf.common.dataplugin.radar.level3.GSMBlock.GSMMessage;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.TabularBlock;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2011            bsteffen     Initial creation
 * Mar 18, 2013 1804       bsteffen    Remove AlphanumericValues from radar
 *                                     HDF5.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarStoredData {

    public static final String RAW_DATA_ID = "Data";

    public static final String SHORT_DATA_ID = "ShortData";

    public static final String ANGLE_DATA_ID = "Angles";

    public static final String THRESHOLDS_ID = "Thresholds";

    public static final String SYM_BLOCK_ID = "Symbology";

    public static final String SYM_DATA_ID = "SymbologyData";

    public static final String GRAPHIC_BLOCK_ID = "Graphic";

    public static final String PRODUCT_VALS_ID = "ProductVals";

    public static final String TABULAR_ID = "Tabular";

    public static final String DEPENDENT_VALS_ID = "DependentValues";

    public static final String RECORD_VALS_ID = "RecordVals";

    public static final String STORM_IDS_ID = "StormIds";

    public static final String GSM_ID = "GSM";

    public static final String ALERT_MESSAGE_ID = "AlertMessage";

    public static final String AAP_ID = "AAP";

    private byte[] rawData;

    private short[] rawShortData;

    private float[] angleData;

    private GraphicBlock graphicBlock;

    private SymbologyBlock symbologyBlock;

    private Map<RadarDataKey, RadarDataPoint> symbologyData = new HashMap<RadarDataKey, RadarDataPoint>();

    private Map<MapValues, Map<String, Map<MapValues, String>>> productVals = new HashMap<MapValues, Map<String, Map<MapValues, String>>>();

    private Map<MapValues, Map<MapValues, String>> mapRecordVals = new HashMap<MapValues, Map<MapValues, String>>();

    private GSMMessage gsmMessage;

    private AlertMessage alertMessage;

    private Map<String, RadarDataKey> stormIDs = new HashMap<String, RadarDataKey>();

    private AlertAdaptationParameters aapMessage;

    private TabularBlock tabularBlock;

    private short[] thresholds;

    private short[] productDependentValues;

    public byte[] getRawData() {
        return rawData;
    }

    public void setRawData(byte[] rawData) {
        this.rawData = rawData;
    }

    public short[] getRawShortData() {
        return rawShortData;
    }

    public void setRawShortData(short[] rawShortData) {
        this.rawShortData = rawShortData;
    }

    public float[] getAngleData() {
        return angleData;
    }

    public void setAngleData(float[] angleData) {
        this.angleData = angleData;
    }

    public GraphicBlock getGraphicBlock() {
        return graphicBlock;
    }

    public void setGraphicBlock(GraphicBlock graphicBlock) {
        this.graphicBlock = graphicBlock;
    }

    public SymbologyBlock getSymbologyBlock() {
        return symbologyBlock;
    }

    public void setSymbologyBlock(SymbologyBlock symbologyBlock) {
        this.symbologyBlock = symbologyBlock;
    }

    public Map<RadarDataKey, RadarDataPoint> getSymbologyData() {
        return symbologyData;
    }

    public void setSymbologyData(Map<RadarDataKey, RadarDataPoint> symbologyData) {
        this.symbologyData = symbologyData;
    }

    public Map<MapValues, Map<String, Map<MapValues, String>>> getProductVals() {
        return productVals;
    }

    public void setProductVals(
            Map<MapValues, Map<String, Map<MapValues, String>>> productVals) {
        this.productVals = productVals;
    }

    public Map<MapValues, Map<MapValues, String>> getMapRecordVals() {
        return mapRecordVals;
    }

    public void setMapRecordVals(
            Map<MapValues, Map<MapValues, String>> mapRecordVals) {
        this.mapRecordVals = mapRecordVals;
    }

    public GSMMessage getGsmMessage() {
        return gsmMessage;
    }

    public void setGsmMessage(GSMMessage gsmMessage) {
        this.gsmMessage = gsmMessage;
    }

    public AlertMessage getAlertMessage() {
        return alertMessage;
    }

    public void setAlertMessage(AlertMessage alertMessage) {
        this.alertMessage = alertMessage;
    }

    public Map<String, RadarDataKey> getStormIDs() {
        return stormIDs;
    }

    public void setStormIDs(Map<String, RadarDataKey> stormIDs) {
        this.stormIDs = stormIDs;
    }

    public AlertAdaptationParameters getAapMessage() {
        return aapMessage;
    }

    public void setAapMessage(AlertAdaptationParameters aapMessage) {
        this.aapMessage = aapMessage;
    }

    public short[] getThresholds() {
        return thresholds;
    }

    public void setThresholds(short[] thresholds) {
        this.thresholds = thresholds;
    }

    public short[] getProductDependentValues() {
        return productDependentValues;
    }

    public void setProductDependentValues(short[] productDependentValues) {
        this.productDependentValues = productDependentValues;
    }

    public TabularBlock getTabularBlock() {
        return tabularBlock;
    }

    public void setTabularBlock(TabularBlock tabularBlock) {
        this.tabularBlock = tabularBlock;
    }

}
