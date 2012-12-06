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

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.GraphicBlockValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.dataplugin.radar.util.TiltAngleBin;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.ScanAlarmXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;
import com.vividsolutions.jts.geom.Coordinate;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * 
 * Process incoming Reflectivity product
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ --------- ---------- --------------------------
 * 05/07/2009   2037      dhladky     Initial Creation.
 * 02/22/2012	DR14414	  mgamazay    Added initializing the ScanTableData table
 * 									  to an empty map if no features are present.
 * 09/06/2012	14727	  Xiaochuan	  setIsNew(false) for the storm id should based  	
 * 									  on FCSTRAN, FCSTDIR values.
 * 11/13/2012	14368	  Xiaochuan	  Required to set alarm time in a quiet time period 
 * 									  from the last event to new event (new storm come in).
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class CompositeReflectivityProduct extends RadarProduct {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private static final IUFStatusHandler theHandler = UFStatus
            .getHandler(CompositeReflectivityProduct.class);

    // radar server sends messages from edex to cave, handle that here
    private final String SCAN = "SCAN";

    /** composite Reflectivity prod ID */
    public static final String cz = "37";

    private short[] vilGrid = null;

    private GridGeometry2D stationGeometry = null;

    private static Date previousTime = null;

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public CompositeReflectivityProduct(String uri, ScanTables tableType,
            ScanURIFilter filter) {
        super(uri, tableType, filter);

    }

    @Override
    public void process() throws Exception {
        RadarRecord rec = null;
        try {
            rec = getRecord();
        } catch (Exception ve) {
            theHandler.error("Can't process Record...");
            return;
        }

        Map<String, Map<GraphicBlockValues, String>> catMap = RadarRecordUtil
                .parseGraphicBlock(rec);
        // Goes hand in hand with the GraphicBlockValues list for attribute
        // names
        ArrayList<String> catKeys = new ArrayList<String>();

        // creates the key list
        for (String key : catMap.keySet()) {
            catKeys.add(key);
        }

        ScanTableData<ScanTableDataRow> table = getTableData();
        table.setFeatureIds(catKeys);
        // DR14414 - if there are no items in the cat map - remove items from the table
		if (catKeys.size() == 0) {
			table.setTableData(new ConcurrentHashMap<String, ScanTableDataRow>());
		}
        table.setVcp(rec.getVolumeCoveragePattern());

        filter.setValidTime(rec.getDataTime().getRefTime());
        table.setVolScanTime(rec.getDataTime().getRefTime());

        /**
         * Sets elevation angle
         */
        if (rec.getTrueElevationAngle() != null) {
            filter.setCellTilt(TiltAngleBin.getPrimaryElevationAngle(rec
                    .getTrueElevationAngle()));
            table.setTrueAngle(rec.getTrueElevationAngle().doubleValue());
        }

        RadarRecord previousRec = filter.getRadarData().getRadarRecord(
                CompositeReflectivityProduct.cz);

        // only the CAT can add a new row to the CELL table
        if (catMap.size() > 0) {

            int initialSize = table.getTableData().keySet().size();

            if (initialSize > 0) {
                // remove old rows
                for (String id : getDeletions(catKeys, table)) {
                    table.removeRow(id);
                    // System.out.println("CELL: Deleted Row: " + id);
                }
                // by storm ID, update
                for (String id : getUpdates(catKeys, table)) {
                    if (table.getRow(id) != null) {
                        // previous position stuff
                        if ((table.getRow(id).getLat() != null)
                                && (table.getRow(id).getLon() != null)) {
                            if (((CellTableDataRow) table.getRow(id))
                                    .getPastCoordinates() != null) {
                                ((CellTableDataRow) table.getRow(id))
                                        .getPastCoordinates().put(
                                                rec.getDataTime().getRefTime(),
                                                new Coordinate(table.getRow(id)
                                                        .getLon(), table
                                                        .getRow(id).getLat()));
                                // System.out
                                // .println("CELL: UPDATED past lat/lon Row: "
                                // + id);
                            } else {
                                HashMap<Date, Coordinate> coors = new HashMap<Date, Coordinate>();
                                coors.put(rec.getDataTime().getRefTime(),
                                        new Coordinate(table.getRow(id)
                                                .getLon(), table.getRow(id)
                                                .getLat()));
                                ((CellTableDataRow) table.getRow(id))
                                        .setPastCoordinates(coors);
                                // System.out.println("CELL: NEW past lat/lon Row: "
                                // + id);
                            }
                        }

                        // clear out all non persistent previous values
                        table.getRow(id).clearNonPersistantData();
                        ((CellTableDataRow) table.getRow(id))
                                .purgeCoordinates(rec.getDataTime()
                                        .getRefTime());
                        table.updateRow(id, write(table.getRow(id), rec, id));
                        
                    }
                }
            }

            ScanAlarmXML alarms = filter.getAlarmData();
            boolean processAlarms = false;
            StringBuffer alarmString = null;

            ArrayList<String> newIds = getAdditions(catKeys, table);
            if (previousTime == null) {
                previousTime = rec.getDataTime().getRefTime();
            }
            
            if (alarms != null && newIds.size() > 0 && previousTime != null) {
                if (((alarms.getCellAlarmTime() * 60 * 1000) <= (rec
                        .getDataTime().getRefTime().getTime() - previousTime
                        .getTime()))) {

                    processAlarms = true;
                    alarmString = new StringBuffer();
                    alarmString.append("NEW cell for " + filter.icao
                            + " over the last " + alarms.getCellAlarmTime()
                            + " minutes.");
                }
                previousTime = rec.getDataTime().getRefTime();
            }
            // add new rows last
            for (String id : newIds) {
            	table.addRow(id,
                        write(new CellTableDataRow(rec.getDataTime()), rec, id));
            }
         
            // send out an alertViz message
            if (processAlarms) {
                EDEXUtil.sendMessageAlertViz(Priority.CRITICAL,
                        RadarConstants.PLUGIN_ID, SCAN, "RADAR",
                        alarmString.toString(), null, null);
            }
        }

        if (rec != null) {
            Date startTime = rec.getDataTime().getRefTime();
            if (filter.isLighting(startTime)) {
                ScanProduct.processLightning(
                        filter,
                        rec,
                        startTime,
                        new Date(
                                startTime.getTime()
                                        + getVolumeScanDurationSeconds(rec,
                                                previousRec) * 1000), table);
            }
        }

        if (rec != null) {
            filter.getRadarData().setRadarRecord(cz, rec);
        }

    }

    private static int getVolumeScanDurationSeconds(RadarRecord rec,
            RadarRecord prevRec) {
        int duration = getKnownVolumeScanTimeDuration(rec
                .getVolumeCoveragePattern());

        if (duration < 0) {
            if (prevRec != null
                    && (int) rec.getVolumeCoveragePattern() == (int) prevRec
                            .getVolumeCoveragePattern()) {
                duration = (int) (rec.getDataTime().getRefTime().getTime() - prevRec
                        .getDataTime().getRefTime().getTime());
            } else
                duration = DEFAULT_VOLUME_SCAN_DURATION;
        }

        return duration;
    }

    private static final int DEFAULT_VOLUME_SCAN_DURATION = 60 * 5;

    private static HashMap<Integer, Integer> volumeScanTimes;

    private static int getKnownVolumeScanTimeDuration(int vcp) {
        if (volumeScanTimes == null) {
            synchronized (CompositeReflectivityProduct.class) {
                if (volumeScanTimes == null) {
                    volumeScanTimes = new HashMap<Integer, Integer>();
                    IPathManager pm = PathManagerFactory.getPathManager();
                    LocalizationFile localizationFile = pm.getLocalizationFile(
                            pm.getContext(LocalizationType.EDEX_STATIC,
                                    LocalizationLevel.BASE),
                            "scan/ScanVCPInfo.txt");
                    try {
                        File file = localizationFile.getFile();
                        Properties p = new Properties();
                        InputStream ins = new FileInputStream(file);
                        try {
                            p.load(ins);
                        } finally {
                            ins.close();
                        }
                        for (Map.Entry<Object, Object> entry : p.entrySet()) {
                            Object ko = entry.getKey();
                            if (ko instanceof String) {
                                String k = (String) ko;
                                if (k.startsWith("VCP")) {
                                    volumeScanTimes.put(Integer.parseInt(k
                                            .substring(3, k.length())),
                                            Integer.parseInt((String) entry
                                                    .getValue()));
                                }
                            }
                        }
                    } catch (Exception e) {
                        theHandler.error("cannot load " + localizationFile, e);
                    }
                }
            }
        }
        if (volumeScanTimes != null) {
            Integer result = volumeScanTimes.get(vcp);
            if (result != null) {
                return result;
            }
        }

        return -1;
    }

    @Override
    public ScanTableDataRow write(ScanTableDataRow row,
            PersistablePluginDataObject rec, String key) {

        row.setIdent(key);
        row = setSpatial(row, key, rec);

        Map<GraphicBlockValues, String> cellValMap = RadarRecordUtil
                .parseGraphicBlock((RadarRecord) rec).get(key);

        // switch to the most recent STI record
        if (filter.getRadarRecord(StormTrackTabularProduct.sti) != null) {
            RadarRecord stiRec = filter
                    .getRadarRecord(StormTrackTabularProduct.sti);

            if (stiRec.getIds(MapValues.STI_TYPE).contains(key)) {

                if (!(stiRec.getProductVals(RadarConstants.MapValues.STI_TYPE,
                        key, RadarConstants.MapValues.STI_FORECAST_15_RANGE)
                        .equals(NO_DATA))) {
                    ((CellTableDataRow) row)
                            .setRng15(new Double(
                                    stiRec.getProductVals(
                                            RadarConstants.MapValues.STI_TYPE,
                                            key,
                                            RadarConstants.MapValues.STI_FORECAST_15_RANGE)));
                }
                if (!(stiRec
                        .getProductVals(
                                RadarConstants.MapValues.STI_TYPE,
                                key,
                                RadarConstants.MapValues.STI_FORECAST_15_DIRECTION)
                        .equals(NO_DATA))) {
                    ((CellTableDataRow) row)
                            .setAzm15(new Double(
                                    stiRec.getProductVals(
                                            RadarConstants.MapValues.STI_TYPE,
                                            key,
                                            RadarConstants.MapValues.STI_FORECAST_15_DIRECTION)));
                }
                if (!(stiRec.getProductVals(RadarConstants.MapValues.STI_TYPE,
                        key, RadarConstants.MapValues.STI_FORECAST_15_RANGE)
                        .equals(NO_DATA))) {
                    ((CellTableDataRow) row)
                            .setRng15(new Double(
                                    stiRec.getProductVals(
                                            RadarConstants.MapValues.STI_TYPE,
                                            key,
                                            RadarConstants.MapValues.STI_FORECAST_15_RANGE)));
                }
                if (!(stiRec
                        .getProductVals(
                                RadarConstants.MapValues.STI_TYPE,
                                key,
                                RadarConstants.MapValues.STI_FORECAST_30_DIRECTION)
                        .equals(NO_DATA))) {
                    ((CellTableDataRow) row)
                            .setAzm30(new Double(
                                    stiRec.getProductVals(
                                            RadarConstants.MapValues.STI_TYPE,
                                            key,
                                            RadarConstants.MapValues.STI_FORECAST_30_DIRECTION)));
                }
                if (!(stiRec.getProductVals(RadarConstants.MapValues.STI_TYPE,
                        key, RadarConstants.MapValues.STI_FORECAST_30_RANGE)
                        .equals(NO_DATA))) {
                    ((CellTableDataRow) row)
                            .setRng30(new Double(
                                    stiRec.getProductVals(
                                            RadarConstants.MapValues.STI_TYPE,
                                            key,
                                            RadarConstants.MapValues.STI_FORECAST_30_RANGE)));
                }
                if (!(stiRec
                        .getProductVals(
                                RadarConstants.MapValues.STI_TYPE,
                                key,
                                RadarConstants.MapValues.STI_FORECAST_45_DIRECTION)
                        .equals("NO DATA"))) {
                    ((CellTableDataRow) row)
                            .setAzm45(new Double(
                                    (stiRec.getProductVals(
                                            RadarConstants.MapValues.STI_TYPE,
                                            key,
                                            RadarConstants.MapValues.STI_FORECAST_45_DIRECTION))));
                }
                if (!(stiRec.getProductVals(RadarConstants.MapValues.STI_TYPE,
                        key, RadarConstants.MapValues.STI_FORECAST_45_RANGE)
                        .equals(NO_DATA))) {
                    ((CellTableDataRow) row)
                            .setRng45(new Double(
                                    (stiRec.getProductVals(
                                            RadarConstants.MapValues.STI_TYPE,
                                            key,
                                            RadarConstants.MapValues.STI_FORECAST_45_RANGE))));
                }
                if (!(stiRec
                        .getProductVals(
                                RadarConstants.MapValues.STI_TYPE,
                                key,
                                RadarConstants.MapValues.STI_FORECAST_60_DIRECTION)
                        .equals(NO_DATA))) {
                    ((CellTableDataRow) row)
                            .setAzm60(new Double(
                                    (stiRec.getProductVals(
                                            RadarConstants.MapValues.STI_TYPE,
                                            key,
                                            RadarConstants.MapValues.STI_FORECAST_60_DIRECTION))));
                }
                if (!(stiRec.getProductVals(RadarConstants.MapValues.STI_TYPE,
                        key, RadarConstants.MapValues.STI_FORECAST_60_RANGE)
                        .equals(NO_DATA))) {
                    ((CellTableDataRow) row)
                            .setRng60(new Double(
                                    (stiRec.getProductVals(
                                            RadarConstants.MapValues.STI_TYPE,
                                            key,
                                            RadarConstants.MapValues.STI_FORECAST_60_RANGE))));
                }
                if ((stiRec.getProductVals(RadarConstants.MapValues.STI_TYPE,
                        key, RadarConstants.MapValues.STI_ERROR_FCST) != null)) {
                    ((CellTableDataRow) row).setMvtErr(new Double((stiRec
                            .getProductVals(RadarConstants.MapValues.STI_TYPE,
                                    key,
                                    RadarConstants.MapValues.STI_ERROR_FCST))));
                }
                if ((stiRec.getProductVals(RadarConstants.MapValues.STI_TYPE,
                        key, RadarConstants.MapValues.STI_ERROR_MEAN) != null)) {
                    ((CellTableDataRow) row).setMvtMn(new Double((stiRec
                            .getProductVals(RadarConstants.MapValues.STI_TYPE,
                                    key,
                                    RadarConstants.MapValues.STI_ERROR_MEAN))));
                }
            }
        }

        // set the direction
        if ((cellValMap.get(GraphicBlockValues.FCSTDIR) != null)
                && !cellValMap.get(GraphicBlockValues.FCSTDIR).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.FCSTDIR).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.FCSTDIR).equals(BLANK)
                && !cellValMap.get(GraphicBlockValues.FCSTDIR).equals(NEW)) {
            ((CellTableDataRow) row).setDir(new Double(cellValMap
                    .get(GraphicBlockValues.FCSTDIR)));
            
            ((CellTableDataRow) row).setIsNew(false);
        }

        // set the speed
        if ((cellValMap.get(GraphicBlockValues.FCSTRAN) != null)
                && !cellValMap.get(GraphicBlockValues.FCSTRAN).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.FCSTRAN).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.FCSTRAN).equals(BLANK)
                && !cellValMap.get(GraphicBlockValues.FCSTRAN).equals(NEW)) {
            ((CellTableDataRow) row).setSpd(new Double(cellValMap
                    .get(GraphicBlockValues.FCSTRAN)));
            ((CellTableDataRow) row).setIsNew(false);
        }

        // Try setting it here
        if ((cellValMap.get(GraphicBlockValues.MDA) != null)
                && !cellValMap.get(GraphicBlockValues.MDA).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.MDA).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.MDA).equals(BLANK)) {
            ((CellTableDataRow) row).setMdaSR(cellValMap
                    .get(GraphicBlockValues.MDA));
        }

        // Set the tvs value
        if ((cellValMap.get(GraphicBlockValues.TVS) != null)
                && !cellValMap.get(GraphicBlockValues.TVS).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.TVS).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.TVS).equals(BLANK)) {
            ((CellTableDataRow) row).setTvs(cellValMap
                    .get(GraphicBlockValues.TVS));
        }

        // VIL and VIL related things
        if ((cellValMap.get(GraphicBlockValues.VIL) != null)
                && !cellValMap.get(GraphicBlockValues.VIL).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.VIL).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.VIL).equals(BLANK)) {

            ((CellTableDataRow) row).setVil(new Double(cellValMap
                    .get(GraphicBlockValues.VIL)));
        }

        if (filter.getRadarData().getRadarRecord(VILProduct.vil) != null) {

            if (vilGrid == null) {
                vilGrid = ScanUtils.convertToGrid(filter.getRadarData()
                        .getRadarRecord(VILProduct.vil),
                        ScanUtils.SCAN_GRID_DIM_SQ);
            }

            if (stationGeometry == null) {
                stationGeometry = ScanUtils.getStationGeometry(filter
                        .getRadarData().getRadarRecord(VILProduct.vil)
                        .getLocation().getStation().getCoordinate(),
                        ScanUtils.SCAN_GRID_DIM_RESOLUTION,
                        ScanUtils.SCAN_GRID_DIM);
            }

            HashMap<String, Float> vilVals = getVilDependentValues((CellTableDataRow) row);

            if (vilVals != null) {

                if (vilVals.get(VILProduct.SVRWX) != null) {
                    ((CellTableDataRow) row).setSvrwx(vilVals.get(
                            VILProduct.SVRWX).intValue());
                }

                if (vilVals.get(VILProduct.POLH) != null) {
                    ((CellTableDataRow) row).setPolh(vilVals.get(
                            VILProduct.POLH).intValue());
                }

                if (vilVals.get(VILProduct.HVYPR) != null) {
                    ((CellTableDataRow) row).setHvyPr(vilVals.get(
                            VILProduct.HVYPR).intValue());
                }
            }
        }
        // sets the DBZ value
        if ((cellValMap.get(GraphicBlockValues.DBZM) != null)
                && !cellValMap.get(GraphicBlockValues.DBZM).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.DBZM).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.DBZM).equals(BLANK)) {

            ((CellTableDataRow) row).setDbz(new Double(cellValMap
                    .get(GraphicBlockValues.DBZM)));
        }
        // sets the cell top
        if ((cellValMap.get(GraphicBlockValues.TOP) != null)
                && !cellValMap.get(GraphicBlockValues.TOP).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.TOP).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.TOP).equals(BLANK)) {

            String top = cellValMap.get(GraphicBlockValues.TOP);

            if (top.startsWith("<") || top.startsWith(">")) {
                top = top.substring(1, top.length());
            }
            ((CellTableDataRow) row).setTop(new Double(top));
        }

        // sets the cell height
        if ((cellValMap.get(GraphicBlockValues.HT) != null)
                && !cellValMap.get(GraphicBlockValues.HT).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.HT).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.HT).equals(BLANK)) {

            ((CellTableDataRow) row).setDbzHt(new Double(cellValMap
                    .get(GraphicBlockValues.HT)));
        }

        // sets the POSH
        if ((cellValMap.get(GraphicBlockValues.POSH) != null)
                && !cellValMap.get(GraphicBlockValues.POSH).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.POSH).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.POSH).equals(BLANK)) {

            ((CellTableDataRow) row).setPosh(new Integer(cellValMap
                    .get(GraphicBlockValues.POSH)));
        }

        // sets the POH
        if ((cellValMap.get(GraphicBlockValues.POH) != null)
                && !cellValMap.get(GraphicBlockValues.POH).equals(UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.POH).equals(NO_DATA)
                && !cellValMap.get(GraphicBlockValues.POH).equals(BLANK)) {

            ((CellTableDataRow) row).setPoh(new Integer(cellValMap
                    .get(GraphicBlockValues.POH)));
        }

        // sets the max Hail size
        if ((cellValMap.get(GraphicBlockValues.MXHAILSIZE) != null)
                && !cellValMap.get(GraphicBlockValues.MXHAILSIZE).equals(
                        UNKNOWN)
                && !cellValMap.get(GraphicBlockValues.MXHAILSIZE).equals(
                        NO_DATA)
                && !cellValMap.get(GraphicBlockValues.MXHAILSIZE).equals(BLANK)) {

            String hailSize = cellValMap.get(GraphicBlockValues.MXHAILSIZE);

            if (hailSize.startsWith("<") || hailSize.startsWith(">")) {
                hailSize = hailSize.substring(1, hailSize.length());
            }
            ((CellTableDataRow) row).setHsize(new Double(hailSize));

        }
        // VCP
        if ((((RadarRecord) rec).getVolumeCoveragePattern()) != null) {
            ((CellTableDataRow) row).setVcp(((RadarRecord) rec)
                    .getVolumeCoveragePattern());
        }

        // CAPE
        if (filter.getModelData().isType(
                filter.getModelParameter(CAPEProduct.cape).getModelName(),
                CAPEProduct.cape)) {
            ((CellTableDataRow) row).setCape(filter.getModelData().getValue(
                    filter.getModelParameter(CAPEProduct.cape).getModelName(),
                    CAPEProduct.cape,
                    new Coordinate(row.getLon(), row.getLat())));
        }
        // HELICITY
        if (filter.getModelData().isType(
                filter.getModelParameter(HELIProduct.heli).getModelName(),
                HELIProduct.heli)) {
            ((CellTableDataRow) row).setSreh(filter.getModelData().getValue(
                    filter.getModelParameter(HELIProduct.heli).getModelName(),
                    HELIProduct.heli,
                    new Coordinate(row.getLon(), row.getLat())));
        }

        return row;
    }

    @Override
    public boolean getAllowNew() {
        return true;
    }

    /**
     * Gets the Composite Reflectivity URI Pattern
     * 
     * @return
     */
    public static Pattern getPattern(String icao, double tiltAngle) {
        return Pattern.compile("^" + uriSeparator + RADAR + uriSeparator
                + wildCard + uriSeparator + icao + uriSeparator + cz
                + uriSeparator + tiltAngle + uriSeparator + layer);
    }

    /**
     * Gets the SQL
     * 
     * @param icao
     * @return
     */
    public static String getSQL(String icao, double tiltAngle, int interval) {
        return "select datauri from radar where icao = \'" + icao
                + "\' and productcode = " + cz
                + " and primaryelevationangle = " + tiltAngle
                + " and reftime > (now()- interval \'" + interval
                + " minutes\')";
    }

    private HashMap<String, Float> getVilDependentValues(CellTableDataRow row) {

        Double thick1000500 = null;
        Double windSpeed700 = null;
        Double uWind500 = null;
        Coordinate coord = new Coordinate(row.getLon(), row.getLat(), 0.0);

        double gh1000 = filter.getModelData().getValue(
                filter.getModelParameter(GH1000Product.GH1000).getModelName(),
                GH1000Product.GH1000, coord);
        double gh500 = filter.getModelData().getValue(
                filter.getModelParameter(GH500Product.GH500).getModelName(),
                GH500Product.GH500, coord);
        double u700 = filter.getModelData().getValue(
                filter.getModelParameter(U700Product.U700).getModelName(),
                U700Product.U700, coord);
        double v700 = filter.getModelData().getValue(
                filter.getModelParameter(V700Product.V700).getModelName(),
                V700Product.V700, coord);
        double u500 = filter.getModelData().getValue(
                filter.getModelParameter(U500Product.U500).getModelName(),
                U500Product.U500, coord);

        // System.out.println(row.getIdent() + " : " + row.getCounty()
        // + " VIL Product Calc: GH1000:" + gh1000 + " gh500:" + gh500
        // + " U700: " + u700 + " V700: " + v700 + " U500:" + u500);

        if ((gh500 != -99999) && (gh1000 != -99999)) {
            thick1000500 = (gh500 - gh1000);
        }
        if ((u700 != -99999) && (v700 != -99999)) {
            windSpeed700 = (Math.sqrt(u700 * u700 + v700 * v700) * ScanUtils.M_PER_SEC_TO_KTS);
        }
        if (u500 != -99999) {
            uWind500 = u500;
        }

        return ScanUtils.getVILDependentValues(vilGrid, stationGeometry,
                (double) filter.getSoundingData().getFrzLevel(), thick1000500,
                windSpeed700, uWind500, (double) filter.getSoundingData()
                        .getTotalTotals(), coord);
    }

}