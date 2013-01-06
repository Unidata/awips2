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
 * Contractor Address:     6825 Pine Street, Suite 144
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

import java.util.ArrayList;
import java.util.Date;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.StringTokenizer;

import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;

import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;

import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.xml.ScanAlarmXML;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Process incoming MeosCyclone tabular product
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/07/2009   2037       dhladky      Initial Creation.
 * 02/23/2012	14536	   Xiaochuan	Add method getIdsFromGraphicBlock() to 
 * 										hold the ids that have the same order   
 * 										as the ids in source file.
 *
 *
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class MesoCycloneTabularProduct extends RadarProduct {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** Meso Cyclone prod ID */
    public static String md = "141";

    // radar server sends messages from edex to cave, handle that here
    private final String SCAN = "SCAN";

    private static Date previousTime = null;

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public MesoCycloneTabularProduct(String uri, ScanTables tableType,
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
        table.setVcp(rec.getVolumeCoveragePattern());

        if (previousTime == null) {
            previousTime = rec.getDataTime().getRefTime();
        }

        // populate needed data
        if (table != null) {
        	ArrayList<String> mesoKeys = (ArrayList<String>) getIdsFromGraphicBlock(rec);  
           
            // first check for alarms
             if (mesoKeys != null)	{
            	ScanAlarmXML alarms = filter.getAlarmData();
                StringBuffer alarmString = null;

                ArrayList<String> newMesoKeys = getAdditions(mesoKeys, table);

                if (newMesoKeys.size() > 0 && alarms != null) {
                    if ((alarms.getMesoAlarmTime() * 60 * 1000) <= (rec
                            .getDataTime().getRefTime().getTime() - previousTime
                            .getTime())) {

                        alarmString = new StringBuffer();
                        alarmString.append("NEW meso for " + filter.icao
                                + " over the last " + alarms.getMesoAlarmTime()
                                + " minutes.");

                        previousTime = rec.getDataTime().getRefTime();

                        EDEXUtil.sendMessageAlertViz(Priority.SIGNIFICANT,
                                RadarConstants.PLUGIN_ID, SCAN, "RADAR",
                                alarmString.toString(), null, null);
                    }
                }
            }

            // remove all rows regardless of age, we need it to blank
            for (String id : table.getTableData().keySet()) {
                table.removeRow(id);
            }
            // add all rows in meso product
            if ((rec != null) && (mesoKeys != null)) {
            	table.setFeatureIds(mesoKeys);
 
            	for (String fid : mesoKeys)
                {
                    table.addRow(
                            fid,
                            write(new DMDTableDataRow(rec.getDataTime()), rec,
                                    fid));
                }
            }
        }

        if (rec != null) {
            // Set meso times
            filter.getRadarData().setRadarRecord(md, rec);
            filter.setValidTime(rec.getDataTime().getRefTime());
            table.setVolScanTime(rec.getDataTime().getRefTime());
        }
    }

	/**
	 * @param rec
	 * @return
	 */
	public ArrayList<String> getIdsFromGraphicBlock(RadarRecord rec) {
		GraphicBlock gb = rec.getGraphicBlock();
		ArrayList<String> ids = new ArrayList<String>();

		if (gb != null) {
			Layer[] pages = gb.getPages();

			// Go through each page
			for (int i = 0; i < pages.length; i++) {
				SymbologyPacket[] packets = pages[i].getPackets();

				// keys listed in the first packet
				if (packets[0] instanceof TextSymbolPacket) {
					String ln_id = ((TextSymbolPacket) packets[0]).getTheText();
					StringTokenizer st = new StringTokenizer(ln_id, " ");
					int countE = 0;
					int countId = 2;
					while (st.hasMoreTokens()) {
						String e = st.nextToken();
						if (countE == countId) {
							ids.add(e);
							countId += 2;
						}
						countE++;
					}
				}
			}
		}

		return ids;
	}

    @Override
    public ScanTableDataRow write(ScanTableDataRow row,
            PersistablePluginDataObject rec, String key) {

        Map<MapValues, String> mesoMap = ((RadarRecord) rec)
                .getMapProductVals().get(MapValues.MESO_TYPE).get(key);

        String featureVal = null;
        Coordinate coor = null;
        String dazm = null;
        String drng = null;

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_CIRC_ID));
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setIdent(featureVal);
        }

        featureVal = (mesoMap
                .get(RadarConstants.MapValues.MESO_AZIMUTH_DIRECTION));
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setAzm(new Double(featureVal));
            dazm = row.getAzm().toString();
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_AZIMUTH_RANGE));
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setRng(new Double(featureVal));
            drng = new Double(row.getRng() * ScanUtils.NMI_TO_KM * 1000)
                    .toString();
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_MOTION_DIR));
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setDir(new Double(featureVal));
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_MOTION_SPD));
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            row.setSpd(new Double(featureVal));
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_STORM_ID));
        if (((featureVal) != null) && !featureVal.equals(BLANK)) {
            ((DMDTableDataRow) row).setStrmID(featureVal);
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_MSI));
        if (((featureVal) != null) && !featureVal.equals(BLANK)) {
            ((DMDTableDataRow) row).setMsi(new Integer(featureVal));
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_SR));
        if (((featureVal) != null) && !featureVal.equals(BLANK)) {
            ((DMDTableDataRow) row).setRank(featureVal);
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_TVS_TYPE));
        if (((featureVal) != null) && !featureVal.equals(BLANK)) {
            ((DMDTableDataRow) row).setTvs(featureVal);
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_BASE));
        if (((featureVal) != null) && !featureVal.equals(BLANK)) {
            String base = featureVal;
            if (base.startsWith("<") || base.startsWith(">")) {
                base = base.substring(1, base.length()).trim();
            }
            ((DMDTableDataRow) row).setBase(new Double(base));
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_DEPTH));
        if (((featureVal) != null) && !featureVal.equals(BLANK)) {
            String depth = featureVal;
            if (depth.startsWith("<") || depth.startsWith(">")) {
                depth = depth.substring(1, depth.length()).trim();
            }
            ((DMDTableDataRow) row).setDepth(new Double(depth));
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_DEPTH_PERCENT));
        if (((featureVal) != null) && !featureVal.equals(BLANK)) {
            ((DMDTableDataRow) row).setRelDepth(new Double(featureVal));
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_RV));
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            ((DMDTableDataRow) row).setLlVr(new Double(featureVal));
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_MAX_RV_SPD));
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            ((DMDTableDataRow) row).setMaxVr(new Double(featureVal));
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_DV));
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            ((DMDTableDataRow) row).setLlgtg(new Double(featureVal));
        }

        featureVal = (mesoMap.get(RadarConstants.MapValues.MESO_MAX_RV));
        if ((featureVal != null) && !featureVal.equals(BLANK)) {
            ((DMDTableDataRow) row).setHtMxVr(new Double(featureVal));
        }

        // coor = RadarRecordUtil
        // .getDMDLonLatFromFeatureID((RadarRecord) rec, key);

        if ((dazm != null) && (drng != null)) {
            coor = RadarRecordUtil
                    .getAzRangeLatLon((RadarRecord) rec, key,
                            ((RadarRecord) rec).getSpatialObject().getLat(),
                            ((RadarRecord) rec).getSpatialObject().getLon(),
                            dazm, drng);
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
    public boolean getAllowNew() {
        return true;
    }

    /**
     * DMD URI Pattern, Meso Cyclone Detection Tabular
     * 
     * @return
     */
    public static Pattern getPattern(String icao, double tiltAngle) {
        return Pattern.compile("^" + uriSeparator + RADAR + uriSeparator
                + wildCard + uriSeparator + icao + uriSeparator + md
                + uriSeparator + tiltAngle + uriSeparator + layer);
    }

    public static String getSQL(String icao, double tiltAngle, int interval) {
        return "select datauri from radar where icao = \'" + icao
                + "\' and productcode = " + md
                + " and primaryelevationangle = " + tiltAngle
                + " and reftime > (now()- interval \'" + interval
                + " minutes\')";
    }

}
