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
package com.raytheon.uf.viz.monitor.data;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;

/**
 * This class is a container of ObZoneHourReports objects for a
 * caller-specified nominal date-time.
 * (this class corresponds to the RcHourReports c++ class in AWIPS-1)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec. 1, 2009  3424       zhao       Initial creation.
 * 
 * </pre>
 * 
 * @author zhao
 * @version 1.0
 */

public class ObHourReports {
	
	/** 
	 * the nominal time of this ObHourReports object
	 */
	private Date nominalTime;

	/**
	 * application name (snow, fog, safeseas, etc)
	 */
	private CommonConfig.AppName appName;
	
	/**
	 * key is zone id, value is ObZoneHourReports object
	 */
	private HashMap<String, ObZoneHourReports> hourReports;
	
	/**
	 * constructor
	 * 
	 * @param appName
	 */
	public ObHourReports(Date nominalTime, CommonConfig.AppName appName, AbstractThresholdMgr thresholdMgr) {
		this.nominalTime = nominalTime;
		this.appName = appName;
		hourReports = new HashMap<String, ObZoneHourReports>();		
		Map<String, ArrayList<String>> zoneStationMap = MonitoringArea.getPlatformMap();
		for ( String zone : zoneStationMap.keySet() ) {
			hourReports.put(zone, new ObZoneHourReports(nominalTime, zone, appName, thresholdMgr));
		}
	}

	public void addReport(ObReport report) {
		String station = report.getPlatformId();
		ArrayList<String> zones = MonitoringArea.getZoneIds(station);
		if ( zones.size() == 0 ) {
			System.err.println("Error: station: " + station + " is not associated with any zone in the monitoring area");
			return;
		}
		boolean hasZone = false;
		for ( String zone : zones ) {
			if ( hourReports.containsKey(zone) ) {
				hasZone = true;;
				hourReports.get(zone).addReport(report);
			}
		}
		if ( hasZone == false ) {
			System.err.println("Error in addreport() of ObHourReports: unable to add obs report to data archive"); 
		}
	}
		
	public HashMap<String, ObZoneHourReports> getHourReports() {
		return hourReports;
	}
	
	public TableData getZoneTableData() {
		TableData tblData = new TableData(appName);
		for ( String zone : hourReports.keySet() ) {
			tblData.addTableRowData(this.getObZoneHourReports(zone).getZoneTableRowData());
			//tblData.addTableRowData(hourReports.get(zone).getZoneTableRowData());
		}			
		return tblData;
	}
	
	public TableData getFogZoneTableData(HashMap<String, CellType> algCellType) {
		TableData tblData = new TableData(AppName.FOG);
		for ( String zone : hourReports.keySet() ) {
			CellType theAlgCellType; 
			if ( algCellType.containsKey(zone) ) {
				theAlgCellType = algCellType.get(zone);
			} else {
				theAlgCellType = CellType.NotAvailable;
			}
			tblData.addTableRowData(this.getObZoneHourReports(zone).getFogZoneTableRowData(theAlgCellType));
		}			
		return tblData;
	}
	
	public TableData getSSZoneTableData(HashMap<String, CellType> fogCellType) {
		TableData tblData = new TableData(AppName.SAFESEAS);
		for (String zone : hourReports.keySet()) {
			CellType theFogCellType;
			if (fogCellType.containsKey(zone)) {
				theFogCellType = fogCellType.get(zone);
			} else {
				theFogCellType = CellType.NotAvailable;
			}
			tblData.addTableRowData(this.getObZoneHourReports(zone)
					.getSSZoneTableRowData(theFogCellType));
		}
		return tblData;
	}

	/**
	 * Returns the ObZoneHourReports object of a caller-specified zone.
	 * If such object not available, returns null.
	 * @param zone
	 * @return
	 */
	public ObZoneHourReports getObZoneHourReports(String zone) {
		if ( !hourReports.containsKey(zone) ) {
			return null;
		}
		return hourReports.get(zone);
	}
	
	public Date getNominalTime() {
		return nominalTime;
	}
	
	public CommonConfig.AppName getAppName() {
		return appName;
	}

}
