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

import java.util.Date;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;


/**
 * This class is a container of observation reports (ObReport objects) for 
 * caller-specified station, zone and nominal date-time.  
 * The reports are sorted according to observation date-time. 
 * (this class corresponds to the RcPlatHourReports c++ class in AWIPS-1)
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

public class ObStnHourReports {

	/**
	 * The nominal time, zone, and station of this ObStnHourreports object
	 */
	private Date nominalTime;
	private String zone;
	private String station;
	
	// application name (snow, fog, safeseas, etc)
	CommonConfig.AppName appName;

	/**
	 * Thresholds manager
	 */
	private AbstractThresholdMgr thresholdMgr;
	
	// key is observation date-time, value is ObReport object
	private SortedMap<Date, ObReport> stnReports;
	
	public ObStnHourReports(Date nominalTime, String zone, String station, CommonConfig.AppName appName, AbstractThresholdMgr thresholdMgr) {
		this.nominalTime = nominalTime;
		this.zone = zone;
		this.station = station;
		this.appName = appName;
		this.thresholdMgr = thresholdMgr;
		stnReports = new TreeMap<Date, ObReport>();
	}

	public void addReport(ObReport report) {
		Date obsTime = report.getObservationTime();
		stnReports.put(obsTime, report);
	}
	
	public TableRowData getStationTableRowData() {
		TableRowData tblRowData = null;
		ObReport report = null; 
		if ( stnReports.isEmpty() ) {  // empty report for empty/default row in station table 
			report = new ObReport();
		} else {
			report = stnReports.get(stnReports.lastKey());
		}
		
		if ( appName==CommonConfig.AppName.FOG ) {
			tblRowData = TableUtil.getFogTableRowData(station, zone, report, thresholdMgr, CellType.NotAvailable);
		} else if ( appName==CommonConfig.AppName.SAFESEAS ) {
			tblRowData = TableUtil.getSafeseasTableRowData(station, zone,
					report, thresholdMgr, CellType.NotAvailable);
		} else if ( appName==CommonConfig.AppName.SNOW ) {
			tblRowData = TableUtil.getSnowTableRowData(station, zone, report, thresholdMgr);
		} else {
			System.out.println("unrecognized appName: " + appName);
		}
		return tblRowData;		
	}	

	/**
	 * Returns the ObReport object of a caller-specified observation time.
	 * If such an object is not available, returns null.
	 * @param obsTime
	 * @return
	 */
	public ObReport getObReport(Date obsTime) {
		if( !stnReports.containsKey(obsTime) ) {
			return null;
		}
		return stnReports.get(obsTime);
	}

	/**
	 * Returns latest ObReport of this hour; 
	 * if no report is available, returns null
	 * @return
	 */
	public ObReport getLatestObReport() {
		if ( stnReports.isEmpty() ) {
			return null;
		}
		return stnReports.get(stnReports.lastKey());
	}
	
	/**
	 * Returns a set (sorted in ascending order) of observation times.
	 * If no data available, returns null. 
	 * @return
	 */
	public Set<Date> getObsTimes() {
		if ( stnReports.isEmpty() ) {
			return null;
		}
		return stnReports.keySet();
	}
	
	public SortedMap<Date, ObReport> getStnReports() {
		return stnReports;
	}
	
	public Date getNominalTime() {
		return nominalTime;
	}
	
	public String getZone() {
		return zone;
	}
	
	public String getStation() {
		return station;
	}
		
	public CommonConfig.AppName getAppName() {
		return appName;
	}

	public AbstractThresholdMgr getThresholdMgr() {
		return thresholdMgr;
	}
	
	public void setThresholdMgr(AbstractThresholdMgr thresholdMgr) {
		this.thresholdMgr = thresholdMgr;
	}
	
    public double[] getStationCenter() {
        double[] stnCenter = null;
        if (!stnReports.isEmpty()) {
            ObReport report = this.getObReport(stnReports.lastKey());
            stnCenter = new double[] { report.getLongitude(),
                    report.getLatitude() };
        }
        return stnCenter;
    }
}
