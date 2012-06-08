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
package com.raytheon.uf.viz.monitor.safeseas;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.regex.Pattern;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.dataplugin.fog.FogRecord.FOG_THREAT;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.AdjacentWfoMgr;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.Monitor;
import com.raytheon.uf.viz.monitor.ObsMonitor;
import com.raytheon.uf.viz.monitor.data.MonitoringArea;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.data.TableCellData;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.data.TableRowData;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.safeseas.listeners.ISSResourceListener;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.safeseas.ui.dialogs.SSMonitoringAreaConfigDlg;
import com.raytheon.uf.viz.monitor.safeseas.ui.dialogs.SSZoneTableDlg;
import com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg;
import com.raytheon.uf.viz.monitor.util.MonitorThresholdConfiguration;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * SafeSeasMonitor, monitor Data that triggers changes to the SafeSeas display.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 1981       dhladky     Initial creation.
 * 3/2/2009     2047       grichard    Made pluginName an array.
 * 3/2/2009     2047       grichard    Added stationName array.
 * 11/30/2009   3424       Zhidong/Slav/wkwock Use real station data.
 * Dec 30, 2009 3424       zhao        use ObMultiHrsReports for obs data archive over time
 * July 20,2010 4891       skorolev    Added resource listener
 * May 15, 2012 14510      zhao        Modified processing at startup
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class SafeSeasMonitor extends ObsMonitor implements ISSResourceListener {
    /** Singleton instance of this class */
    private static SafeSeasMonitor monitor = null;

    /** zone table dialog **/
    public SSZoneTableDlg zoneDialog = null;

    /**
     * monitoring area config dialog
     */
    SSMonitoringAreaConfigDlg areaDialog = null;

    private SSMonitorConfigurationManager safeseasConfig = null;

    /**
     * This object contains all observation data necessary for the table dialogs
     * and trending plots
     */
	private ObMultiHrsReports obData;

    /** table data for the zone table **/
    private final TableData zoneTableData = new TableData(
            CommonConfig.AppName.SAFESEAS);

    /** table data for all table data **/
    private final TableData stationTableData = new TableData(
            CommonConfig.AppName.SAFESEAS);

    /** All SafeSeas plugins start with this */
	private static String OBS = "fssobs";

    /** regex wild card filter */
    protected static String wildCard = "[\\w\\(\\)-_:.]+";

    /** The application key */
    ChosenAppKey chosenAppKey = ChosenAppKey.SAFESEAS;

    private final ArrayList<ISSResourceListener> safeSeasResources = new ArrayList<ISSResourceListener>();

    /**
     * Time which Zone/County dialog shows.
     */
    public Date dialogTime = null;

	/** list of coordinates for each zone **/
	public HashMap<String, Geometry> zoneGeometries = null;

	/** data holder for FOG data **/
	public HashMap<Date, HashMap<String, FOG_THREAT>> algorithmData = null;

    /** Adjacent areas for current cwa **/
    private Geometry geoAdjAreas;

    /** Array of fogAlg listeners **/
    private final ArrayList<ISSResourceListener> fogResources = new ArrayList<ISSResourceListener>();

    /**
     * Private constructor, singleton
     */
    private SafeSeasMonitor() {
		pluginPatterns.add(ssPattern);
        readTableConfig(MonitorThresholdConfiguration.SAFESEAS_THRESHOLD_CONFIG);
		initObserver("fssobs", this);
    }

    public static synchronized SafeSeasMonitor getInstance() {
        if (monitor == null) {
            monitor = new SafeSeasMonitor();
			// Pre-populate dialog with an observation (METAR) for KOMA
			monitor.createDataStructures();
            monitor.getAdjAreas();
			monitor.processProductAtStartup("ss");
			monitor.fireMonitorEvent(monitor);
        }
        return monitor;
    }

    /**
     * DR#11279:
     * When monitor area configuration is changed, 
     * this module is called to re-initialize monitor
     * using new monitor area configuration 
     */
    public static void reInitialize() {
    	if ( monitor != null ) {
    		monitor = null;
    		monitor = new SafeSeasMonitor();
    	}
    }
    
	/**
	 * Creates the maps
	 */
	private void createDataStructures() {
		// [Jan 21, 2010, zhao]
		obData = new ObMultiHrsReports(CommonConfig.AppName.SAFESEAS);
		obData.setThresholdMgr(SSThresholdMgr.getInstance());

		algorithmData = new HashMap<Date, HashMap<String, FOG_THREAT>>();
	}

    public void launchDialog(String type, Shell shell) {

        if (type.equals("zone")) {
            if (zoneDialog == null) {
                zoneDialog = new SSZoneTableDlg(shell, obData);
                addMonitorListener(zoneDialog);
                zoneDialog.addMonitorControlListener(this);
            }
            zoneDialog.open();
            fireMonitorEvent(zoneDialog.getClass().getName());
        } else if (type.equals("area")) {
            areaDialog = new SSMonitoringAreaConfigDlg(shell,
                    "Safe Seas Monitor Area Configuration");
            areaDialog.open();
        }
    }

    @Override
    public boolean filterNotifyMessage(NotificationMessage alertMessage) {

        return false;
    }

    @Override
    public void processNotifyMessage(NotificationMessage filtered) {

    }

    @Override
    public void processProductMessage(final AlertMessage filtered) {
		if (ssPattern.matcher(filtered.dataURI).matches()) {
            // System.out.println("Found match: " + ssPattern + " URI: "
            // + filtered.dataURI);
                processURI(filtered.dataURI, filtered);
            }
    }

    public void addToStationDataTable(TableRowData trd, String stationID) {
        int numberCell = trd.getNumberOfCellData();
        TableRowData stationRowData = new TableRowData(numberCell + 1);
        RGB bgColor = new RGB(220, 175, 55);
        stationRowData.setTableCellData(0, new TableCellData(stationID,
                stationID, CellType.StationID, false, bgColor));
        for (int i = 0; i < numberCell; i++) {
            stationRowData.setTableCellData(i + 1, trd.getTableCellData(i));
        }

        stationTableData.addReplaceDataRow(stationRowData);
    }

    /**
     * Method that reads the table configuration and updates the zone monitor
     * threshold map
     * 
     * @param file
     *            -- the xml configuration filename
     */
    public void readTableConfig(String file) {
        HashMap<String, ArrayList<String>> zones = new HashMap<String, ArrayList<String>>();
        // create zones and station list
        try {
            SSMonitorConfigurationManager areaConfig = getMonitorAreaConfig();
            for (String zone : areaConfig.getAreaList()) {
                ArrayList<String> stations = areaConfig.getAreaStations(zone);
                zones.put(zone, stations);
            }
        } catch (Exception e) {
            System.out.println("SafeSeas failed to load configuration..."
                    + this.getClass().getName());
        }
        MonitoringArea.setPlatformMap(zones);
    }

    public SSMonitorConfigurationManager getMonitorAreaConfig() {
        if (safeseasConfig == null) {
            LocalizationManager mgr = LocalizationManager.getInstance();
            String siteScope = mgr.getCurrentSite();

            safeseasConfig = SSMonitorConfigurationManager.getInstance();
            safeseasConfig.readConfigXml(siteScope);
        }
        return safeseasConfig;
    }

	private Pattern ssPattern = Pattern
			.compile(URIFilter.uriSeperator + OBS + URIFilter.uriSeperator
					+ wildCard + URIFilter.uriSeperator + wildCard
					+ URIFilter.uriSeperator + cwa + URIFilter.uriSeperator
					+ wildCard + URIFilter.uriSeperator + wildCard
					+ URIFilter.uriSeperator + wildCard
					+ URIFilter.uriSeperator + "ss");

    @Override
    public void initObserver(String pluginName, Monitor monitor) {
        ProductAlertObserver.addObserver(pluginName, this);

    }

    @Override
    public void thresholdUpdate(IMonitorThresholdEvent me) {
        fireMonitorEvent(zoneDialog.getClass().getName());
    }

    @Override
    public void configUpdate(IMonitorConfigurationEvent me) {

    }

    /**
     * Kill this monitor by nullifying the monitor's private instance variable.
     */
    @Override
    public void nullifyMonitor() {
        // /**
        // * Before making the monitor null, remove observers
        // */
        // for (String p : pluginName) {
        // if (!pluginName.equals("")) {
        // stopObserver(p, this);
        // }
        // }
        monitor.removeMonitorListener(zoneDialog);
        monitor.fogResources.removeAll(getMonitorListeners());
        stopObserver("fssobs", this);
        monitor = null;
    }

    public TableData getZoneTableData() {
        return zoneTableData;
    }

    public TableData getStationTableData() {
        return stationTableData;
    }

    public ObMultiHrsReports getObData() {
        return obData;
    }

    @Override
    protected void process(ObReport result)
            throws Exception {

        // ProcessSafeSeasReport safeSeas = new ProcessSafeSeasReport(result);
        // safeSeas.processSafeSeasReport();
        // add data to obData
        obData.addReport(result);
        fireMonitorEvent(this);
    }

    /**
     * SSResource sets the dialogTime
     * 
     * @param dialogTime
     */
    public void updateDialogTime(Date dialogTime) {
            this.dialogTime = dialogTime;
            fireMonitorEvent(zoneDialog.getClass().getName());
    }

    public Date getDialogTime() {
        return dialogTime;
    }

    public void setDialogTime(Date dialogTime) {
        this.dialogTime = dialogTime;
    }

    public void addSSResourceListener(ISSResourceListener issr) {
        safeSeasResources.add(issr);
    }

    public void removeSSResourceListener(ISSResourceListener issr) {
        safeSeasResources.remove(issr);
    }

    public void closeDialog() {
        if (zoneDialog != null) {
            monitor.nullifyMonitor();

            zoneDialog.removeMonitorContorlListener(this);
            zoneDialog.shellDisposeDialog();
            zoneDialog = null;
        }
        if (areaDialog != null) {
            areaDialog.shellDisposeDialog();
            areaDialog = null;
        }
    }

    /**
     * Order the dates
     * 
     * @param type
     * @return
     */
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type) {
        return null;
    }

    /**
     * Get most recent time
     * 
     * @param type
     * @return
     */
    public DataTime getMostRecent(IMonitor monitor, String type) {
        return null;
    }

	public HashMap<String, Geometry> getMonitoringAreaGeometries() {
		if (zoneGeometries == null) {
			ArrayList<String> zones = getMonitorAreaConfig().getAreaList();
			zoneGeometries = new HashMap<String, Geometry>();
			for (String zone : zones) {
				try {
					zoneGeometries.put(zone,
							MonitorAreaUtils.getZoneGeometry(zone));
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
		return zoneGeometries;
	}

	public void setAlgorithmData(Date refTime,
			HashMap<String, FOG_THREAT> zoneThreats) {
		if (algorithmData.containsKey(refTime)) {
			algorithmData.remove(refTime);
		}
		algorithmData.put(refTime, zoneThreats);
	}

	/**
	 * Gets the algorithm threat
	 * 
	 * @param zone
	 * @return
	 */
	public HashMap<String, FOG_THREAT> getAlgorithmData(Date time) {

		HashMap<String, FOG_THREAT> algData = new HashMap<String, FOG_THREAT>();

		if ((algorithmData != null) && algorithmData.containsKey(time)) {
			algData = algorithmData.get(time);
		} else {
			// default nothing in Fog column
			for (String zone : MonitoringArea.getPlatformMap().keySet()) {
				algData.put(zone, FOG_THREAT.GRAY);
			}
		}
		return algData;
	}

	public HashMap<String, CellType> getAlgCellTypes(
			HashMap<String, FOG_THREAT> fogAlgThreats) {
		HashMap<String, CellType> types = new HashMap<String, CellType>();
		for (String zone : fogAlgThreats.keySet()) {
			CellType type = getAlgorithmCellType(fogAlgThreats.get(zone));
			types.put(zone, type);
		}
		return types;
	}

	private CellType getAlgorithmCellType(FOG_THREAT fog_THREAT) {
		CellType type = CellType.NotDetermined;
		if (fog_THREAT == FogRecord.FOG_THREAT.GREEN) {
			type = CellType.G;
		} else if (fog_THREAT == FogRecord.FOG_THREAT.YELLOW) {
			type = CellType.Y;
		} else if (fog_THREAT == FogRecord.FOG_THREAT.RED) {
			type = CellType.R;
		}
		return type;
	}

	/**
	 * Get most recent time
	 * 
	 * @param type
	 * @return
	 */
	public DataTime getMostRecent() {
		DataTime time = null;
		Set<Date> ds = this.algorithmData.keySet();
		DataTime[] times = new DataTime[ds.size()];
		int i = 0;
		for (Date d : ds) {
			times[i] = new DataTime(d);
			i++;
		}
		java.util.Arrays.sort(times);
		if (times.length > 0) {
			time = times[times.length - 1]; // most recent
		}

		return time;
	}

	public ZoneTableDlg getDialog() {
		return zoneDialog;
	}

    public void getAdjAreas() {
        this.setGeoAdjAreas(AdjacentWfoMgr.getAdjacentAreas(cwa));
    }

    /**
     * @param geoAdjAreas
     *            the geoAdjAreas to set
     */
    public void setGeoAdjAreas(Geometry geoAdjAreas) {
        this.geoAdjAreas = geoAdjAreas;
    }

    /**
     * @return the geoAdjAreas
     */
    public Geometry getGeoAdjAreas() {
        return geoAdjAreas;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.safeseas.listeners.ISSResourceListener#fogUpdate
     * ()
     */
    @Override
    public void fogUpdate() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                Iterator<ISSResourceListener> iter = fogResources.iterator();
                while (iter.hasNext()) {
                    ISSResourceListener listener = iter.next();
                    listener.fogUpdate();
                }
            }
        });

    }

	@Override
	protected void processAtStartup(ObReport report) {
		obData.addReport(report);
	}

}
