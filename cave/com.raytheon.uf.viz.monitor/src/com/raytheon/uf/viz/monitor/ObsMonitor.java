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
package com.raytheon.uf.viz.monitor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.regex.Pattern;

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;

/**
 * 
 * obsMonitor, common for observation types
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2010 4759       dhladky     Initial creation.
 * Mar 15, 2012 14510      zhao        modified processProductAtStartup()
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public abstract class ObsMonitor extends Monitor {
    @Override
    protected abstract boolean filterNotifyMessage(NotificationMessage alertMessage);

    @Override
    protected abstract void nullifyMonitor();
    
    /**
     * these are the over-arching "First line" checked patterns by plugin, first
     * letter of icao
     **/
    protected ArrayList<Pattern> pluginPatterns = new ArrayList<Pattern>();
    
    /** these are the patterns for the stations **/
    protected ArrayList<Pattern> stationPatterns = new ArrayList<Pattern>();

	/** Current CWA **/
    public static String cwa = LocalizationManager.getInstance().getSite();

    /**
     * This method processes the incoming messages
     * 
     * @param result
     */
    protected abstract void process(ObReport result)
			throws Exception;
    
    protected abstract void processAtStartup(ObReport report);

    @Override
    protected abstract void processNotifyMessage(NotificationMessage filtered);

    @Override
    protected abstract void processProductMessage(AlertMessage filtered);

    @Override
    public abstract void thresholdUpdate(IMonitorThresholdEvent me);

    @Override
    public abstract void configUpdate(IMonitorConfigurationEvent me);
    
    /**
     * use this to do the initial filtering
     */
    public boolean filterProductMessage(AlertMessage alertMessage) {
        // Determine whether or not there is a station ID present in the decoded
        // alert notification for this particular plugin name.
        try {
            boolean b = false;
            for (Pattern p : pluginPatterns) {
                if (p.matcher(alertMessage.dataURI).find()) {
                    b = true;
                    break;
                }
            }
            return b;
        } catch (NullPointerException e) {
            return false;
        }
    }
    
    /**
     * Process the incoming dataURI
     * 
     * @param dataURI
     * @param filtered
     */
    public void processURI(String dataURI, AlertMessage filtered) {

		// Map<String, Object> attribs = filtered.decodedAlert;
        HashMap<String, RequestConstraint> vals = new HashMap<String, RequestConstraint>();
        LayerProperty lp = new LayerProperty();
        try {
			// vals.put("cwa", new RequestConstraint(cwa));
            vals.put("dataURI", new RequestConstraint(dataURI));
			vals.put("pluginName", new RequestConstraint("fssobs"));
			// vals.put("monitorUse",
			// new RequestConstraint(attribs.get("monitorUse").toString()));

            lp.setDesiredProduct(ResourceType.PLAN_VIEW);
            lp.setEntryQueryParameters(vals);

            String script = null;
            try {
                script = ScriptCreator.createScript(lp);
            } catch (IllegalArgumentException e) {
                System.out.println("Unsupported plugin name: " + pluginName
                        + " encountered");
            }
            if (script != null) {
                Object[] resp = Connector.getInstance().connect(script, null,
                        60000);
                if ((resp != null) && (resp.length > 0)) {
                    final PluginDataObject objectToSend = (PluginDataObject) resp[0];
                    try {
                        Display.getDefault().asyncExec(new Runnable() {
                            public void run() {
                                try {
                                    // ObReport obReport = new ObReport();
                                    // obReport.init();
                                    if (objectToSend instanceof FSSObsRecord
                                            && ((FSSObsRecord) objectToSend)
                                                    .getTimeObs() != null) {
                                        ObReport result = GenerateFSSObReport.generateObReport(objectToSend);
                                        System.out
                                                .println("New FSSrecord ===> "
                                                        + objectToSend
                                                                .getDataURI());
                                        process(result);
                                    }
                                } catch (Exception e) {
                                    e.printStackTrace();
                                }
                            }
                        });
                    } catch (Exception e) {
                        e.printStackTrace();
                    }

                } else if (resp == null) {

                } else if (resp.length == 0) {

                }
            }
        } catch (final VizException e) {
            System.err.println("ObsMonitor: URI: " + dataURI
                    + " failed to process. "+e.getMessage());
        }
    }

	/**
	 * Process products at startup
	 * 
	 * @param monitorUse
	 * 
	 */
	public void processProductAtStartup(String monitorUse) {

		/**
		 * Assume this number for MaxNumObsTimes is larger enough to cover data
		 * of all observations (at least 24 hours' worth of data) in database
		 * [changed from 10 to 240 on May, 18, 2010 for DR #6015, zhao]
		 */
		int MaxNumObsTimes = 240;
		HashMap<String, RequestConstraint> vals = new HashMap<String, RequestConstraint>();
		LayerProperty lp = new LayerProperty();
		try {
			vals.put("cwa", new RequestConstraint(cwa));
			vals.put("pluginName", new RequestConstraint("fssobs"));
			vals.put("monitorUse", new RequestConstraint(monitorUse));

			lp.setDesiredProduct(ResourceType.PLAN_VIEW);
			lp.setEntryQueryParameters(vals);

			// Ensure that the latest product is retrieved.
			// [Modified: retrieve at most MaxNumObsTimes data
			// points, Feb
			// 19, 2010, zhao]
			DataTime[] dataTimesAvailable = lp.getEntryTimes();
			Arrays.sort(dataTimesAvailable);
			if (dataTimesAvailable.length != 0) {
				// at most, MaxNumObsTimes observation times are
				// considered
				if (dataTimesAvailable.length > MaxNumObsTimes) {
					DataTime[] obsDataTime = new DataTime[MaxNumObsTimes];
					System.arraycopy(dataTimesAvailable,
							dataTimesAvailable.length - MaxNumObsTimes,
							obsDataTime, 0, MaxNumObsTimes);
					lp.setSelectedEntryTimes(obsDataTime);
				} else {
					lp.setSelectedEntryTimes(dataTimesAvailable);
				}
			}
			String script = null;
			try {
				script = ScriptCreator.createScript(lp);
			} catch (IllegalArgumentException e) {
				System.out.println("Unsupported plugin name encountered");
			}
			if (script != null) {
                final Object[] resp = Connector.getInstance().connect(script,
                        null,
						60000);
				System.out.println("ObsMonitor: Retriving data for monitor: " + monitorUse);
				if ((resp != null) && (resp.length > 0)) {

                   //Display.getDefault().syncExec(new Runnable() {
                        //public void run() {
                            for (int j = 0; j < resp.length; j++) {
                                PluginDataObject objectToSend = (PluginDataObject) resp[j];
                                ObReport result = GenerateFSSObReport.generateObReport(objectToSend);
                                processAtStartup(result);
                            }
                        //}

                    //});

				} else if (resp == null) {
					System.out
							.println("Null Response From Script Created For: "
									+ monitorUse);
				} else if (resp.length == 0) {
					System.out
							.println("Zero Length Response From Script Created For: "
									+ monitorUse);
				}
			} else {
				System.out.println("Null script return For: " + monitorUse);
			}
		} catch (final VizException e) {
			System.err
					.println("No data in database at startup.  " + monitorUse);
		}
	}
}
