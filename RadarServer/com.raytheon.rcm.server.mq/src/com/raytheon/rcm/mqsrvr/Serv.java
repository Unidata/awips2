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
package com.raytheon.rcm.mqsrvr;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.alertreq.AlertRequestDefinition;
import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.Globals;
import com.raytheon.rcm.config.MutableConfiguration;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.Util;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.otrmgr.OTRHandler;
import com.raytheon.rcm.otrmgr.OTRManager;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.rmr.ActiveRequest;
import com.raytheon.rcm.rmr.MultipleRequest;
import com.raytheon.rcm.rmrmgr.RMRManager;
import com.raytheon.rcm.rpsmgr.RPSListManager;
import com.raytheon.rcm.server.AlertRequestManager;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.server.RadarServer;
import com.raytheon.rcm.server.StatusManager;
import com.raytheon.rcm.server.StatusManager.RadarStatus;


/**
 * Common RadarServer request message handler.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ...
 * 2014-02-03   DR 14762   D. Friedman Handle NDM config files.
 * </pre>
 *
 */
public class Serv {
	
	protected RadarServer server;
	protected OTRManager otrManager;
	private RMRManager rmrManager;
	private AlertRequestManager arManager;

	public Serv(RadarServer server) {
		this.server = server;
		
		for (RadarEventListener l : server.getListeners()) {
			if (l instanceof OTRManager)
				otrManager = (OTRManager) l;
			else if (l instanceof RMRManager)
				rmrManager = (RMRManager) l;
			else if (l instanceof AlertRequestManager)
				arManager = (AlertRequestManager) l;
		}
		
		if (otrManager == null)
			Log.warn("No OTR manager is registered.  Remote clients cannot sent OTRs.");
		if (rmrManager == null)
			Log.warn("No RMR manager is registered.  Remote clients cannot sent RMRs.");
		if (arManager == null)
			Log.warn("No alert request manager is registered.  Remote clients cannot sent alert requests.");
	}

	public Collection<String> getRadarList() {
		return server.getConfiguration().getConfiguredRadarList();
	}
	
	public RadarConfig getRadarConfig(String radarID) {
		return server.getConfiguration().getConfigForRadar(radarID);
	}
	
	public Collection<RadarConfig> getAllRadarConfigs() {
		Configuration config = server.getConfiguration();
		Collection<RadarConfig> result = new ArrayList<RadarConfig>();
		for (String radarID : config.getConfiguredRadarList()) {
			RadarConfig rc = config.getConfigForRadar(radarID);
			if (rc != null)
				result.add(rc);
		}
		return result;
	}
	
	public RadarStatus getRadarStatus(String radarID) {
		return server.getStatusManager().getRadarStatus(radarID);
	}
	
	public Map<String, RadarStatus> getAllRadarStatus() {
		HashMap<String, RadarStatus> result = 
			new HashMap<String, RadarStatus>();
		Configuration config = server.getConfiguration();
		StatusManager statusManager = server.getStatusManager();
		for (String radarID : config.getConfiguredRadarList()) {
			// TODO: status should be a copy because it could change
			// another reason for AbstractRadarStatus
			RadarStatus rs = statusManager.getRadarStatus(radarID);
			if (rs != null)
				result.put(radarID, rs);
		}
		return result;
	}
	
	public void sendOTRs(List<String> radarIDs, List<Request> requests, OTRHandler otrHandler) {
		if (otrManager != null)
			otrManager.sendOneTimeRequests(radarIDs, requests, otrHandler);		
	}

	public void logOtrStatus() {
		otrManager.logStatus();
	}

	public String debugHandleMessage(String radarID, byte[] message) {
		Configuration config = server.getConfiguration(); 
		if (radarID == null) {
			int srcId = Message.sourceIdOf(message);
			for (String aRadarID : config.getConfiguredRadarList()) {
				RadarConfig rc = config.getConfigForRadar(aRadarID);
				if (rc != null && rc.getNexradID() == srcId) {
					radarID = rc.getRadarID();
					break;
				}
			}
			if (radarID == null)
				return String.format("No radar with ID " + srcId);
		} else {
			if (config.getConfigForRadar(radarID) == null)
				return String.format("Radar '%s' is not configured", radarID); 
		}
		// TODO: support explicit and detected radarID
		server.handleRadarEvent(new RadarEvent(RadarEvent.MESSAGE_RECEIVED, 
				radarID, message));
		return null;
	}

	public String sendRpsList(List<String> radarIDs, int vcp,
			List<Request> requests, boolean store) {
		// TODO: INVALID_OP_MODE
		RpsList rpsList = new RpsList(-1, vcp, 
				requests.toArray(new Request[0]));
		return sendRpsListCommon(radarIDs, rpsList, vcp, store);
	}

	public String sendRpsListData(List<String> radarIDs, int vcp,
			byte[] data, boolean store) {
		// TODO: INVALID_OP_MODE
		RpsList rpsList;
		try {
			rpsList = Util.parseRpsListData(data, -1, vcp);
		} catch (IOException e) {
			// TODO: ...
			return e.toString();
		} catch (JAXBException e) {
			// TODO: ...
			return e.toString();
		}
		return sendRpsListCommon(radarIDs, rpsList, vcp, store);
	}
	
	private String sendRpsListCommon(List<String> radarIDs, RpsList rpsList, 
			int vcp, boolean store) {
		StringBuilder sb = new StringBuilder();
		RPSListManager lm = getRPSListManager();
		if (lm != null) {
			for (String radarID : radarIDs) {
				String error = lm.sendRpsList(radarID, rpsList, store);
				if (error != null) {
					if (sb.length() > 0)
						sb.append('\n');
					sb.append(error);
				}
			}
		}
		if (sb.length() > 0)
			return sb.toString();
		else
			return null;
	}

	public RpsList getRpsList(String radarID, int opMode, int vcp) {
		if (vcp != -1) {
			Configuration config = server.getConfiguration();
			return config.getLocalRpsList(radarID, opMode, vcp, null);
		} else {
			RPSListManager lm = getRPSListManager();
			if (lm != null)
				return lm.getCurrentRpsList(radarID);
			else
				return null;
		}
	}
	
	private RPSListManager getRPSListManager() {
		for (RadarEventListener l : server.getListeners()) {
			if (l instanceof RPSListManager) {
				return (RPSListManager) l;
			}
		}
		return null;
	}

	public Globals getGlobalConfig() {
		Globals globals = new Globals();
		Configuration config = server.getConfiguration();
		globals.collectionEnabled = config.isCollectionEnabled();
		globals.decompressProducts = config.isDecompressProducts();
		globals.edexEndpoint = config.getEdexEndpoint();
		globals.pupID = config.getPupId();
		globals.regionCode = config.getRegionCode();
		globals.tdwrCollectionLimited = config.isTdwrCollectionLimited();
		globals.wmoSiteID = config.getWmoSiteID();
		globals.endpointConfig =  config.getEndpointConfig();
		return globals;
	}

	public String setGlobalConfig(Globals global) {
		Configuration config = server.getConfiguration();
		if (! (config instanceof MutableConfiguration))
			return "The RadarServer's current configuration system does not support live changes.";
		MutableConfiguration mc = (MutableConfiguration) config;
		boolean result = mc.setGlobalConfig(global);
		
		if (result)
			return null;
		else
			return "Error updating configuration."; // TODO: saved vs. taking affect...
	}

	public String setRadarConfig(RadarConfig rc) {
		Configuration config = server.getConfiguration();
		if (! (config instanceof MutableConfiguration))
			return "The RadarServer's current configuration system does not support live changes.";
		MutableConfiguration mc = (MutableConfiguration) config;
		boolean result = mc.setRadarConfig(rc);

		if (result)
			return null;
		else
			return "Error updating configuration."; // TODO: saved vs. taking affect...
	}
	
	public String activateRMR(MultipleRequest mr) {
		return rmrManager.activateRequest(mr);
	}
	
	public String cancelRMR(String requestName) {
		return rmrManager.cancelRequest(requestName);
	}

	public Collection<ActiveRequest> getActiveRMRs() {
		return rmrManager.getActiveRequests();
	}
	
	public AlertRequestDefinition getAlertRequest(String radarID, int areaIndex) {
		return arManager.getAlertRequest(radarID, areaIndex);
	}
	
	public String sendAlertRequest(String radarID, int areaIndex, 
			AlertRequestDefinition alertRequest) {
		return arManager.sendAlertRequest(radarID, areaIndex, alertRequest);
	}

	public String sendMessageToRPG(String radarID, byte[] message) {
		server.getConnectionManager().sendMessageToRadar(radarID, message);
		return null;
	}

    public String sendConfigFile(String fileName, byte[] fileData) {
        Configuration config = server.getConfiguration();
        if (! (config instanceof MutableConfiguration))
            return "The RadarServer's current configuration system does not support live changes.";
        MutableConfiguration mc = (MutableConfiguration) config;
        boolean result = mc.storeConfigFile(fileName, fileData);
        if (result)
            return null;
        else
            return "Error updating configuration.";
    }
}
