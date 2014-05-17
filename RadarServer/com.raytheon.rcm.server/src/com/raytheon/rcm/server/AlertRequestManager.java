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
package com.raytheon.rcm.server;

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.HashSet;

import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.rcm.alertreq.AlertRequestDefinition;
import com.raytheon.rcm.alertreq.AlertRequestXml;
import com.raytheon.rcm.alertreq.AlertRequestXml.AlertRequests;
import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.message.AlertRequest;
import com.raytheon.rcm.message.Message;

public class AlertRequestManager extends RadarEventAdapter {

	RadarServer radarServer;
	HashSet<String> sentToRpg = new HashSet<String>();
	
	public AlertRequestManager(RadarServer radarServer) {
		this.radarServer = radarServer;
	}

	// TODO: null === send empty request and delete the file
	public String sendAlertRequest(String radarID, int areaIndex, 
			AlertRequestDefinition alertRequest) {
		
		Configuration config = radarServer.getConfiguration();
		RadarConfig rc = radarServer.getConfiguration().getConfigForRadar(radarID);

		if (rc == null)
			return "Unknown radar";
		if (! rc.isDedicated())
			return "Not a dedicated radar";

		doSendAlertRequest(radarID, areaIndex, alertRequest);
		
		if (alertRequest != null) {
			try {
				ByteArrayOutputStream outs = new ByteArrayOutputStream();
				Marshaller m = AlertRequestXml.getMarshaller();
				AlertRequests container = new AlertRequests();
				container.list.add(alertRequest);
				synchronized (m) {
					m.marshal(container, outs);
				}
				config.putPersistedData(getPersistedName(radarID, areaIndex), 
						outs.toByteArray());
			} catch (Exception e) {
				Log.errorf("Error storing aleart area #%d for %s: %s",
						areaIndex, radarID, e);
				return "Alert request sent, but could not store: " + e;
			}
		} else {
			config.removePersistedData(getPersistedName(radarID, areaIndex));
		}
		
		return null;
	}
	
	public AlertRequestDefinition getAlertRequest(String radarID, int areaIndex) {
		try {
			Configuration config = radarServer.getConfiguration();
			InputStream ins = config.getPersistedData(
					String.format("alertRequest.%s.%d", radarID, areaIndex));
			try {
				Unmarshaller u = AlertRequestXml.getUnmarshaller();
				synchronized (u) {
					AlertRequests ard = (AlertRequests) u.unmarshal(ins);
					if (ard.list != null && ard.list.size() > 0)
						return ard.list.get(0);
					else
						return null;
				}
			} finally {
				ins.close();
			}
		} catch (FileNotFoundException e) {
			// ignore
		} catch (Exception e) {
			// TODO: return exception for clients...
			Log.errorf("Error loading aleart area #%d for %s: %s",
					areaIndex, radarID, e);
		}
		return null;
	}
	
	private String getPersistedName(String radarID, int areaIndex) {
		return String.format("alertRequest.%s.%d", radarID, areaIndex);
	}

	
	@Override
	public void handleRadarEvent(RadarEvent event) {
		if (event.getType() == RadarEvent.MESSAGE_RECEIVED &&
				Message.messageCodeOf(event.getMessageData()) == Message.GSM) {
			String radarID = event.getRadarID();
			if (! sentToRpg.contains(radarID)) {
				Configuration config = radarServer.getConfiguration();
				RadarConfig rc = config.getConfigForRadar(radarID);
				if (rc == null || ! rc.isDedicated())
					return;

				for (int areaIndex = 1; areaIndex <= 2; ++areaIndex) {
					AlertRequestDefinition alertRequest = null;
					alertRequest = getAlertRequest(radarID, areaIndex);
					
					if (alertRequest != null)
						doSendAlertRequest(radarID, areaIndex, alertRequest);
				}
				
				sentToRpg.add(radarID);
			}
		} else if (event.getType() == RadarEvent.CONNECTION_DOWN)
			sentToRpg.remove(event.getRadarID());
	}

	private void doSendAlertRequest(String radarID, int areaIndex,
			AlertRequestDefinition alertRequest) {
		
		// If null, send empty request to RPG
		if (alertRequest == null)
			alertRequest = new AlertRequestDefinition();
		
		AlertRequest.Threshold[] outThresholds = 
			new AlertRequest.Threshold[alertRequest.elements.size()];
		// TODO: handle null thresholdIndex
		for (int i = 0; i < outThresholds.length; ++i) {
			AlertRequestDefinition.Threshold inp = 
				alertRequest.elements.get(i);
			AlertRequest.Threshold out = 
				new AlertRequest.Threshold((short) inp.category, 
						inp.thresholdIndex, inp.requestProduct);
			outThresholds[i] = out;
		}
			
		byte[] msg = AlertRequest.encode(areaIndex, 
				outThresholds, alertRequest.boxBits);
		radarServer.getConnectionManager().sendMessageToRadar(radarID, msg);
	}
	
}
