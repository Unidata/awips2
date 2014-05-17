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
package com.raytheon.rcm.rmrmgr;

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.concurrent.DelayQueue;

import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.otrmgr.OTRManager;
import com.raytheon.rcm.rmr.ActiveRequest;
import com.raytheon.rcm.rmr.MultipleRequest;
import com.raytheon.rcm.rmr.RmrEvent;
import com.raytheon.rcm.rmr.RmrXml;
import com.raytheon.rcm.rmr.RmrXml.ActiveRequests;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.server.RadarServer;

public class RMRManager extends RadarEventAdapter implements Runnable {

	private RadarServer radarServer;
	private DelayQueue<ActiveRequest> activeRequests;
	/* We can't use synchronized(activeRequests) { ... } to check for duplicate
	 * names.  So...
	 */
	private HashMap<String, ActiveRequest> activeRequestNames;
	private OTRManager otrManager;
	
	public RMRManager(RadarServer radarServer) {
		this.radarServer = radarServer;
		activeRequests = new DelayQueue<ActiveRequest>();
		activeRequestNames = new HashMap<String, ActiveRequest>(); 
		
		loadActiveRequests();
		
		Thread t = new Thread(this, "RMR runner");
		t.setDaemon(true);
		t.start();
	}
	
	private static final String activeRequestPersistName =
		"ActiveMultipleRequests.xml";

	/** Load active requests.  This should only be called before the timer
	 * thread has started.
	 */
	private void loadActiveRequests() {
		Collection<ActiveRequest> currentActiveRequests = null;
		try {
			Unmarshaller u = RmrXml.getUnmarshaller();
			InputStream ins = radarServer.getConfiguration().
				getPersistedData(activeRequestPersistName);
			synchronized (u) {
				ActiveRequests arl = (ActiveRequests) u.unmarshal(ins);
				currentActiveRequests = arl.list;
			}
		} catch (FileNotFoundException e) {
			// silently ignore
		} catch (Exception e) {
			Log.errorf("Error loading active RMRs: %s", e);
		}

		// Add persisted requests to active list
		if (currentActiveRequests != null && currentActiveRequests.size() > 0) {
			Log.eventf("Reactivating %d RMRs.", currentActiveRequests.size());
			for (ActiveRequest ar : currentActiveRequests) {
				MultipleRequest mr = ar.getMultipleRequest();
				
				// Input validation checks...
				if (mr != null && ! activeRequestNames.containsKey(mr.getName())) {
					activeRequestNames.put(mr.getName(), ar);
					activeRequests.add(ar);				
				}
			}
		}
		
		// TODO: send notification...
	}
	
	private void storeActiveRequests() {
		try {
			Marshaller m = RmrXml.getMarshaller();
			ByteArrayOutputStream outs = new ByteArrayOutputStream(); 
			ActiveRequests arl = new ActiveRequests();
			synchronized (activeRequestNames) {
				arl.list.addAll(activeRequestNames.values());
			}
			synchronized (m) {
				 m.marshal(arl, outs);
			}
			radarServer.getConfiguration().
				putPersistedData(activeRequestPersistName, outs.toByteArray());
		} catch (Exception e) {
			Log.errorf("Error storing active RMRs: %s", e);
		}
	}
	
	public Collection<ActiveRequest> getActiveRequests() {
		synchronized (activeRequestNames) {
			return new ArrayList<ActiveRequest>(activeRequestNames.values());
		}
	}

	public String activateRequest(MultipleRequest mr) {
		synchronized (activeRequestNames) {
			if (activeRequestNames.containsKey(mr.getName())) {
				return String.format("A request named \"%s\" is already active",
						mr.getName());
			}
			
			if (! mr.isSingle()) {
				Log.eventf("RMR: Activating request \"%s\"", mr.getName());
				ActiveRequest ar = new ActiveRequest(mr);
				activeRequestNames.put(mr.getName(), ar);
				activeRequests.add(ar);
				storeActiveRequests();
				activeRequestNames.notifyAll();
				fireChanged();
			}
		}
		
		executeRequest(mr);
		return null;
	}
	
	public String cancelRequest(String requestName) {
		synchronized (activeRequestNames) {
			ActiveRequest ar = activeRequestNames.get(requestName);
			if (ar != null) {
				activeRequests.remove(ar);
				activeRequestNames.remove(requestName);
				storeActiveRequests();
				Log.eventf("RMR: Canceled request \"%s\"", ar.getMultipleRequest().getName());
				fireChanged();
			} else
				return String.format("There is no active request named \"%s\"",
						requestName);
		}
		return null;
	}
	
	
	@Override
	public void handleConfigEvent(ConfigEvent event) {
		// nothing
	}

	@Override
	public void handleRadarEvent(RadarEvent event) {
		// nothing
	}

	@Override
	public void run() {
		while (true) {
			try {
				doNextExecution(activeRequests.take());
			} catch (Exception e) {
				Log.errorf("Error while executing RMR: %s", e);
			}
		}
		
	}

	private void doNextExecution(ActiveRequest ar) {
		MultipleRequest mr = ar.getMultipleRequest();
		ar.setRemainingTime(ar.getRemainingTime() - mr.getInterval());
		if (ar.getRemainingTime() > 0) {
			// TODO: or use last nextTime if it does not differ too much?
			ar.setNextTime(System.currentTimeMillis() + 
					mr.getInterval() * 1000);
			activeRequests.add(ar);
			fireChanged();
		} else {
			synchronized (activeRequestNames) {
				activeRequestNames.remove(ar.getMultipleRequest().getName());
				Log.eventf("RMR: Request \"%s\" expired", mr.getName());
				fireChanged();
			}
		}
		storeActiveRequests();
		executeRequest(mr);
	}

	private void executeRequest(MultipleRequest mr) {
		OTRManager otrManager = getOTRManager();
		if (otrManager == null)
			return;
		
		Log.eventf("RMR: Running one iteration of request \"%s\"", mr.getName());
		
		for (MultipleRequest.Element elem : mr.getElements()) {
			otrManager.sendOneTimeRequests(Arrays.asList(elem.getRadarIDs()), 
					Arrays.asList(elem.getRequest()), null);
		}	
	}

	private OTRManager getOTRManager() {
		if (otrManager == null) {
			synchronized (this) {
				if (otrManager == null) {
					for (RadarEventListener l : radarServer.getListeners()) {
						if (l instanceof OTRManager) {
							otrManager = (OTRManager) l;
							break;
						}
					}
					if (otrManager == null)
						Log.error("Cannot send RMR: OTR manager not available.");
				}
			}
		}
			
		return otrManager;
	}
	
	private void fireChanged() {
		radarServer.handleNotificationEvent(new RmrEvent());
	}
}

