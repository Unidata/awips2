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
package com.raytheon.rcm.oldio;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.config.*;
import com.raytheon.rcm.server.Log;


/**
 * Manages all links to RPGs.  This is a relatively low-level interfaces.  The
 * various components of the Radar Server use ConnectionManager instead of
 * this class.
 * <p>
 * Provides the following operations: connect, disconnect, and send-message.
 * There is no send queue.  Thus, the send-message operation will only succeed
 * if a connection to the RPG is active.  
 * <p>
 * Events relating to the RPGs are
 * delegated to a signle RadarEventListener interface.
 */
public class LinkManager implements com.raytheon.rcm.server.LinkManager {

	Configuration config;
	Map<String, Link> activeLinks = new HashMap<String, Link>();
	
	public LinkManager(Configuration config) {
		this.config = config;
	}

    /* There is no requirement that specifies that dead connections
     * must be detected (or how often, etc.)  The following code
     * is a possible implementation.
     */
	/*
	private Thread thread;
	private synchronized void startKeepAlives() {
		if (thread == null) {
    	    Thread t = new Thread(new Runnable() {
    	       @Override
    	       public void run() {
    	           while (true) {
    	               try {
    	                   Thread.sleep(60 * 1000);
    	               } catch (InterruptedException e) {
    	                   // nothing
    	               }
    	               synchronized (LinkManager.this) {
    	                   for (Link link : activeLinks.values()) {
    	                       try {
    	                           link.sendKeepAlives();
	                           } catch (Exception e) {
	                               // nothing
	                           }
	                       }
    	               }
    	           }
    	       }
    	    });
    	    t.setName("LinkManager Keep-Alive");
    	    t.setDaemon(true);
    	    t.start();
	    }
	}
	*/
	
	public synchronized void stop() {
		Log.event("Shutting down.");
		// TODO: synch?
		for (Link link : activeLinks.values())
			link.teardown();
	}
	
	public void sendMessageToRadar(String radarID, byte[] msg) {
		synchronized (activeLinks) {
			Link link = activeLinks.get(radarID);
			if (link != null)
				link.sendMessage(msg);
			else
				Log.errorf("Attempt to send message to unconnected radar '%s'", radarID);
		}
	}
	
	public void notifyMessage(String radarID, byte[] msg) {
		if (radarHandler != null)
			radarHandler.handleRadarEvent(new RadarEvent(RadarEvent.MESSAGE_RECEIVED, 
					radarID, msg));
	}

	public void notifyConnected(String radarID, LinkResource linkResource) {
		if (radarHandler != null)
			radarHandler.handleRadarEvent(new RadarEvent(RadarEvent.CONNECTION_UP,
					radarID, linkResource));
	}
	
	public synchronized void notifyConnectFailed(String radarID) {
		synchronized (activeLinks) {
			activeLinks.remove(radarID);
		}
		// Currently not logged by EventLogger.  Logging is done in Link
		if (radarHandler != null)
			radarHandler.handleRadarEvent(new RadarEvent(RadarEvent.CONNECTION_ATTEMPT_FAILED,
					radarID));
	}

	public synchronized void notifyDisconnected(String radarID) {
		synchronized (activeLinks) {
			activeLinks.remove(radarID);
		}
		if (radarHandler != null)
			radarHandler.handleRadarEvent(new RadarEvent(RadarEvent.CONNECTION_DOWN,
					radarID));
	}

	public void connectRadar(String radarID) {
		Link link = null;

		synchronized (activeLinks) {
			if (activeLinks.get(radarID) != null)
				return;
			
			RadarConfig rc = config.getConfigForRadar(radarID);
			if (rc != null) {
				if (rc.isEnabled()) {
					link = new Link(this, rc);
					activeLinks.put(radarID, link);
				} else
					// not necc. an error..
					Log.errorf("Attempt to access disabled radar '%s'", radarID);
			} else
				Log.errorf("Attempt to access unconfigured radar '%s'", radarID);
		}
		
		// Do not run event handlers inside synchronized to avoid deadlock
		if (link != null) {
			if (radarHandler != null)
				radarHandler.handleRadarEvent(new RadarEvent(RadarEvent.CONNECTION_ATTEMPT_STARTED,
						radarID));
			link.createLinkInBackground();
		} else 
			notifyConnectFailed(radarID); // So ConnectonManager can free a line
		
	}

	public void disconnectRadar(String radarID) {
		Link link;
		synchronized (activeLinks) {
			link = activeLinks.get(radarID);
			if (link == null)
				return;
		}
		activeLinks.remove(radarID);
		link.teardown();
	}

	protected RadarEventListener radarHandler;
	
	public RadarEventListener getEventHandler() {
		return radarHandler;
	}

	public void setEventHandler(RadarEventListener handler) {
		radarHandler = handler;
	}

	public Configuration getConfiguration() {
		return config;
	}
}
