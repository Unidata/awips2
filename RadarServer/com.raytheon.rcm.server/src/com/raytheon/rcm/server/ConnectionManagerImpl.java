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

import java.nio.ByteBuffer;
import java.util.ArrayDeque;
import java.util.HashMap;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.LinkResource;
import com.raytheon.rcm.config.LinkType;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.message.Message;


public class ConnectionManagerImpl extends RadarEventAdapter implements ConnectionManager, StatusManager {
	
	private enum State { UNCONNECTED, CONNECTING, CONNECTED }
	
	private static class Status implements StatusManager.RadarStatus {
		String radarID;
		State state = State.UNCONNECTED;
		LinkType linkTypeUsed;
		LinkResource linkResource;
		//ArrayDeque<byte[]> messageQueue = new ArrayDeque<byte[]>();
		byte[] currentAAP;
		byte[] currentGSM; 
		byte[] currentPTL;
		byte[] lastAAP;
		byte[] lastGSM; 
		byte[] lastPTL;
		
		public Status(String radarID) {
			this.radarID = radarID;
		}
		
		void setUnconnected() {
			state = State.UNCONNECTED;
			currentGSM = currentAAP = currentPTL = null;
		}
		
		@Override
		public byte[] getCurrentAAP() {
			return currentAAP;
		}
		@Override
		public byte[] getCurrentGSM() {
			return currentGSM;
		}
		@Override
		public byte[] getCurrentPTL() {
			return currentPTL;
		}
		@Override
		public byte[] getLastAAP() {
			return lastAAP;
		}
		@Override
		public byte[] getLastGSM() {
			return lastGSM;
		}
		@Override
		public byte[] getLastPTL() {
			return lastPTL;
		}
		@Override
		public LinkResource getLinkResource() {
			return linkResource;
		}
	}
	
	protected static class InterfaceStatus {
		LinkType linkType;
		int linesLeft;
		public InterfaceStatus(LinkType linkType, Configuration config) {
			this.linkType = linkType;
			linesLeft = 1;
		}
		boolean isLineAvailable() { return linesLeft > 0; }
		void reserveLine() { --linesLeft; }
		void releaseLine() { ++linesLeft; } // TODO: vs max.
	}
	
	protected RadarServer radarServer;
	protected LinkManager linkManager;
	protected HashMap<String, Status> st = new HashMap<String, Status>();
	protected HashMap<LinkType, InterfaceStatus> ifcSt = new
			HashMap<LinkType, InterfaceStatus>();
	protected ArrayDeque<String> connectReqQueue = new ArrayDeque<String>();
	
	private ConnectionManagerImpl() { }
	
	public ConnectionManagerImpl(RadarServer radarServer, LinkManager linkManager) {
		this.radarServer = radarServer;
		this.linkManager = linkManager;
	}

	public void handleRadarEvent(RadarEvent event) {
		Status status = getStatus(event.getRadarID());
		if (status == null)
			return; // TODO: log?

		if (event.getType() == RadarEvent.CONNECTION_UP) {
			status.state = State.CONNECTED;
			status.linkResource = event.getLinkResource();
		} else if (event.getType() == RadarEvent.CONNECTION_DOWN ||
				event.getType() == RadarEvent.CONNECTION_ATTEMPT_FAILED) {
			
			//status.state = State.UNCONNECTED;
			status.setUnconnected();

			Configuration config = getConfiguration();
			
			InterfaceStatus ifcStatus = getInterfaceStatus(status.linkTypeUsed);
			if (ifcStatus == null)
				return; // TODO: log?
			{
				// see where .reserveLine() is used
				RadarConfig rc = config.getConfigForRadar(event.getRadarID());
				if (rc != null && ! rc.isDedicated())
				synchronized (ifcStatus) {
					ifcStatus.releaseLine();
				}
			}
			
			// synchronized waitQueue
			for (String radarID : connectReqQueue) {
				Status otherStatus = getStatus(radarID);
				RadarConfig rc = config.getConfigForRadar(radarID);
				if (otherStatus == null || rc == null)
					continue;
				if (rc.getLinkType() == ifcStatus.linkType) {
					// ups.. it's not a dequeue.. or we need a separate queue
					// for each interface
					connectReqQueue.remove(radarID);
					connectRadar(radarID);
					break;
				}
			}
		} else if (event.getType() == RadarEvent.MESSAGE_RECEIVED) {
			byte[] msg = event.getMessageData();
			int code = Message.messageCodeOf(msg);
			
			switch (code) {
			case Message.GSM:
				status.currentGSM = status.lastGSM = msg; 
				break;
			case Message.ALERT_ADAPTATION_PARAMETERS: 
				status.currentAAP = status.lastAAP = msg; 
				break;
			case Message.PRODUCT_LIST:
				status.currentPTL = status.lastPTL = msg;
				break;
			default:
				// nothing
			}
		}
	}

	@Override
	public void connectRadar(String radarID) {
		// TODO: Ugh, now I'm duplicating the checks in LinkManager
		Status status = getStatus(radarID);
		RadarConfig rc = getConfiguration().getConfigForRadar(radarID);
		
		if (status != null && rc != null) {
			// TODO: synch global or status?
			if (status.state == State.CONNECTED || status.state == State.CONNECTING)
				return;
			else if (status.state == State.UNCONNECTED) {
				LinkType linkType = rc.getLinkType();
				if (linkType != null) {
					InterfaceStatus ifcStatus = getInterfaceStatus(linkType);
					if (ifcStatus == null)
						return; // TODO: what is this?
					boolean connect = false;
					synchronized (ifcStatus) {
						/* Logic/design shortcut: If a dedicated connection, just
						 * ignore the line count.  TCP connections are 'unlimited'
						 * anyway and there are no longer dedicated X25 connections.
						 * 
						 * Note the second rc.isDedicated check below.
						 */
						if (ifcStatus.isLineAvailable() || rc.isDedicated()) {
							status.state = State.CONNECTING;
							status.linkTypeUsed = linkType;
							if (! rc.isDedicated())
								ifcStatus.reserveLine();
							connect = true;
						}
					}
					// Do not run this inside synchronized(...)
					if (connect)
						getLinkManager().connectRadar(radarID);
					else
						connectReqQueue.addLast(radarID);
				} else {
					// TODO: this is another duplicated check and error
					Log.errorf("Link type for radar '%s' not configured", radarID);
				}
			}
		} else {
			Log.errorf("Attempt to access unconfigured radar '%s'", radarID);
			return;
		}
	}

	@Override
	public void disconnectRadar(String radarID) {
		getLinkManager().disconnectRadar(radarID);		
	}

	/** Sends a message to the specified radar.
	 * 
	 *  @param msg A Nexrad message.  If either the source or destination ID is 
	 *  zero, it will be changed to the appropriate value.  <code>msg</code>
	 *  must not be immutable in this case as its contents will be changed.
	 */
	@Override
	public void sendMessageToRadar(String radarID, byte[] msg) {
		Status status = getStatus(radarID);
		if (status != null) {
			Configuration config = getConfiguration();
			ByteBuffer buf = ByteBuffer.wrap(msg);
			if (buf.getShort(12) == 0)
				buf.putShort(12, (short) config.getPupId());
			if (buf.getShort(14) == 0)
				buf.putShort(14, (short) config.getConfigForRadar(radarID).getNexradID());
			getLinkManager().sendMessageToRadar(radarID, msg);
		} else {
			Log.errorf("Attempt to access unconfigured radar '%s'", radarID);
			return;
		}
	}

	@Override
	public RadarStatus getRadarStatus(String radarID) {
		return getStatus(radarID);
	}

	protected Configuration getConfiguration() {
		return radarServer.getConfiguration();
	}
	
	protected LinkManager getLinkManager() { return linkManager; }

	protected Status getStatus(String radarID) {
		Status status = st.get(radarID);
		if (status == null) {
			RadarConfig rc = getConfiguration().getConfigForRadar(radarID);
			if (rc != null) {
				status = new Status(radarID);
				st.put(radarID, status);
			}
		}
		return status;
	}
	
	protected InterfaceStatus getInterfaceStatus(LinkType linkType) {
		InterfaceStatus status = ifcSt.get(linkType);
		if (status == null) {
			if (linkType != null) {
				status = new InterfaceStatus(linkType, getConfiguration());
				ifcSt.put(linkType, status);
			}
		}
		return status;
	}

	@Override
	public void handleConfigEvent(ConfigEvent event) {
		String radarID = event.getRadarID(); 
		if (radarID != null) {
			if (event.getNewConfig() == null)
				disconnectRadar(radarID);
		}
	}

}
