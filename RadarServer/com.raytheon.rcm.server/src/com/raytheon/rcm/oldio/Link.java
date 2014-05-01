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

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

import com.raytheon.rcm.config.*;
import com.raytheon.rcm.message.SignOn;
import com.raytheon.rcm.server.Log;


/**
 * Manages an active connection to an RPG which may consist of more than one
 * socket connection.
 * 
 * <p>
 * Currently only supports TCP connections.
 */
public class Link {
	
    public static final int DEDICATED_MESSAGE_READ_TIMEOUT = 90 * 1000;
    public static final int DIAL_MESSAGE_READ_TIMEOUT = 15 * 1000;

	private LinkManager manager;
	private RadarConfig config;
	private TcmConnection[] tcmConnections;
	
	protected boolean isConnected;
	protected boolean isDisconnected;
	protected boolean teardownInProgress;
	
	public Link(LinkManager manager, RadarConfig config) {
		this.manager = manager;
		this.config = config;
		
		int nConnections = config.isDedicated() ? 2 : 1;
		tcmConnections = new TcmConnection[nConnections];
		for (int i = 0; i < nConnections; ++i)
			tcmConnections[i] = new TcmConnection(this);
	}
	
	public LinkManager getManager() {
		return manager;
	}
	
	public RadarConfig getConfig() {
		return config;
	}
	
	public TcmConnection[] getTcmConnections() {
		return tcmConnections;
	}
	
    public boolean isTeardownInProgress() {
        return teardownInProgress;
    }
    
	public void createLinkInBackground() {
		Thread t = new Thread(new Runnable() {

			public void run() {
				createLink();
			}
			
		});
		t.setDaemon(true);
		t.start();
	}
	
	private SocketAddress createSocketAddress(LinkResource lr) {
		String address = lr.getLinkAddress();
		String host = null;
		int port = 0;
		int pos = address.lastIndexOf(':');
		if (pos >= 0) {
			host = address.substring(0, pos);
			port = Integer.parseInt(address.substring(pos + 1));
			return new InetSocketAddress(host, port);
		} else
			throw new IllegalArgumentException(
					String.format("Invalid link address \"%s\"", address));
	}
	
	protected void createLink() {
		/* Managing the login process on separate threads is overly 
		 * complex.  There should not be any problem working 
		 * sequentially. 
		 */
		
		/* Need to make all connections and then send the login message.  
		 * Only then will a LOGIN_ACK be returned on the first connection.
		 */
		LinkType linkType = config.getLinkType();
		if (linkType == null) {
			Log.errorf("Link type for radar '%s' not configured", config.getRadarID());
			manager.notifyConnectFailed(config.getRadarID());
			return;
		}

		int nTried = 0;

		Llink:
		for (LinkResource lr : config.getLinkResources()) {
			if (lr.getLinkType() != linkType || 
					lr.isDedicated() != config.isDedicated())
				continue;
			
			++nTried;
			
			Exception e = null;
			int maxConnected = 0;
			
			try {
				for (int i = 0; i < tcmConnections.length; ++i) {
					/* if (config.getLinkType() == LINK_X25_DIAL ) ... // did wfoApi make two conns? 
					 *   (and multiplex over the link?) 
					 *   if (simpactProcs == null) simpactProcs = new Process[nConnection] 
					 *     spawn("simpactProxy -n $radarID -d 1234567 -l $logdir/blah");
					 *   readLine() -> socketaddr,cookie?
					 */
					TcmConnection tc = tcmConnections[i];
					try {
						Log.eventf("%s: Attempt connection to %s", config.getRadarID(), 
								lr.getLinkAddress());
						tc.connect(createSocketAddress(lr)); // includes timeout...
						Log.eventf("%s: Connected to %s", config.getRadarID(), 
								lr.getLinkAddress());
						maxConnected = i;
						// It's a secret: CODE no longer actually checks the link index
						tc.sendLogin(String.format("%d %d %d %s\0", lr.getLinkIndex(),
								tcmConnections.length, i, lr.getTcmPassword()).getBytes());
					} catch (RuntimeException e2) {
						e = e2;
						continue Llink;
					} catch (IOException e2) {
						e = e2;
						continue Llink;
					}
				}
				
				try {
					tcmConnections[0].waitLoginAck();
				} catch (IOException e2) {
					e = e2;
					continue Llink;
				}
	
				if (! config.isDedicated()) {
					// Send Class 2 user sign-on message
					tcmConnections[0].sendMessageFirst(SignOn.encode(getPupId(), 
							config.getNexradID(), lr.getUserPassword(), 
							lr.getPortPassword(), false));
				}

				notifyConnected(lr);

				int ci = 1;
				for (TcmConnection tcm : tcmConnections) {
					String threadName = 
						String.format("%s #%d", config.getRadarID(), ci++); 
					tcm.setMessageReceiveTimeout(config.isDedicated() ? 
					        DEDICATED_MESSAGE_READ_TIMEOUT : DIAL_MESSAGE_READ_TIMEOUT);
					tcm.startThreads(threadName);
				}

				return;
			} finally {
				if (e != null) {
					Log.eventf("%s: Connect/login failed: %s", config.getRadarID(), e);
					for (int j = 0; j <= maxConnected; ++j)
						tcmConnections[j].close();
				}
			}
		}
	
		if (nTried == 0) {
			Log.errorf("No %slinks defined for radar '%s'", 
					config.getLinkResources().length > 0 ? "usable " : "",
							config.getRadarID());
		} else {
			Log.eventf("All usable links to radar '%s' failed", config.getRadarID());
		}
		
		manager.notifyConnectFailed(config.getRadarID());
	}
	
	public synchronized void teardown() {
		if (teardownInProgress)
			return;
		teardownInProgress = true;
		
		Log.debugf("Tearning down connection");
		
		if (tcmConnections != null) {
			for (TcmConnection tcm : tcmConnections) {
				tcm.close();
				tcm.stopThreads();
			}
			
			if (isConnected)
				notifyDisconnected();
		}
	}

	public void sendMessage(byte[] msg) {
		tcmConnections[0].sendMessage(msg);
	}
	
	public void sendKeepAlives() {
        for (TcmConnection tcm : tcmConnections)
            tcm.sendKeepAlive();
	}
	
	protected void notifyConnected(LinkResource linkResource) {
		if (! isConnected) {
			isConnected = true;
			manager.notifyConnected(config.getRadarID(), linkResource);
		}
	}
	
	protected void notifyDisconnected() {
		if (! isDisconnected) {
			isDisconnected = true;
			manager.notifyDisconnected(config.getRadarID());
		}
	}
	
	protected int getPupId() {
		return manager.getConfiguration().getPupId();
	}
}
