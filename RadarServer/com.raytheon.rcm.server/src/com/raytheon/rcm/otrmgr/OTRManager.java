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
package com.raytheon.rcm.otrmgr;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import com.raytheon.rcm.event.OtrEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.Util;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.message.MD;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.MessageFormatException;
import com.raytheon.rcm.message.MessageInfo;
import com.raytheon.rcm.message.ProductRequest;
import com.raytheon.rcm.message.RequestResponse;
import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.request.Filter;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.request.Sequence;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.server.RadarServer;


/*
 * TODO: timeout requests
 * TODO: notify requester (delegate?)
 * TODO: handle PRRs
 */

/**
 * Manages One Time Requests for the RPGs.
 * <p>
 * Does not actually do much except provide a place to queue up requests while
 * waiting to connect to the RPG.  Does do some coalescing of duplicate 
 * requests.
 */
public class OTRManager extends RadarEventAdapter {
	
	protected class RadarStatus {
		protected String radarID;
		protected boolean isReady; // isConnected+goGsm: because there is not ConnectionManager.isConnected
		protected List<Req> requests = new ArrayList<Req>();
		protected GSM lastGSM;
		
		public RadarStatus(String radarID) {
			this.radarID = radarID;
		}
		
		public RadarConfig getRadarConfig() {
			return radarServer.getConfiguration().getConfigForRadar(radarID);
		}
		
		@Override
		public String toString() {
			StringBuilder s = new StringBuilder();
			s.append("{ ").append(radarID).append(! isReady ? " not " : " ").
					append("ready\n");
			for (Req r : requests)
				s.append("  ").append(r.toString()).append('\n');
			s.append('}');
			
			return s.toString();
		}

		public void handleMessage(byte[] messageBuffer) {
			int code = Message.messageCodeOf(messageBuffer);
			if (code == Message.GSM) {
				try {
					lastGSM = (GSM) MD.decode(messageBuffer);
			    } catch (MessageFormatException e) {
			    	return;
			    }
				if (! isReady) {
					isReady = true;
					trySendingRequests();
				}
			} else if (code == Message.REQUEST_RESPONSE) {
				RequestResponse rr = null;
				try {
					rr = (RequestResponse) MD.decode(messageBuffer);
				} catch (Exception e) { // TODO: correct exception
					return;
				} 
				Iterator<Req> i = requests.iterator();
				while (i.hasNext()) {
					Req r = i.next();
					if (r.sent && r.sequence == rr.sequence) {
						boolean done = false;
						
						/* From Class 1 ICD section 3.2.2.7 (Request Response
						 * Message):  These two errors indicate the product
						 * will be sent in the next scan.
						 */
						if ((rr.errorCode &
						        ~(RequestResponse.AVAILABLE_NEXT_SCAN |
						                RequestResponse.OTR_PROCESS_FAULTED)) != 0) {
                            r.nExpected--;
                            // TODO: correct logic
                            if (r.nExpected < 1) {
                                i.remove();
                                done = true;
                            }
                        }
						
						OtrEvent ev = new OtrEvent();
						ev.radarID = radarID;
						ev.request = r.request;
						ev.product = messageBuffer;
						ev.done = done;
						r.sendOtrEvent(ev);
						
						break;
					}
				}
			} else if (code >= 16) { // TODO: isProduct
				PDB pdb = null;
				try {
					pdb = GraphicProduct.pdbOfMessage(messageBuffer);
				} catch (MessageFormatException e) {
					return;
				}
				Iterator<Req> i = requests.iterator();
				while (i.hasNext()) {
					Req r = i.next();
					if (r.sent && r.sequence == pdb.sequence) {
						r.nExpected--;
						// TODO: correct logic
						boolean done = false;
						if (r.nExpected < 1) {
							done = true;
							i.remove();
						}
						
						OtrEvent ev = new OtrEvent();
						ev.radarID = radarID;
						ev.request = r.request;
						ev.done = done;
						ev.product = messageBuffer;
						r.sendOtrEvent(ev);
						break;
					}
				}
			} else
				return; // nothing has changed
			
			if (requests.isEmpty()) {
				/* The only reason we contact dial radars is for
				 * OTRs so this is okay for now.
				 */
				if (! getRadarConfig().isDedicated()) {
					// TODO: connectionRequest.cancel() or
					//   connectionManager.idleDisconnect(radarID, 5 seconds)
					
					radarServer.getConnectionManager().disconnectRadar(radarID);
				}
			}
		}
		
		public void sendRequests(Iterable<Request> requests, OTRHandler handler) {
		    // Add new requests to the queue, filtering out duplicates.
			synchronized (this.requests) {
				
				Lsrc:
				for (Request src : requests) {
					Filter filter = Filter.getFilterForCode(src.productCode);
					for (Req r : this.requests) {
					    /* This could be a "merge" rather than a "filter".
					     * Although exact comparison of fields such as
					     * elevation and repeat count produce safe results,
					     * it does not filter out all possible duplicates.
					     */
						if (filter.requestsEqual(src, r.request, null/*TODO: ...*/,
						        Util.getRadarType(getRadarConfig()))) {
							if (handler != null)
								r.addHandler(handler);
							continue Lsrc;
						}
					}
	
					Log.eventf("Queuing request %s for %s", src, radarID);
					Req r = new Req((Request) src.clone());
					if (handler != null)
						r.addHandler(handler);
					this.requests.add(r);
				}
			}
		
			trySendingRequests();
		}
		
		private void trySendingRequests() {
			if (isReady()) {
				ArrayList<Request> requestsToSend = new ArrayList<Request>();
				long now = System.currentTimeMillis();
				synchronized (this.requests) {
					for (Req r : requests) {
						if (! r.sent) {
							r.sent = true;
							r.sequence = r.request.sequence = Sequence.next(radarID);
							r.timeSent = now;
							r.calculateNExpected();
							Log.eventf("Sending request %s to %s", r.request, radarID);
							requestsToSend.add(r.request);
						}
					}
				}
				if (requestsToSend.size() < 1)
					return; // Otherwise, we would cancel the RPS list.
				byte[] msg = ProductRequest.encode(requestsToSend.toArray(new Request[0])); 
				radarServer.getConnectionManager().sendMessageToRadar(radarID, msg);
			} else {
				boolean anyToSend = false;
				synchronized (this.requests) {
					for (Req r : requests)
						if (! r.sent) {
							anyToSend = true;
							break;
						}
				}
				if (anyToSend)
					radarServer.getConnectionManager().connectRadar(radarID);
			}
		}

		private boolean isReady() {
			return this.isReady;
		}

		public void handleUp() {
			isReady = false; // Not ready until a GSM is received
		}
		
		public void handleDown() {
			isReady = false;
			// TODO: Maybe just clear those that have been sent.
			requests.clear();
		}

		private class Req {
			public Request request;
			public boolean sent;
			public long timeSent;
			int sequence; // redundant?
			int nExpected; // something request.repeatCount * countOf(request)
			public ArrayList<OTRHandler> handlers;
			// countOf() isElevationProduct -- OH NO: getMultiCount() or 1
			
			public Req(/*String radarID, */Request request) {
				//this.radarID = radarID;
				this.request = request;
				// The other fields cannot be sent until we send the product.
			}
			
			@Override
			public String toString() {
				return String.format("{ req=%s \n    sent=%s %s seq=%d expected=%d }",
						request, sent, sent ? new Date(timeSent) : "", sequence, nExpected);
			}
			
			public void calculateNExpected() {
				int nElevations = 1;
				boolean exactCountUnknown = false; // TODO: use this
				
				/* If the request is for multiple elevations, try to determine
				 * how many elevations will be returned.  Note AWIPS 1 does 
				 * not do this and disconnects (from dial radars) after the
				 * first product is received. 
				 */
				if (MessageInfo.isElevationBasedProduct(request.productCode) &&
						request.getElevationSelection() != Request.SPECIFIC_ELEVATION) {
					if (lastGSM != null) {
						if (request.getElevationSelection() == Request.ALL_ELEVATIONS) {
							/* We do not get information about duplicate elevations.  Could
							 * put in TDWR-specific knowledge.  If vcp==80 && is-low-elevation...
							 * 
							 * But probably needs something like. nExpectedUnknown = true
							 * and (if nExpectedUnknown then connMgr.idleDisconnectRadar(...)
							 */
							// if is tdwr and vcp80...
							exactCountUnknown = true;
							
							nElevations = request.getElevationAngle() == 0 ?
									lastGSM.cuts.length : 1;
						} else if (request.getElevationSelection() == Request.N_ELEVATIONS) {
							nElevations = Math.min(lastGSM.cuts.length, 
									request.getElevationAngle());
						} else if (request.getElevationSelection() == Request.LOWER_ELEVATIONS) {
							nElevations = 0;
							int reqEA = request.getElevationAngle();
							for (int ea : lastGSM.cuts) {
								if (ea <= reqEA)
									++nElevations;
								else
									break;
							}
						} else
							exactCountUnknown = true;
					} else {
						Log.warnf("Do not have the latest GSM for '%s'.  Cannot determine how many products to expect for multi-elevation OTR",
								radarID);
					}
				}
				nExpected = request.count * nElevations;
			}
			
			public void addHandler(OTRHandler handler) {
				if (handlers == null)
					handlers = new ArrayList<OTRHandler>();
				handlers.add(handler);
			}

			public void sendOtrEvent(OtrEvent event) {
				if (handlers != null)
					for (OTRHandler handler : handlers) {
						try {
							handler.handleOtrEvent(event);
						} catch (Exception e) {
							Log.errorf("Error while handling OTR event: %s", e);
						}
					}
			}

		}
	}
	
	
	/*public static final int SUCCESS = 1;
	public static final int FAILED = 2;
	public static final int TIMED_OUT = 3;
	public static final int PRR = 4;
	
	interface Handler {
		void handleRequestResult(Object cookie, String radarID, Request request, int result, byte[] msg);
	}*/
	
	
	RadarServer radarServer;
	//ArrayList<Req> requests = new ArrayList<Req>();
	HashMap<String,RadarStatus> state = new HashMap<String,RadarStatus>();
	
	public OTRManager(RadarServer radarServer) {
		this.radarServer = radarServer;
	}
	
	public void sendOneTimeRequests(Iterable<String> radarIDs, Iterable<Request> requests, 
			OTRHandler handler) {
		for (String radarID : radarIDs) {
			RadarStatus rs = getRadarStatus(radarID);
			if (rs != null)
				rs.sendRequests(requests, handler);
		}
	}
	
	// Can still return null if invalid radarID
	protected RadarStatus getRadarStatus(String radarID) {
		return getRadarStatus(radarID, true);
	}

	protected RadarStatus getRadarStatus(String radarID, boolean create) {
		RadarStatus rs = state.get(radarID);
		if (rs == null && create) {
			RadarConfig rc = radarServer.getConfiguration().getConfigForRadar(radarID);
			if (rc == null) {
				Log.errorf("Request to send OTR to unconfigured radar '%s'", radarID);
				return null;
			}
			
			rs = new RadarStatus(radarID);
			state.put(radarID, rs);
		}
		return rs;
	}

	@Override
	public void handleRadarEvent(RadarEvent event) {
		RadarStatus rs = getRadarStatus(event.getRadarID(), true/*false*/); // need to create status so we can track state
		//... less necessary if we have ConnMgr.isConnected ... not neccary if we have
		// ConnMgr.getRadarState().getCurrentGsm()
		if (rs == null)
			return;
		if (event.getType() == RadarEvent.MESSAGE_RECEIVED) {
			rs.handleMessage(event.getMessageData());
		} else if (event.getType() == RadarEvent.CONNECTION_DOWN) {
			rs.handleDown();
		} else if (event.getType() == RadarEvent.CONNECTION_UP) {
			rs.handleUp();
		}
	}
	
	public void logStatus() {
		StringBuilder s = new StringBuilder();
		s.append("{ OTRManager status\n");
		for (RadarStatus rs : state.values()) {
			s.append(" ").append(rs.toString()).append(" \n");
		}
		s.append("}");
		Log.event(s.toString());
	}
}
