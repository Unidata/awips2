package com.raytheon.uf.edex.dissemination;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import org.apache.camel.Header;

import com.raytheon.uf.common.dissemination.OUPResponse;

/**
 * Manages MHS acknowledgments.  Currently this means supporting synchronous
 * waiting for acknowledgments.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2012-04-13   DR 10388   D. Friedman Initial creation
 * 
 * </pre>
 * 
 */

public class OUPAckManager {
	
	private static final long MAX_ENTRY_LIFE_TIME_MILLIS = 15 * 60 * 1000; // 15 minutes
	private static final long ACK_WAIT_TIMEOUT_MILLIS = 5 * 60 * 1000; // 5 minutes
	
	private static final String ACK_RESPONSE = "ACK";
	private static final String NACK_RESPONSE = "NACK";
	private static final String NWWS_UPLINK_ADDRESS = "NWWSUP";
	private static final String[] EXCLUSIVE_ADDRESS_SEARCH_STRINGS = { "NCF", "SBN" };
	
	/*
	 * EDEX can receive an ACK before waitAck is called. Because the ACK cannot
	 * be correctly processed before the addresses are know (i.e., waitAck is
	 * called), we must queue all ACKs for processing on the thread that calls
	 * waitAck. This includes ACKs for messages that were not even sent from
	 * this JVM. If OUP requests and ACK/NACK messages could be processed
	 * serially in the same thread, this complexity could be reduced.
	 */
	private static class Entry {
		public Entry() {
			this.createTime = System.currentTimeMillis();
		}
		long createTime;
		String addresses;
		String result;
		ArrayList<Response> pendingResponses = new ArrayList<OUPAckManager.Response>(1);
		
		boolean isDone() {
			if (result != null)
				return true;
			while (pendingResponses.size() > 0) {
				Response r = pendingResponses.remove(0);
				if (matches(r)) {					
					result = r.response != null ? r.response : "(null response)";
					return true;
				}
			}
			return false;
		}
		
		/* Logic and comments from MhsWfoProduct.C: MhsWfoProduct::matches() */
		private boolean matches(Response r) {
			// Check for acks from the NWWS address.
			if (NWWS_UPLINK_ADDRESS.equals(r.sender)
					&& this.addresses.indexOf(NWWS_UPLINK_ADDRESS) >= 0)
				return true;

			/*
			 * If the message was sent to the NCF, then only handle acks from
			 * the NCF. If the message was sent to the NCF for distribution on
			 * the SBN, then only handle acks with the SBN address.
			 */
			for (String searchString : EXCLUSIVE_ADDRESS_SEARCH_STRINGS)
				if (this.addresses.indexOf(searchString) >= 0) {
					if (r.sender != null && r.sender.indexOf(searchString) >= 0)
						return true;
					else
						return false;
				}

			/*
			 * If none of the above special addresses were used, compare the
			 * sender to the list of addresses. If there's only one address, and
			 * the sender matches it, return true.
			 */
			if (addresses != null && addresses.equals(r.sender))
				return true;

			return false;
		}
	}

	private static class Response {
		private String sender;
		private String response;
		public Response(String sender, String response) {
			this.sender = sender;
			this.response = response;
		}
	}
	
	private HashMap<String, Entry> entries = new HashMap<String, OUPAckManager.Entry>();
	
	public Entry getEntry(String messageId) {
		synchronized (entries) {
			// Purge abandonded entries
			long purgeTime = System.currentTimeMillis() - MAX_ENTRY_LIFE_TIME_MILLIS;
			Iterator<Entry> i = entries.values().iterator();
			while (i.hasNext()) {
				Entry entry = i.next();
				if (entry.createTime <= purgeTime) {
					i.remove();
					synchronized (entry) {
						entry.result = "ACK wait purged";
						entry.notify();
					}
				}
			}
			
			Entry entry = entries.get(messageId);
			if (entry == null) {
				entry = new Entry();
				entries.put(messageId, entry);
			}
			return entry;
		}
	}
	
	/** Synchronously waits for an acknowledgment of the specified MHS message
	 * @param messageId MHS message ID
	 * @param addresses list of addresses used to determine which ACKs are relevant 
	 * @param response On return, the {@code acknowledged} and (if no positive ACK) {@code message}
	 *    properties will be set. 
	 * @param messageDescription description of the MHS message to be used in error messages
	 */
	public void waitAck(String messageId, String addresses, OUPResponse response, String messageDescription) {
		long now = System.currentTimeMillis();
		long targetTime = now + ACK_WAIT_TIMEOUT_MILLIS; 
		Entry entry = getEntry(messageId);

		synchronized (entry) {
			entry.addresses = addresses;
			while (! entry.isDone()) {
				now = System.currentTimeMillis();
				if (now < targetTime) {
					try {
						entry.wait(targetTime - now);
					} catch (InterruptedException e) {
						response.setAcknowledged(false);
						response.setMessage(String.format("Interrupted while waiting for acknowledgement of %s", 
								messageDescription));
						return;
					}
				} else {
					response.setAcknowledged(false);
					response.setMessage(String.format("Timed out waiting for acknowledgement of %s", 
							messageDescription));
					return;
				}
			}
			response.setAcknowledged(ACK_RESPONSE.equals(entry.result));
			if (! response.isAcknowledged())
				response.setMessage(String.format(
						"An error was received while sending %s to the WAN.\nA %s.",
						messageDescription,
						NACK_RESPONSE.equals(entry.result) ?
								"negative acknowledgment was received from the remote site" :
									"unknown response was received: " + entry.result));
		}

		synchronized (this) {
			entries.remove(messageId);
		}
	}
	
	public void processAck(String messageId, @Header("sender") String sender, 
			@Header("response") String response) {
		Entry entry = getEntry(messageId);
		synchronized (entry) {
			entry.pendingResponses.add(new Response(sender, response));
			entry.notify();
		}
	}
}
