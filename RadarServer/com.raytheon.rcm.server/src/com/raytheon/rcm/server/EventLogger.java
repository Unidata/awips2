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

import java.util.Arrays;

import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.RequestResponse;
import com.raytheon.rcm.message.GraphicProduct.PDB;

/**
 * A radar server component that logs various radar events.
 */
public class EventLogger extends RadarEventAdapter {
	
	public EventLogger() {
		
	}

	@Override
	public void handleRadarEvent(RadarEvent event) {
		switch (event.getType()) {
		case RadarEvent.CONNECTION_UP:
			Log.eventf("%s: connected", event.getRadarID());
			break;
		case RadarEvent.CONNECTION_DOWN:
			Log.eventf("%s: disconnected", event.getRadarID());
			break;
		case RadarEvent.MESSAGE_RECEIVED:
			{
				StringBuilder s = new StringBuilder();
				byte[] msg = event.getMessageData();
				int messageCode = Message.messageCodeOf(msg);
				s.append("code=" + messageCode);
				s.append(" size=" + msg.length);
				if (messageCode == Message.GSM) {
					s.append(' ');
					GSM gsm = null;
					try {
						gsm = GSM.decode(msg);
					} catch (Exception e) {
						s.append("(Message decoding failed)");
					}
					if (gsm != null)
						s.append(formatGSM(gsm));
				} else if (messageCode == Message.REQUEST_RESPONSE) {
					s.append(' ');
					RequestResponse rr = null;
					try {
						rr = RequestResponse.decode(msg);
					} catch (Exception e) {
						s.append("(Message decoding failed)");
					}
					if (rr != null)
						s.append(formatPRR(rr));
				} else if (messageCode >= 16) {
					PDB pdb = null;
					
					s.append(' ');
					try {
						pdb = GraphicProduct.pdbOfMessage(msg);
					} catch (Exception e) {
						s.append("(Message decoding failed)");
					}
					if (pdb != null)
						s.append(String.format("elev=%.1f sequence=%d"+
								" vs=%3$tY-%3$tm-%3$td %3$tH:%3$tM:%3$tS #%4$d",
								pdb.getElevationAngle() / 10.0, pdb.sequence,
								pdb.volumeScanTime, pdb.volumeScan));
				}
					
				Log.eventf("%s: message %s", event.getRadarID(), s.toString());
				break;
			}
		}
	}

	protected final String[] rdaOpStatusStr = {
			"auto-calib-disab", "online", "maint-req", "maint-mand", "cmd-shutdown",
			"inoperable", null, "wideband-disconn"
	};
	protected final String[] rdaStatusStr = {
			null, "startup", "standby", "restart", "operate", null, "offline-op"
	};
	protected final String[] rdaAlarmStr = {
			"indeterminate", "tower", "pedestal", "transmitter", "receiver", "control", "comms"	
	};
	protected final String[] dteStr = {
			null, "none", "refl", "vel", "sw", "dual-pol"
	};
	protected final String[] rpgOpStr = {
			"load-shed", "online", "maint-req", "maint-mand", "cmd-shutdown"
	};
	protected final String[] rpgAlarmStr = {
			"none", "node-conn", null, "ctl-task-fail", "db-fail", null, "input-load-shed", 
			null, "store-load-shed", null, null, null, "link-fail", "redundant-channel-error",
			"task-fail", "media-fail"
	};
	protected final String[] rpgStatusStr = {
			"restart", "operate", "standby", null, "test-mode"
	};
	protected final String[] productAvailStr = {
		"avail", "degraded", "not-avail"	
	};
	protected final String[] prrStr = {
			"no-such-msg", "no-such-prod", "not-gen", "proc-fault",
			"narrowband-loadshed", "illegal-req", "mem-loadshed", "cpu-loadshed",
			"slot-unavail", "task-failed", "task-unavail", "avail-next-scan",
			"moment-disabled", "invalid-password", null, "aborted-scan",
			"inval-prod-param", "data-seq-error", "task-term"
	};
	
	protected String formatBits(short bits, String[] strings) {
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < 16; i++) {
			if ((bits & (1 << i)) != 0) {
				if (result.length() > 0)
					result.append(',');
				String s = null;
				if (i < strings.length)
					s = strings[i];
				if (s == null)
					s = "unk" + Integer.toString(15 - i);
				result.append(s);
			}
		}
		return result.toString();
	}
	
	protected String formatPrrBits(int bits, String[] strings) {
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < 32; i++) {
			// PRR bits are defined from the MSB on down, so
			// note the (31-i)
			if ((bits & (1 << (31-i))) != 0) {
				if (result.length() > 0)
					result.append(',');
				String s = null;
				if (i < strings.length)
					s = strings[i];
				if (s == null)
					s = "unk" + Integer.toString(31 - i);
				result.append(s);
			}
		}
		return result.toString();
	}
	
	protected String formatGSM(GSM gsm) {
		StringBuilder o = new StringBuilder();
		String s;

		switch (gsm.opMode) {
		case GSM.OP_MODE_CLEAR_AIR: s = "clear-air"; break;
		case GSM.OP_MODE_STORM: s = "storm"; break;
		case GSM.OP_MODE_MAINTENANCE: s = "maintenance"; break;
		default: s = "(" +Integer.toString(gsm.opMode) + ")";
		}
		o.append("opMode=" + s);
		
		o.append(" vcp=" + gsm.vcp);
		o.append(" cuts=" + Arrays.toString(gsm.cuts));
		
		o.append(String.format(" rdaOp=%s rdaStat=%s rdaAlarm=%s dte=%s rpgOp=%s rpgStat=%s rpgAlarm=%s", 
				formatBits((short) gsm.rdaOpStatus, rdaOpStatusStr),
				formatBits((short) gsm.rdaStatus, rdaStatusStr),
				formatBits((short) gsm.rdaAlarms, rdaAlarmStr),
				formatBits((short) gsm.dataAvailability, dteStr),
				formatBits((short) gsm.rpgOpStatus, rpgOpStr),
				formatBits((short) gsm.rpgStatus, rpgStatusStr),
				formatBits((short) gsm.rpgAlarms, rpgAlarmStr)));
		
		o.append(String.format(" avail=%s", 
				formatBits((short) gsm.productAvailability, productAvailStr)));
		
		o.append(String.format(" rdaVer=%.1f rpgVer=%.1f", gsm.rdaVersion/10.0, gsm.rpgVersion/10.));
		
		return o.toString();
	}
	
	protected String formatPRR(RequestResponse rr) {
		StringBuilder o = new StringBuilder();
		
		o.append(String.format("productCode=%d sequence=%d elev=%d flags=%s",
				rr.productCode,
				rr.sequence,
				rr.elevationAngle,
				formatPrrBits(rr.errorCode, prrStr)));
		
		return o.toString();
	}

}
