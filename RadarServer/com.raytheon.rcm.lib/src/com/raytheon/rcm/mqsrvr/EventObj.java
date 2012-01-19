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

import javax.xml.bind.annotation.XmlSeeAlso;

import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.OtrEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.message.Message;


@XmlSeeAlso({RadarEvent.class, ConfigEvent.class, OtrEvent.class})
public class EventObj {
	public static RadarEvent filterRadarEvent(RadarEvent ev) {
		byte[] msg = ev.getMessageData();
		if (msg != null) {
			try {
				if (Message.messageCodeOf(msg) != Message.GSM)
					return null;
			} catch (RuntimeException e) {
				msg = null;
			}
			ev = new RadarEvent(ev.getType(), ev.getRadarID(), msg);
		}
		return ev;
	}
}
