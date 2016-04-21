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
package com.raytheon.rcm.event;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import com.raytheon.rcm.config.LinkResource;

/** Describes events that can occur within the Radar Server. */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class RadarEvent {
	public static final int CONNECTION_UP = 1;
	public static final int CONNECTION_DOWN = 2;
	public static final int MESSAGE_RECEIVED = 3;

	public static final int CONNECTION_ATTEMPT_STARTED = 4;
	public static final int CONNECTION_ATTEMPT_FAILED = 5;

	private int type;
	private String radarID;
	private byte[] messageData;
	// TODO: Cache decoded message?

	/*
	 * Transient because this field is currently only used to let the
	 * ConnectionManger know which link is being used so that the RPSListManager
	 * can know what the maximum RPS list size is. May do this another way in
	 * the future and there is no need for clients to use it now.
	 */
	@XmlTransient
	private LinkResource linkResource;

	public RadarEvent() {

	}

	public RadarEvent(int type, String radarID) {
		this.type = type;
		this.radarID = radarID;
	}

	public RadarEvent(int type, String radarID, byte[] messageBuffer) {
		this.type = type;
		this.radarID = radarID;
		this.messageData = messageBuffer;
	}

	public RadarEvent(int type, String radarID, LinkResource linkResource) {
		this.type = type;
		this.radarID = radarID;
		this.linkResource = linkResource;
	}

	public final int getType() {
		return type;
	}

	public final String getRadarID() {
		return radarID;
	}

	public final byte[] getMessageData() {
		return messageData;
	}

	public final LinkResource getLinkResource() {
		return linkResource;
	}

}
