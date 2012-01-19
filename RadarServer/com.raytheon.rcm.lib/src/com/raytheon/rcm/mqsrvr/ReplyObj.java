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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.rcm.alertreq.AlertRequestDefinition;
import com.raytheon.rcm.config.Globals;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.rmr.ActiveRequest;


@XmlRootElement(name="reply")
@XmlSeeAlso({ReplyObj.RadarsReply.class, ReplyObj.ConfigReply.class,
	ReplyObj.StatusMessagesReply.class, ReplyObj.RpsListReply.class,
	ReplyObj.GlobalConfigReply.class, ReplyObj.RmrReply.class,
	ReplyObj.AlertReqReply.class})
public class ReplyObj {
	@XmlAttribute(required=false)
	public String error;
	
	public ReplyObj() {
		
	}

	public ReplyObj(String error) {
		this.error = error;
	}

	@XmlRootElement
	public static class RadarsReply extends ReplyObj {
		@XmlElement
		@XmlList
		public Collection<String> radarIDs = new ArrayList<String>();
	}
	@XmlRootElement
	public static class ConfigReply extends ReplyObj {
		public Collection<RadarConfig> config = new ArrayList<RadarConfig>();
	}

	@XmlType(name="status",propOrder={})
	@XmlAccessorType(XmlAccessType.FIELD)
	public static class ROStatus {
		@XmlAttribute(name="name")
		public String radarID;
		public byte[] currentAAP;
		public byte[] currentGSM; 
		public byte[] currentPTL;
		public byte[] lastAAP;
		public byte[] lastGSM; 
		public byte[] lastPTL;
		
		public ROStatus() {
		}
	}
	
	@XmlRootElement
	@XmlAccessorType(XmlAccessType.FIELD)
	public static class StatusMessagesReply extends ReplyObj {
		public Collection<ROStatus> status = new ArrayList<ROStatus>();

		public StatusMessagesReply() { }
		public StatusMessagesReply(ROStatus status) {
			this.status = Arrays.asList(status);
		}
		public StatusMessagesReply(Collection<ROStatus> status) {
			this.status = new ArrayList<ROStatus>(status);
		}
	}
	
	@XmlRootElement
	public static class RpsListReply extends ReplyObj {
		public RpsList rpsList;
		public RpsListReply() { }
		public RpsListReply(RpsList rpsList) {
			this.rpsList = rpsList;
		}
	}
	
	@XmlRootElement
	public static class RmrReply extends ReplyObj {
		@XmlElement(name="activeRequest")
		public Collection<ActiveRequest> list = new ArrayList<ActiveRequest>();
	}
	
	@XmlRootElement
	public static class GlobalConfigReply extends ReplyObj {
		public Globals global;
	}
	
	@XmlRootElement
	public static class AlertReqReply extends ReplyObj {
		public AlertRequestDefinition alertRequest;
	}

	public static ReplyObj toGetRadarList(Collection<String> radarIDs) {
		RadarsReply r = new RadarsReply();
		r.radarIDs.addAll(radarIDs);
		return r;
	}
	public static ReplyObj toGetRadarConfig(RadarConfig radarConfig) {
		ConfigReply r = new ConfigReply();
		r.config = Arrays.asList(radarConfig);
		return r;
	}
	public static ReplyObj toGetRadarConfig(Collection<RadarConfig> radarConfig) {
		ConfigReply r = new ConfigReply();
		r.config = radarConfig;
		return r;
	}
	public static ReplyObj toGetRadarStatusMessages(ROStatus radarStatus) {
		StatusMessagesReply r = new StatusMessagesReply(radarStatus);
		return r;
	}
	public static ReplyObj toGetRadarStatusMessages(Collection<ROStatus> radarStatus) {
		StatusMessagesReply r = new StatusMessagesReply(radarStatus);
		return r;
	}
	public static ReplyObj error(String message) {
		ReplyObj r = new ReplyObj();
		r.error = message;
		return r;
	}
}
