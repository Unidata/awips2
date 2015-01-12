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
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import com.raytheon.rcm.alertreq.AlertRequestDefinition;
import com.raytheon.rcm.config.Globals;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.rmr.MultipleRequest;


/**
 * Request messages that can be sent to the RadarServer.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ...
 * 2014-02-03   DR 14762   D. Friedman Add SendConfigFile
 * </pre>
 *
 */
@XmlSeeAlso({ReqObj.GetRadarList.class, 
	ReqObj.GetRadarConfig.class, ReqObj.SetRadarConfig.class,
	ReqObj.GetRadarStatus.class, ReqObj.GetRadarStatusMessages.class, 
	ReqObj.SendOneTimeRequests.class, ReqObj.DebugCommand.class, 
	ReqObj.DebugHandleMessage.class,
	ReqObj.SendRpsList.class, ReqObj.SendRpsListData.class,
	ReqObj.GetRpsList.class,
	ReqObj.GetGlobalConfig.class, ReqObj.SetGlobalConfig.class,
	ReqObj.ActivateRMR.class, ReqObj.CancelRMR.class, 
	ReqObj.GetActiveRMRs.class, ReqObj.GetAlertRequest.class,
	ReqObj.SendAlertRequest.class, ReqObj.SendMessageToRPG.class,
	ReqObj.SendConfigFile.class})
public class ReqObj {
	@XmlRootElement
	public static class GetRadarList extends ReqObj {		
	}
	
	public static class RadarReq extends ReqObj { 
		public String radarID;
		public String toString() { 
			return String.format("%s(%s)", getClass().getSimpleName(), radarID); 
		}
	}
	
	@XmlRootElement
	public static class GetRadarConfig extends RadarReq {
	}
	@XmlRootElement
	public static class SetRadarConfig extends RadarReq {
		// TODO: Action action = ADD | REMOVE | MODIFY (or null)
		public RadarConfig config;
	}
	@XmlRootElement
	public static class GetRadarStatus extends RadarReq {
	}
	@XmlRootElement
	public static class GetRadarStatusMessages extends RadarReq {
	}
	@XmlRootElement
	public static class SendOneTimeRequests extends ReqObj {
		@XmlList
		public List<String> radarIDs = new ArrayList<String>();
		@XmlElement(name="request")
		public List<Request> requests = new ArrayList<Request>();
		public String toString() { 
			return String.format("%s(%s, ...)", getClass().getSimpleName(), radarIDs); 
		}
	}

	public static class RpsListReq extends ReqObj {
		@XmlList
		public List<String> radarIDs = new ArrayList<String>();
		public int vcp;
		public Boolean store;
		public boolean isStore() { return store != null && store.booleanValue(); }
		public String toString() { 
			return String.format("%s(%s, vcp=%d, store=%s,...)", 
					getClass().getSimpleName(), radarIDs, vcp, isStore()); 
		}		
	}
	@XmlRootElement
	public static class SendRpsList extends RpsListReq {
		@XmlElement(name="request")
		public List<Request> requests = new ArrayList<Request>();
	}
	@XmlRootElement
	public static class SendRpsListData extends RpsListReq {
		public byte[] listData;
	}
	@XmlRootElement
	public static class GetRpsList extends RadarReq {
		public Integer vcp; // or null for current list
		public Integer opMode; // TODO: should be removed eventually
	}
	
	public static abstract class GlobalConfigReq extends ReqObj {
		public Globals global;
	}
	@XmlRootElement
	public static class GetGlobalConfig extends GlobalConfigReq {
		// nothing else
	}
	@XmlRootElement
	public static class SetGlobalConfig extends GlobalConfigReq {
		// nothing else
	}
	
	@XmlRootElement
	public static class SendConfigFile extends ReqObj {
	    public String fileName;
	    public byte[] fileData;
	    public String toString() {
	        return String.format("%s(%s, ...)",
	            getClass().getSimpleName(), fileName);
	    }
	}

	@XmlRootElement
	public static class ActivateRMR extends ReqObj {
		public MultipleRequest multipleRequest;
	}
	@XmlRootElement
	public static class CancelRMR extends ReqObj {
		public String requestName;
	}
	@XmlRootElement
	public static class GetActiveRMRs extends ReqObj {
		// nothing
	}
	
	@XmlRootElement
	public static class GetAlertRequest extends RadarReq {
		public int areaIndex;
	}
	@XmlRootElement
	public static class SendAlertRequest extends RadarReq {
		public int areaIndex;
		public AlertRequestDefinition alertRequest;
	}
	
	@XmlRootElement
	public static class SendMessageToRPG extends RadarReq {
		public byte[] message;
	}
	
	@XmlRootElement
	public static class DebugCommand extends ReqObj {
		public enum Command { NOP, LOG_OTR_STATUS }
		@XmlAttribute(required=true)
		public Command command;
		public String toString() { 
			return String.format("%s(%s)", getClass().getSimpleName(), command); 
		}
	}
	@XmlRootElement
	public static class DebugHandleMessage extends RadarReq {
		public byte[] message;
	}
	
	public ReqObj() { }
	
	public String toString() { return getClass().getSimpleName(); }
	
	public static ReqObj getRadarConfig(String radarID) {
		GetRadarConfig r = new GetRadarConfig();
		r.radarID = radarID;
		return r;
	}
	public static ReqObj sendOTRs(List<String> radarIDs, List<Request> requests) {
		SendOneTimeRequests r = new SendOneTimeRequests();
		r.radarIDs.addAll(radarIDs);
		r.requests.addAll(requests);
		return r;
	}
	public static ReqObj debugHandleMessage(String radarID, byte[] message) {
		DebugHandleMessage r = new DebugHandleMessage();
		r.radarID = radarID;
		r.message = message;
		return r;
	}
}
