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
package com.raytheon.uf.viz.radarapps.alertreq.impl;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.mqsrvr.ReplyObj.AlertReqReply;
import com.raytheon.rcm.mqsrvr.ReqObj.GetAlertRequest;
import com.raytheon.uf.viz.radarapps.core.RadarApps;

/** Coordinates editing of alert request between the threshold editor window
 * and alert area layers.
 */
public class AlertRequestEditing {
	
	public static class Sel {
		private String radarID;
		private int areaIndex;
		public Sel(String radarID, int areaIndex) {
			this.radarID = radarID;
			this.areaIndex = areaIndex;
		}
		public String getRadarID() {
			return radarID;
		}
		public int getAreaIndex() {
			return areaIndex;
		}
	}
	
	private HashMap<String,AlertRequestDoc[]> docs = 
		new HashMap<String, AlertRequestDoc[]>();

	/* TODO: Document why this can return null.
	 */
	public AlertRequestDoc getDocument(String radarID, int areaIndex, Shell shell) {
		AlertRequestDoc[] aas = docs.get(radarID);
		AlertRequestDoc doc = null;
		
		if (aas == null) {
			aas = new AlertRequestDoc[2];
			docs.put(radarID, aas);			
		}
		
		if (aas[areaIndex - 1] != null)
			doc = aas[areaIndex - 1];
		
		/* Get the alert request from the RadarServer if we do not already
		 * have it or if we already have but have not modified it.  The later
		 * is a workaround for the lack of a notification from the RadarServer
		 * when alert requests are modified. 
		 */
		if (shell != null && (doc == null || ! doc.isModified())) {
			// TODO: check realm
			GetAlertRequest req = new GetAlertRequest();
			req.radarID = radarID;
			req.areaIndex = areaIndex;
			AlertReqReply r = (AlertReqReply) 
				RadarApps.getRcmSystem().sendCheckedAndHandled(req, shell);
			
			if (r != null) {
				if (doc == null) {
					doc = new AlertRequestDoc();
					aas[areaIndex - 1] = doc;
				}
				if (r.alertRequest != null) {
					doc.set(r.alertRequest);
					doc.setModified(false);
				}
			}
			/* If request failed, but we had an earlier version, just return
			 * that.
			 */
		}
		
		return doc;
	}

	private static AlertRequestEditing instance;
	
	public static AlertRequestEditing getInstance() {
		if (instance == null) {
			synchronized (AlertRequestEditing.class) {
				if (instance == null) {
					instance = new AlertRequestEditing(
							/*SWTObservables.getRealm(Display.getDefault())*/);
				}
			}
		}
		return instance;
	}

	/** Reset the contents of the specified alert request.  Also mark it as
	 * unmodified so that it can be reloaded from the RadarServer. 
	 * @param radarID
	 * @param areaIndex
	 */
	public void clearDocument(String radarID, int areaIndex) {
		AlertRequestDoc[] aas = docs.get(radarID);
		if (aas != null) {
			AlertRequestDoc doc = aas[areaIndex - 1];
			if (doc != null) {
				doc.getBoxBits().clear();
				doc.getThresholds().clear();
				doc.handleBoxBitsChanged();
				doc.setModified(false);
			}
		}
	}
	
	public Map<Sel,AlertRequestDoc> getDocuments() {
		HashMap<Sel, AlertRequestDoc> result = new HashMap<Sel, AlertRequestDoc>();
		
		for (Map.Entry<String, AlertRequestDoc[]> entry : docs.entrySet()) {
			AlertRequestDoc[] aas = entry.getValue(); 
			for (int i = 0; i < aas.length; ++i) {
				if (aas[i] != null)
					result.put(new Sel(entry.getKey(), i + 1), aas[i]);
			}
		}
		
		return result;
	}
}
