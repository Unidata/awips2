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
package com.raytheon.uf.viz.radarapps.otr;

import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.NotificationEvent;
import com.raytheon.rcm.event.OtrEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.RequestResponse;
import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.radarapps.core.RadarApps;
import com.raytheon.uf.viz.radarapps.requests.Activator;

public class OtrEventListener implements RadarEventListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(OtrEventListener.class);
	private static OtrEventListener instance;
	
	public static void activate() {
		if (instance == null) {
			synchronized (OtrEventListener.class) {
				if (instance == null) {
					instance = new OtrEventListener();
					/* TODO: Subscribe/unsubscribe as needed.  True subscription not 
					 * actually needed now because OTR events are sent back to the client 
					 * on the request connection.  Still need to register as a listener,
					 * however.  
					 */
					RadarApps.getRcmSystem().getClient().addEventListener(instance);
				}
			}
		}
	}

	@Override
	public void handleConfigEvent(ConfigEvent event) {
		// nothing
	}

	@Override
	public void handleNotificationEvent(NotificationEvent event) {
		if (event instanceof OtrEvent) {
			OtrEvent otrEvent = (OtrEvent) event;
			String mne = "(unknown product)";
			String otrError = "unknown error";
			String description = "";
			RadarProduct rp = null;
			int messageCode = -1;
			
			try {
				int code = otrEvent.request.productCode;
				mne = ProductInfo.getInstance().getMnemonicForCode(code);
				rp = ProductInfo.getInstance().getPoductForCode(code); // TODO: radar type variants

				messageCode = Message.messageCodeOf(otrEvent.product);
				StringBuilder sb = new StringBuilder();
				
				if (messageCode != Message.REQUEST_RESPONSE) {
					PDB pdb = GraphicProduct.pdbOfMessage(otrEvent.product);
					sb.append(String.format("%1$tH:%1$tM", pdb.volumeScanTime));
					if (rp.resolution != null)
						sb.append(String.format("  res %.2f", rp.resolution));
					if (rp.params.contains(Param.ELEVATION))
						sb.append(String.format("  elev %.2f", pdb.getElevationAngle() / 10.0));
				} else {
					RequestResponse prr = RequestResponse.decode(otrEvent.product); 
					otrError = prr.getErrorMessages();
					sb.append(String.format("%1$tH:%1$tM", prr.volumeScanTime));
					if (rp.resolution != null)
						sb.append(String.format("  res %.2f", rp.resolution));
					if (rp.params.contains(Param.ELEVATION))
						sb.append(String.format("  elev %.2f", prr.elevationAngle / 10.0));
				}
				description = sb.toString();
			} catch (Exception e) {
				statusHandler.handle(Priority.PROBLEM, 
	                    "Could not process radar message.", e);
			}

			if (messageCode != Message.REQUEST_RESPONSE) {
				statusHandler.handle(Priority.EVENTA,
						String.format("%s: %s OTR %s", otrEvent.radarID, mne,
								description));
			} else
				statusHandler.handle(Priority.PROBLEM,
						String.format("%s: %s %s OTR %s", otrEvent.radarID, mne,
								otrError, description));
		}
	}

	@Override
	public void handleRadarEvent(RadarEvent event) {
		// nothing
	}
}
