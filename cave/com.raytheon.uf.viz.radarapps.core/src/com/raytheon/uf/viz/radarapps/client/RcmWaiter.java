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
package com.raytheon.uf.viz.radarapps.client;

import java.io.IOException;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.mqsrvr.ReplyObj;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.uf.viz.radarapps.core.RadarApps;


public class RcmWaiter implements RcmClientHandler {
	ReqObj req;
	ReplyObj reply;
	Shell shell;
	MessageDialog dialog;
	boolean canceled;
	
	public RcmWaiter(ReqObj req, Shell shell) {
		this.req = req;
		this.shell = shell;
	}
	public ReplyObj send() throws IOException {
		return send(RadarApps.getRcmSystem().getClient());
	}
	public ReplyObj send(RcmClient client) throws IOException {
		synchronized (this) {
			client.sendRequest(req, this);
			try {
				wait(getSilentTimeout());
			} catch (InterruptedException e) {
				return reply;
			}
			if (reply == null && ! canceled) {
				String[] labels = { "Cancel" };
				dialog = new MessageDialog(shell, "Radar Application"/*TODO*/, null, 
						"Waiting for response from RadarServer...", 
						MessageDialog.NONE, labels, 0);
			}
		}
		if (dialog != null) {
			dialog.open();
		}
		return reply;
	}
	public long getSilentTimeout() { return 500; }
	
	@Override
	public void onReply(ReplyObj obj) {
		synchronized (this) {
			reply = obj;
			cancelWait();
			notifyAll();
		}
	}
	private void cancelWait() {
		synchronized (this) {
			canceled = true;
			Display display = shell != null ? shell.getDisplay() : Display.getCurrent();
			display.asyncExec(new Runnable() {
				public void run() {
					if (dialog != null) 
						dialog.close();
				}
			});
		}
	}
}
