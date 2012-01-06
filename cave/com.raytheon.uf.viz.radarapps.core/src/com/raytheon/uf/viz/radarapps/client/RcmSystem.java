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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.mqsrvr.ReplyObj;
import com.raytheon.rcm.mqsrvr.ReqObj;

public class RcmSystem {
    RcmClient client;

    Object getz = new Object();

    public RcmSystem() {
        client = new RcmClient();
        client.start();
    }

    public ReplyObj sendCheckedAndHandled(ReqObj req, Shell shell) {
        RcmWaiter waiter = new RcmWaiter(req, shell);
        String error = null;
        ReplyObj reply = null;
        try {
            reply = waiter.send(getClient());
        } catch (IOException e) {
            error = e.getMessage();
        }
        if (reply != null && reply.error != null)
            error = reply.error;
        if (error != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Error");
            mb.setMessage(error);
            mb.open();
            return null;
        } else
            return reply;
    }

    public RcmClient getClient() {
        return client;
    }
}
