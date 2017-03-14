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

import java.util.Arrays;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.mqsrvr.ReqObj.SendOneTimeRequests;
import com.raytheon.rcm.products.Usage;
import com.raytheon.uf.viz.radarapps.core.RadarApps;
import com.raytheon.uf.viz.radarapps.products.ui.ExtProductsUI;
import com.raytheon.uf.viz.radarapps.products.ui.RadarProductUI;

public class OtrWindow extends Dialog {
	
	private static final int ID_SEND = IDialogConstants.CLIENT_ID  + 1;

	private RadarProductUI pui = new RadarProductUI() {
		// TODO: this should be a listener interface
		protected void onRequestChanged() {
			super.onRequestChanged();
			validate();
		}
	};	
	
	public OtrWindow(Shell shell) {
        super(shell);
        setBlockOnOpen(false);
        setShellStyle(getShellStyle() & ~SWT.APPLICATION_MODAL);
    }

	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("One Time Request");
	}

	@Override
	protected Control createDialogArea(Composite parent) {
        Composite c = (Composite) super.createDialogArea(parent);
        GridLayout gl = (GridLayout) c.getLayout();
        gl.numColumns = 1;

        pui.setPrioritySelectionEnabled(true);
        pui.setVolumeScanSelectionEnabled(true);
        pui.setRepeatCountEnabled(true);
        pui.setRpgSelectorEnabled(true);
        pui.setUsage(Usage.OTR);
        Control rc = pui.createProductRequestUI(c);
        rc.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        pui.setDefaultRequest();
        
        getShell().getDisplay().asyncExec(new Runnable() {
        	public void run() {
        		ExtProductsUI rpgSelector = pui.getRpgSelector();
        		if (rpgSelector != null)
        			rpgSelector.selectDefaultRpg();
        	}
        });
        
        return c;
	}
	
	private void validate() {
		Button b = getButton(ID_SEND);
		if (b != null)
			b.setEnabled(pui.isRequestValid());
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, ID_SEND,
                "Send", true);
		createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, false);
	}

	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == ID_SEND) {
			if (pui.isRequestValid()) {
				String radarID = pui.getSelectedRpg();
				if (radarID != null) {
					OtrEventListener.activate();
					
					SendOneTimeRequests r = new SendOneTimeRequests();
					r.radarIDs = Arrays.asList(radarID); 
					r.requests = Arrays.asList(pui.getRequest());
					RadarApps.getRcmSystem().sendCheckedAndHandled(r, getShell());
				} else {
					MessageBox box = new MessageBox(getShell(), SWT.OK | SWT.ICON_ERROR);
					box.setText("One Time Request");
					box.setMessage("Please select a RPG.");
					box.open();
				}
			}
		} else if (buttonId == IDialogConstants.CLOSE_ID) {
			setReturnCode(Window.OK);
			close();
		}
	}
}
