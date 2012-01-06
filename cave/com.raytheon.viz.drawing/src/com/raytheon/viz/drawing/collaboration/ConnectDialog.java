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

package com.raytheon.viz.drawing.collaboration;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.jivesoftware.smack.XMPPException;

import com.raytheon.viz.drawing.DrawingLayer;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The connection dialog for collaboration
 * 
 * <pre>
 *  
 *   SOFTWARE HISTORY
 *  
 *   Date         Ticket#     Engineer    Description
 *   ------------ ----------  ----------- --------------------------
 *   Nov 21, 2006 66          chammack    Initial Creation.
 *   
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ConnectDialog extends CaveJFACEDialog {

	/** The login button */
	private Button loginButton;

	/** The register button */
	private Button registerButton;

	/** The hostname field */
	private Text hostName;

	/** The username field */
	private Text userName;

	/** The password field */
	private Text password;

	/** The drawing layer */
	private DrawingLayer layer;

	public ConnectDialog(Shell parentShell, DrawingLayer layer) {
		super(parentShell);
		this.layer = layer;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {

		Composite body = new Composite(parent, SWT.NONE);

		body.setLayout(new GridLayout(2, true));
		body.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));

		Label hostLabel = new Label(body, SWT.BOLD);
		hostLabel.setText("Hostname: ");

		hostName = new Text(body, SWT.BORDER);
		hostName.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		hostName.setTextLimit(64);

		Label userLabel = new Label(body, SWT.BOLD);
		userLabel.setText("Username: ");

		userName = new Text(body, SWT.BORDER);
		userName.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		userName.setTextLimit(32);

		Label passwordLabel = new Label(body, SWT.BOLD);
		passwordLabel.setText("Password: ");

		password = new Text(body, SWT.PASSWORD | SWT.BORDER);
		password.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		password.setTextLimit(32);

		applyDialogFont(parent);
		return parent;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	protected void createButtonsForButtonBar(Composite parent) {
		registerButton = createButton(parent, IDialogConstants.OPEN_ID,
				"Register", true);
		registerButton.setVisible(true);

		loginButton = createButton(parent, IDialogConstants.OK_ID,
				IDialogConstants.OK_LABEL, true);

		loginButton.setVisible(true);
		loginButton.setFocus();

		createButton(parent, IDialogConstants.CANCEL_ID,
				IDialogConstants.CANCEL_LABEL, false);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	protected void buttonPressed(int buttonId) {

		if (buttonId == IDialogConstants.OK_ID) {
			try {
				CollaborationManager.connect(layer, hostName.getText(),
						userName.getText(), password.getText());
			} catch (XMPPException e) {
				MessageBox mb = new MessageBox(this.getShell());
				mb.setText("Error!");
				mb.setMessage(e.getMessage());
				mb.open();
				return;
			}
		} else if (buttonId == IDialogConstants.OPEN_ID) {
			try {
				CollaborationManager.createAccount(hostName.getText(), userName
						.getText(), password.getText());

			} catch (XMPPException e) {
				MessageBox mb = new MessageBox(this.getShell());
				mb.setText("Error!");
				mb.setMessage(e.getMessage());
				mb.open();
				return;
			}

			try {
				CollaborationManager.connect(layer, hostName.getText(),
						userName.getText(), password.getText());
			} catch (XMPPException e) {
				MessageBox mb = new MessageBox(this.getShell());
				mb.setText("Error!");
				mb.setMessage(e.getMessage());
				mb.open();
				return;
			}
			this.close();
		}
		super.buttonPressed(buttonId);
	}

}
