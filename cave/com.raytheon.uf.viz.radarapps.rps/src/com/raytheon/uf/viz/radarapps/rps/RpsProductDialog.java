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
package com.raytheon.uf.viz.radarapps.rps;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.products.Usage;
import com.raytheon.uf.viz.radarapps.products.ui.RadarProductUI;


public class RpsProductDialog extends Dialog {

	private RadarProductUI pui = new RadarProductUI() {
		// TODO: this should be a listener interface
		protected void onRequestChanged() {
			super.onRequestChanged();
			validate();
		}
	};
	
	public RpsProductDialog(Shell parentShell) {
		super(parentShell);
		pui.setPrioritySelectionEnabled(true);
		pui.setUsage(Usage.RPS);
	}
	
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite p = (Composite) super.createDialogArea(parent);
		((GridLayout) p.getLayout()).numColumns = 1;

		Control c = pui.createProductRequestUI(p);
		c.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		
		return p;
	}

	public RadarProductUI getProductUI() {
		return pui;
	}

	@Override
	protected int getShellStyle() {
		return (super.getShellStyle() & ~(SWT.DIALOG_TRIM))|SWT.RESIZE|SWT.SHELL_TRIM;
	}

	@Override
	protected boolean isResizable() {
		return true;
	}
	
	protected void validate() {
		Button b = getButton(IDialogConstants.OK_ID);
		if (b != null)
			b.setEnabled(pui.isRequestValid());
	}

	@Override
	protected void okPressed() {
		if (pui.isRequestValid())
			super.okPressed();
	}

	@Override
	public void create() {
		super.create();
		validate();
	}
}
