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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.activetable.response.GetNextEtnResponse;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.TextDisplayDlg;
import com.raytheon.viz.ui.dialogs.TextDisplayDlg.Location;

/**
 * Dialog displayed to manually confirm ETN assignment when backup partners can
 * not be reached
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2013  #1843     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ETNConfirmationDialog extends CaveJFACEDialog {

    // TODO make this configurable
    private static final int MAX_ETN_DELTA = 10;

    private String phenSig;

    private int proposedEtn;

    private Map<String, Integer> resultsByHost;

    private List<String> errorMessages;

    private Spinner proposedEtnSpinner;

    /**
     * Dialog constructor
     * 
     * @param parentShell
     * @param etnResponse
     */
    public ETNConfirmationDialog(Shell parentShell,
            GetNextEtnResponse etnResponse) {
        super(parentShell);

        this.phenSig = etnResponse.getPhensig();
        this.proposedEtn = etnResponse.getNextEtn();
        this.resultsByHost = etnResponse.getResultsByHost();
        this.errorMessages = etnResponse.getErrorMessages();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        newShell.setText("Confirm ETN for " + this.phenSig);
        super.configureShell(newShell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) top.getLayout();
        layout.numColumns = 2;

        Label label = new Label(top, SWT.LEFT);
        GridData layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
        label.setLayoutData(layoutData);
        label.setText("Proposed ETN for " + this.phenSig + ":");

        proposedEtnSpinner = new Spinner(top, SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.CENTER, false, false);
        proposedEtnSpinner.setLayoutData(layoutData);
        proposedEtnSpinner.setValues(this.proposedEtn, this.proposedEtn,
                this.proposedEtn + MAX_ETN_DELTA, 0, 1, 1);

        Group group = new Group(top, SWT.NONE);
        layout = new GridLayout(2, false);
        group.setLayout(layout);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.horizontalSpan = 2;
        group.setLayoutData(layoutData);
        group.setText("Partner ETNs");

        for (Entry<String, Integer> entry : this.resultsByHost.entrySet()) {
            Label host = new Label(group, SWT.LEFT);
            layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            host.setLayoutData(layoutData);
            host.setText(entry.getKey());

            Label etn = new Label(group, SWT.RIGHT);
            layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            etn.setLayoutData(layoutData);
            String text = entry.getValue() == null ? "ERR" : entry.getValue()
                    .toString();
            etn.setText(text);
        }

        final Button showErrors = new Button(top, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.horizontalSpan = 2;
        showErrors.setLayoutData(layoutData);
        showErrors.setText("Show Errors");
        showErrors.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                showErrors.setEnabled(false);
                StringBuilder errorText = new StringBuilder();
                for (String s : errorMessages) {
                    errorText.append(s).append('\n');
                }
                TextDisplayDlg errorDlg = new TextDisplayDlg(
                        ETNConfirmationDialog.this.getShell(), "Errors",
                        errorText.toString(), Location.BELOW);

                errorDlg.open();
                showErrors.setEnabled(true);
            }

        });

        return top;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButtonsForButtonBar(parent);
        getButton(IDialogConstants.OK_ID).setText("Transmit");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @Override
    protected void okPressed() {
        this.proposedEtn = this.proposedEtnSpinner.getSelection();
        super.okPressed();
    }

    /**
     * @return the proposedEtn
     */
    public int getProposedEtn() {
        return proposedEtn;
    }
}
