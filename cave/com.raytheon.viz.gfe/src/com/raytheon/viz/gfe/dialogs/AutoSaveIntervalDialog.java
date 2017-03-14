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
package com.raytheon.viz.gfe.dialogs;

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

import com.raytheon.viz.gfe.jobs.AutoSaveJob;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.SpinScale;

/**
 * The auto save interval dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2008            Eric Babin  Initial Creation
 * Aug 27, 2013     #2302  randerso    Code cleanup
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class AutoSaveIntervalDialog extends CaveJFACEDialog {

    private AutoSaveJob autoSaveJob;

    private Button offButton;

    private Button onButton;

    private Label intervalLabel;

    private SpinScale intervalScale;

    /**
     * Constructor
     * 
     * @param parent
     * @param currentInterval
     * @param isEnabled
     */
    public AutoSaveIntervalDialog(Shell parent, AutoSaveJob autoSaveJob) {
        super(parent);
        this.autoSaveJob = autoSaveJob;
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
        super.configureShell(newShell);
        newShell.setText("Auto Save Interval Dialog");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) comp.getLayout();
        layout.marginHeight = 0;
        layout.marginWidth = 0;

        Group group = new Group(comp, SWT.NONE);
        layout = new GridLayout(2, false);
        group.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        group.setLayoutData(layoutData);
        group.setText("Auto Save");

        int interval = autoSaveJob.getInterval();

        offButton = new Button(group, SWT.RADIO);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        offButton.setLayoutData(layoutData);
        offButton.setText("Off");
        offButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (offButton.getSelection()) {
                    intervalLabel.setEnabled(false);
                    intervalScale.setEnabled(false);
                }
            }
        });
        offButton.setSelection(interval == 0);

        onButton = new Button(group, SWT.RADIO);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        onButton.setLayoutData(layoutData);
        onButton.setText("On");
        onButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (onButton.getSelection()) {
                    intervalLabel.setEnabled(true);
                    intervalScale.setEnabled(true);
                }
            }
        });
        onButton.setSelection(interval > 0);

        intervalLabel = new Label(group, SWT.CENTER);
        intervalLabel.setText("Save Interval in Minutes");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false, 2, 1);
        intervalLabel.setLayoutData(layoutData);

        intervalScale = new SpinScale(group, SWT.HORIZONTAL);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false, 2, 1);
        layoutData.minimumWidth = 240;
        intervalScale.setLayoutData(layoutData);
        intervalScale.setMinimum(AutoSaveJob.MIN_INTERVAL);
        intervalScale.setMaximum(AutoSaveJob.MAX_INTERVAL);
        if (interval > 0) {
            intervalScale.setSelection(interval);
        } else {
            intervalLabel.setEnabled(false);
            intervalScale.setEnabled(false);
            intervalScale.setSelection(AutoSaveJob.MAX_INTERVAL);
        }

        return comp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @Override
    protected void okPressed() {
        if (offButton.getSelection()) {
            autoSaveJob.setInterval(0);
        } else {
            autoSaveJob.setInterval(intervalScale.getSelection());
        }
        super.okPressed();
    }

}
