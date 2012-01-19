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
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.jobs.AutoSaveJob;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The auto save iterval dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Jan 30, 2008					Eric Babin Initial Creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class AutoSaveIntervalDialog extends CaveJFACEDialog {

    private Composite top = null;

    private Label intervalMinutes;

    private Scale intervalScale;

    private int currentInterval = 1;

    private boolean autoSaveEnabled = false;

    private Button offButton;

    private Button onButton;

    public AutoSaveIntervalDialog(Shell parent, int currentInterval,
            boolean isEnabled) {
        super(parent);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        this.currentInterval = currentInterval;
        this.autoSaveEnabled = isEnabled;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        top.setLayout(new GridLayout(2, false));
        loadConfigData();
        initializeComponents();

        return top;
    }

    private void initializeComponents() {

        Group g = new Group(top, SWT.NONE);
        g.setLayout(new GridLayout(2, true));

        offButton = new Button(g, SWT.RADIO);
        offButton.setText("Off");
        offButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                autoSaveEnabled = false;
            }
        });
        offButton.setSelection(!autoSaveEnabled);

        onButton = new Button(g, SWT.RADIO);
        onButton.setText("On");
        onButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                autoSaveEnabled = true;
            }
        });
        onButton.setSelection(autoSaveEnabled);

        Composite c = new Composite(top, SWT.NONE);
        GridLayout layout = new GridLayout(2, false);
        layout.marginBottom = 10;

        c.setLayout(layout);
        c.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL));

        Label label = new Label(c, SWT.NONE);
        label.setText("Save Interval in Minutes");
        GridData data = new GridData();
        data.horizontalSpan = 2;
        label.setLayoutData(data);

        intervalScale = new Scale(c, SWT.HORIZONTAL);
        intervalScale.setLayoutData(new GridData(120, SWT.DEFAULT));
        intervalScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                autoSaveEnabled = true;
                currentInterval = intervalScale.getSelection();
                String s = Integer.toString(currentInterval);
                intervalMinutes.setText(s);
                intervalMinutes.setToolTipText(s);
                onButton.setSelection(autoSaveEnabled);
                offButton.setSelection(!autoSaveEnabled);
            }
        });
        intervalScale.setMinimum(AutoSaveJob.MIN_INTERVAL);
        intervalScale.setMaximum(AutoSaveJob.MAX_INTERVAL);
        intervalScale.setSelection(this.currentInterval);

        intervalMinutes = new Label(c, SWT.NONE);
        intervalMinutes.setLayoutData(new GridData(20, SWT.DEFAULT));
        intervalMinutes.setText(String.valueOf(intervalScale.getSelection()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Auto Save Interval Dialog");
    }

    /**
     * Method for loading the various config data for the dialog.
     */
    private void loadConfigData() {

    }

    public int getCurrentInterval() {
        return currentInterval;
    }

    public boolean isAutoSaveEnabled() {
        return autoSaveEnabled;
    }
}
