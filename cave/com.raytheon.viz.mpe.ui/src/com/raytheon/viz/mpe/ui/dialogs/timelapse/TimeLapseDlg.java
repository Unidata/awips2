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
package com.raytheon.viz.mpe.ui.dialogs.timelapse;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TimeLapseDlg extends CaveSWTDialog {

    /** The font */
    private Font font = null;

    /**
     * The precip value spinner control.
     */
    private Spinner hourSpinner = null;

    /**
     * The precip slider control.
     */
    private Scale hourSlider = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     */
    public TimeLapseDlg(Shell parentShell) {
        super(parentShell);
        setText("Time Lapse Interval");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);
        // Initialize all of the controls and layoutsendCal
        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));

        createSlider(comp);
        createLabel(comp);
        createButtons(comp);
    }

    /**
     * Create the slider/spinner controls.
     * 
     * @param comp
     *            The main composite
     */
    private void createSlider(Composite comp) {
        GridData gd = new GridData(250, 30);
        hourSlider = new Scale(comp, SWT.HORIZONTAL);
        hourSlider.setMinimum(1);
        hourSlider.setMaximum(24);
        hourSlider.setIncrement(1);
        hourSlider.setLayoutData(gd);
        hourSlider.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                hourSpinner.setSelection(hourSlider.getSelection());
            }
        });

        // Create the spinner.
        hourSpinner = new Spinner(comp, SWT.BORDER);
        gd = new GridData(30, SWT.DEFAULT);
        hourSpinner.setLayoutData(gd);
        hourSpinner.setMinimum(1);
        hourSpinner.setMaximum(24);
        hourSpinner.setSelection(hourSlider.getSelection());
        hourSpinner.setDigits(0);

        hourSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                hourSlider.setSelection(hourSpinner.getSelection());
            }
        });

    }

    /**
     * Create the label and separator.
     * 
     * @param comp
     *            The main composite
     */
    private void createLabel(Composite comp) {
        Label hoursLbl = new Label(comp, SWT.NONE);
        hoursLbl.setText("Hours before current time.");

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false, 2, 1);
        gd.widthHint = 315;
        Label sepLbl = new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the ok and cancel buttons.
     * 
     * @param comp
     *            The main composite
     */
    private void createButtons(Composite comp) {
        Composite buttonComp = new Composite(comp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false, 2, 1);
        buttonComp.setLayoutData(gd);

        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        gd = new GridData(75, SWT.DEFAULT);
        okBtn.setAlignment(SWT.CENTER);
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    int hours = Integer.parseInt(hourSpinner.getText());
                    MPEDisplayManager.startLooping(container, hours);
                    close();
                } else {
                    MessageDialog.openError(getShell(),
                            "Could not start time lapse",
                            "No editor active to start time lapse on");
                }
            }
        });

        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        gd = new GridData(75, SWT.DEFAULT);
        cancelBtn.setAlignment(SWT.CENTER);
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }
}
