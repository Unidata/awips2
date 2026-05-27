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
package com.raytheon.viz.xdat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Precipitation Data dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27 Oct 2008             lvenable    Initial creation.
 * 10 Feb 2009             wkwock      Added functions.
 * 31 May 2015  4501       skorolev    Got rid of Vector.
 * 04 Aug 2016  5800       mduff       Cleanup
 * 12 Mar 2018  DCS18260   astrakovsky Moved displayCoopPrecip and displayPrecipAccumulation
 *                                     methods to XdatDlg to avoid repeating code.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class PrecipDataDlg extends CaveSWTDialog {

    /**
     * Hour spinner.
     */
    private Spinner hourSpnr;

    /**
     * Duration spinner.
     */
    private Spinner durationSpnr;

    /**
     * this class is called by owner
     */
    ITextDisplay displayCB;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public PrecipDataDlg(Shell parentShell, ITextDisplay displayCB) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText("Precipitation Data");

        this.displayCB = displayCB;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.verticalSpacing = 10;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        createControls();
        createCloseButton();
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(shell.getBounds().width, shell.getBounds().height);
    }

    /**
     * Create the controls on the dialog.
     */
    private void createControls() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group precipGrp = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 10;
        gl.marginHeight = 10;
        precipGrp.setLayout(gl);
        precipGrp.setLayoutData(gd);
        precipGrp.setText(" Choose Precipitation Option ");

        Button coopPrecBtn = new Button(precipGrp, SWT.PUSH);
        coopPrecBtn.setText(" 24 Hour COOP Precipitation ");
        coopPrecBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayCB.displayCoopPrecip();
            }
        });

        Button pcPrecBtn = new Button(precipGrp, SWT.PUSH);
        pcPrecBtn.setText(" 24 Hour (12Z) PC Precipitation ");
        pcPrecBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayCB.displayPrecipAccumulation(12, 24);
            }
        });

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group endingHrGrp = new Group(shell, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        gl.marginHeight = 10;
        endingHrGrp.setLayout(gl);
        endingHrGrp.setLayoutData(gd);
        endingHrGrp.setText(" Choose Precipitation Option ");

        Label hourLbl = new Label(endingHrGrp, SWT.NONE);
        hourLbl.setText("Choose Hour (GMT): ");

        gd = new GridData(45, SWT.DEFAULT);
        gd.horizontalIndent = 10;
        hourSpnr = new Spinner(endingHrGrp, SWT.BORDER);
        hourSpnr.setDigits(0);
        hourSpnr.setIncrement(1);
        hourSpnr.setPageIncrement(5);
        hourSpnr.setMinimum(0);
        hourSpnr.setMaximum(23);
        hourSpnr.setSelection(12);
        hourSpnr.setLayoutData(gd);

        Label durationLbl = new Label(endingHrGrp, SWT.NONE);
        durationLbl.setText("Choose Duration in Hours: ");

        gd = new GridData(45, SWT.DEFAULT);
        gd.horizontalIndent = 10;
        durationSpnr = new Spinner(endingHrGrp, SWT.BORDER);
        durationSpnr.setDigits(0);
        durationSpnr.setIncrement(1);
        durationSpnr.setPageIncrement(5);
        durationSpnr.setMinimum(0);
        durationSpnr.setMaximum(72);
        durationSpnr.setSelection(12);
        durationSpnr.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, true);
        gd.horizontalSpan = 2;
        Button precipAccumBtn = new Button(endingHrGrp, SWT.PUSH);
        precipAccumBtn.setText("Display Precipitation Accumulation");
        precipAccumBtn.setLayoutData(gd);
        precipAccumBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int hour = hourSpnr.getSelection();
                int duration = durationSpnr.getSelection();
                displayCB.displayPrecipAccumulation(hour, duration);
            }
        });
    }

    /**
     * Create close button at the bottom of the dialog.
     */
    private void createCloseButton() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

}
