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
package com.raytheon.uf.viz.datadelivery.bandwidth.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialogBase;
import com.raytheon.viz.ui.widgets.TwoValueSliderCanvas;

/**
 * Dialog to configure the values for the bandwidth used graph.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2013    2430    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class BandwidthUsedConfigDlg extends CaveSWTDialogBase {

    private TwoValueSliderCanvas slider;

    private final int lowerVal;

    private final int upperVal;

    private final RGB lowerColor;

    private final RGB midColor;

    private final RGB upperColor;

    /**
     * Constructor.
     * 
     * @param shell
     *            The parent shell
     * @param lowerVal
     *            The lower threshold value
     * @param upperVal
     *            The upper threshold value
     * @param lowerColor
     *            The lower threshold color
     * @param midColor
     *            The middle threshold color
     * @param upperColor
     *            The upper threshold color
     */
    public BandwidthUsedConfigDlg(Shell shell, int lowerVal, int upperVal,
            RGB lowerColor, RGB midColor, RGB upperColor) {
        super(shell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        this.setText("Utilization Threshold");
        this.lowerVal = lowerVal;
        this.upperVal = upperVal;
        this.lowerColor = lowerColor;
        this.midColor = midColor;
        this.upperColor = upperColor;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        Group thresholdGroup = new Group(shell, SWT.NONE);
        thresholdGroup.setLayout(new GridLayout(1, false));
        thresholdGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));
        thresholdGroup.setText(" Bandwidth Threshold Configuration ");

        slider = new TwoValueSliderCanvas(thresholdGroup, 0, 100, 1, lowerVal,
                upperVal, lowerColor, midColor, upperColor);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, true);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayoutData(gd);
        buttonComp.setLayout(gl);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int[] returnVal = new int[2];
                returnVal[0] = slider.getLowerValue();
                returnVal[1] = slider.getUpperValue();
                setReturnValue(returnVal);
                close();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(false);
                close();
            }
        });
    }

    /**
     * Get the lower value.
     * 
     * @return The lower value
     */
    public int getLowerValue() {
        return slider.getLowerValue();
    }

    /**
     * Get the upper value.
     * 
     * @return The upper value
     */
    public int getUpperValue() {
        return slider.getUpperValue();
    }
}
