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
package com.raytheon.uf.viz.monitor.fog.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.fog.threshold.FogDisplayWindData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

/**
 * Fog Display Wind Edit Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 21, 2016  5901     randerso  Remove unused style parameter to super
 *                                  constructor
 *
 * </pre>
 *
 * @author ????
 */
public class FogDisplayWindEditDlg extends EditThresholdsDlg
{
    private Spinner windSpdRSpnr;
    private Spinner windSpdYSpnr;
    private Spinner gustSpdRSpnr;
    private Spinner gustSpdYSpnr;
    private Spinner peakWindRSpnr;
    private Spinner peakWindYSpnr;
    private Spinner windDirFromRSpnr;
    private Spinner windDirFromYSpnr;
    private Spinner windDirToRSpnr;
    private Spinner windDirToYSpnr;

    private FogDisplayWindData fdwd;
    private IUpdateDisplayWind updateCB;

    public FogDisplayWindEditDlg(Shell parent, FogDisplayWindData fdwd, IUpdateDisplayWind updateCB)
    {
        super(parent);

        this.fdwd = fdwd;
        this.updateCB = updateCB;
    }

    /**
     * Initialize the components on the display.
     */
    @Override
    protected void initializeComponents()
    {
        shell.setText("FOG: Display Edit Wind");

        rangeUtil = RangesUtil.getInstance();

        createControls();
        createDirControls();
        addSeparator(getDialogShell());
    }

    private void createControls()
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Composite windComp = new Composite(getDialogShell(), SWT.NONE);
        windComp.setLayout(gl);
        windComp.setLayoutData(gd);

        createRYRangeLabels(windComp);

        /*
         * Wind Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windSpdLbl = new Label(windComp, SWT.RIGHT);
        windSpdLbl.setText("Wind Speed (kt):");
        windSpdLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.FOG_Wind);

        windSpdRSpnr = new Spinner(windComp, SWT.BORDER);
        windSpdYSpnr = new Spinner(windComp, SWT.BORDER);

        createSpinnerControls(windComp, windSpdRSpnr, windSpdYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Peak Wind
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label peakWindLbl = new Label(windComp, SWT.RIGHT);
        peakWindLbl.setText("Peak Wind (kt):");
        peakWindLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.FOG_Wind);

        peakWindRSpnr = new Spinner(windComp, SWT.BORDER);
        peakWindYSpnr = new Spinner(windComp, SWT.BORDER);

        createSpinnerControls(windComp, peakWindRSpnr, peakWindYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Gust Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label gustLbl = new Label(windComp, SWT.RIGHT);
        gustLbl.setText("Gust Speed (kt):");
        gustLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.FOG_Wind);

        gustSpdRSpnr = new Spinner(windComp, SWT.BORDER);
        gustSpdYSpnr = new Spinner(windComp, SWT.BORDER);

        createSpinnerControls(windComp, gustSpdRSpnr, gustSpdYSpnr, rd, true, rangeUtil.RValIsHigher);
    }
    private void createDirControls()
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(6, false);
        gl.verticalSpacing = 10;
        Composite windComp = new Composite(getDialogShell(), SWT.NONE);
        windComp.setLayout(gl);
        windComp.setLayoutData(gd);

        createYRRYRangeLabels(windComp);


        /*
         * Wind Direction
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windDirFromLbl = new Label(windComp, SWT.RIGHT);
        windDirFromLbl.setText("Wind Direction:");
        windDirFromLbl.setLayoutData(gd);

        windDirFromYSpnr = new Spinner(windComp, SWT.BORDER);
        windDirFromRSpnr = new Spinner(windComp, SWT.BORDER);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.FOG_WindDirT);

        windDirToRSpnr = new Spinner(windComp, SWT.BORDER);
        windDirToYSpnr = new Spinner(windComp, SWT.BORDER);

        createSpinnerControls(windComp, windDirFromYSpnr,windDirFromRSpnr,windDirToRSpnr, windDirToYSpnr, rd, false, rangeUtil.ryNotNeeded);
        addIncrementListeners(windDirFromRSpnr, windDirFromYSpnr);
        addIncrementListeners(windDirToRSpnr, windDirToYSpnr);
    }

    @Override
    protected void updateControlsWithData()
    {
        /*
         * Wind Speed
         */
        this.windSpdRSpnr.setSelection((int)fdwd.getWindWindSpeedR());
        this.windSpdYSpnr.setSelection((int)fdwd.getWindWindSpeedY());

        /*
         * Peak Wind
         */
        this.peakWindRSpnr.setSelection((int)fdwd.getWindPeakR());
        this.peakWindYSpnr.setSelection((int)fdwd.getWindPeakY());

        /*
         * Gust Speed
         */
        this.gustSpdRSpnr.setSelection((int)fdwd.getWindGustR());
        this.gustSpdYSpnr.setSelection((int)fdwd.getWindGustY());

        /*
         * Wind Direction: From
         */
        this.windDirFromRSpnr.setSelection((int)fdwd.getWindDirFromR());
        this.windDirFromYSpnr.setSelection((int)fdwd.getWindDirFromY());

        /*
         * Wind Direction: To
         */
        this.windDirToRSpnr.setSelection((int)fdwd.getWindDirToR());
        this.windDirToYSpnr.setSelection((int)fdwd.getWindDirToY());
    }

    @Override
    protected void updateData()
    {
        /*
         * Wind Speed
         */
        fdwd.setWindWindSpeedR(this.windSpdRSpnr.getSelection());
        fdwd.setWindWindSpeedY(this.windSpdYSpnr.getSelection());

        /*
         * Peak Wind
         */
        fdwd.setWindPeakR(this.peakWindRSpnr.getSelection());
        fdwd.setWindPeakY(this.peakWindYSpnr.getSelection());

        /*
         * Gust Speed
         */
        fdwd.setWindGustR(this.gustSpdRSpnr.getSelection());
        fdwd.setWindGustY(this.gustSpdYSpnr.getSelection());

        /*
         * Wind Direction: From
         */
        fdwd.setWindDirFromR(this.windDirFromRSpnr.getSelection());
        fdwd.setWindDirFromY(this.windDirFromYSpnr.getSelection());

        /*
         * Wind Direction: To
         */
        fdwd.setWindDirToR(this.windDirToRSpnr.getSelection());
        fdwd.setWindDirToY(this.windDirToYSpnr.getSelection());
    }

    @Override
    protected void applyAction()
    {
        updateData();

        this.updateCB.updateThresholdData(fdwd);
    }

    @Override
    protected boolean determineError(){
        /*
         * make sure the red alert must be contained in the yellow alert arc for wind direction
         */
        int wdRedFrom = this.windDirFromRSpnr.getSelection();
        int wdRedTo = this.windDirToRSpnr.getSelection();
        int wdYellowFrom = this.windDirFromYSpnr.getSelection();
        int wdYellowTo = this.windDirToYSpnr.getSelection();
        return determineError (wdYellowFrom,wdRedFrom,wdRedTo,wdYellowTo);
    }
}
