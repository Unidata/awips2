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
package com.raytheon.uf.viz.monitor.snow.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayWindData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

/**
 * Snow Display Wind Edit Dialog
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
public class SnowDisplayWindEditDlg extends EditThresholdsDlg
{

    private Spinner windSpdRSpnr;
    private Spinner windSpdYSpnr;
    private Spinner peakWindRSpnr;
    private Spinner peakWindYSpnr;
    private Spinner gustSpdRSpnr;
    private Spinner gustSpdYSpnr;
    private Spinner windDirFromRSpnr;
    private Spinner windDirFromYSpnr;
    private Spinner windDirToRSpnr;
    private Spinner windDirToYSpnr;

    private SnowDisplayWindData sdwd;
    private IUpdateDisplayWind updateCB;

    public SnowDisplayWindEditDlg(Shell parent, SnowDisplayWindData sdwd,
            IUpdateDisplayWind updateCB)
    {
        super(parent);

        this.sdwd = sdwd;
        this.updateCB = updateCB;
    }

    /**
     * Initialize the components on the display.
     */
    @Override
    protected void initializeComponents()
    {
        shell.setText("SNOW: Display Edit Wind");

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

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Wind);

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

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Wind);

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

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Wind);

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

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_WindDirT);

        windDirToRSpnr = new Spinner(windComp, SWT.BORDER);
        windDirToYSpnr = new Spinner(windComp, SWT.BORDER);

        createSpinnerControls(windComp,windDirFromYSpnr,windDirFromRSpnr, windDirToRSpnr, windDirToYSpnr, rd, false, rangeUtil.ryNotNeeded);
        addIncrementListeners(windDirFromRSpnr, windDirFromYSpnr);
        addIncrementListeners(windDirToRSpnr, windDirToYSpnr);
    }

    @Override
    protected void updateControlsWithData()
    {
        windSpdRSpnr.setSelection((int)sdwd.getWindWindSpeedR());
        windSpdYSpnr.setSelection((int)sdwd.getWindWindSpeedY());

        peakWindRSpnr.setSelection((int)sdwd.getWindPeakR());
        peakWindYSpnr.setSelection((int)sdwd.getWindPeakY());

        gustSpdRSpnr.setSelection((int)sdwd.getWindGustR());
        gustSpdYSpnr.setSelection((int)sdwd.getWindGustY());

        windDirFromRSpnr.setSelection((int)sdwd.getWindDirFromR());
        windDirFromYSpnr.setSelection((int)sdwd.getWindDirFromY());

        windDirToRSpnr.setSelection((int)sdwd.getWindDirToR());
        windDirToYSpnr.setSelection((int)sdwd.getWindDirToY());
    }

    @Override
    protected void updateData()
    {
        sdwd.setWindWindSpeedR(windSpdRSpnr.getSelection());
        sdwd.setWindWindSpeedY(windSpdYSpnr.getSelection());

        sdwd.setWindPeakR(peakWindRSpnr.getSelection());
        sdwd.setWindPeakY(peakWindYSpnr.getSelection());

        sdwd.setWindGustR(gustSpdRSpnr.getSelection());
        sdwd.setWindGustY(gustSpdYSpnr.getSelection());

        sdwd.setWindDirFromR(windDirFromRSpnr.getSelection());
        sdwd.setWindDirFromY(windDirFromYSpnr.getSelection());

        sdwd.setWindDirToR(windDirToRSpnr.getSelection());
        sdwd.setWindDirToY(windDirToYSpnr.getSelection());
    }

    @Override
    protected void applyAction()
    {
        updateData();
        this.updateCB.updateThresholdData(sdwd);
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
