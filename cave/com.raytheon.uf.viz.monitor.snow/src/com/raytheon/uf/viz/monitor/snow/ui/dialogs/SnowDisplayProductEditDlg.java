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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayProductData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

public class SnowDisplayProductEditDlg extends EditThresholdsDlg
{
    
    private Text visRTF;
    private Button visIncR;
    private Button visDecR;
    private Text visYTF;
    private Button visIncY;
    private Button visDecY;
    
    private Spinner windSpdRSpnr;
    private Spinner windSpdYSpnr;
    private Spinner gustSpdRSpnr;
    private Spinner gustSpdYSpnr;
    private Spinner peakWindRSpnr;
    private Spinner peakWindYSpnr;
    
    private Spinner tempRSpnr;
    private Spinner tempYSpnr;
    private Spinner hrPrecipRSpnr;
    private Spinner hrPrecipYSpnr;
    
    private Spinner snincrHrRSpnr;
    private Spinner snincrHrYSpnr;
    private Spinner snincrTotRSpnr;
    private Spinner snincrTotYSpnr;
    private Spinner snowDepthRSpnr;
    private Spinner snowDepthYSpnr;
    
    private SnowDisplayProductData sdpd;
    private IUpdateDisplayProduct updateCB;
    
    public SnowDisplayProductEditDlg(Shell parent, SnowDisplayProductData sdpd,
            IUpdateDisplayProduct updateCB)
    {    
        super(parent, 0);
        
        this.sdpd = sdpd;
        this.updateCB = updateCB;
    }
    
    /**
     * Initialize the components on the display.
     */
    protected void initializeComponents()
    {
        shell.setText("SNOW: Display Edit Product");
        
        rangeUtil = RangesUtil.getInstance();
        
        createBlizzardGroup();
        createFreezingPrecipGroup();
        createHeavySnowGroup();
    }
    
    private void createBlizzardGroup()
    {
        RangeData rd;
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Group blizzardGrp = new Group(getDialogShell(), SWT.NONE);
        blizzardGrp.setLayout(gl);
        blizzardGrp.setLayoutData(gd);
        blizzardGrp.setText(" Blizzard Warning ");
        
        createRYRangeLabels(blizzardGrp);
        
        /*
         * Visibility
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label visLbl = new Label(blizzardGrp, SWT.RIGHT);
        visLbl.setText("Vis (mi):");
        visLbl.setLayoutData(gd);
        
        Text[] textArray = createVisControls(blizzardGrp, visRTF, visIncR, visDecR, visYTF, visIncY, visDecY);
        visRTF = textArray[0];
        visYTF = textArray[1];
        
        /*
         * Wind Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windSpdLbl = new Label(blizzardGrp, SWT.RIGHT);
        windSpdLbl.setText("Wind Speed (kt):");
        windSpdLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Wind);
        
        windSpdRSpnr = new Spinner(blizzardGrp, SWT.BORDER);
        windSpdYSpnr = new Spinner(blizzardGrp, SWT.BORDER);
        
        createSpinnerControls(blizzardGrp, windSpdRSpnr, windSpdYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         * Gust Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label gustLbl = new Label(blizzardGrp, SWT.RIGHT);
        gustLbl.setText("Gust Speed (kt):");
        gustLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Wind);
        
        gustSpdRSpnr = new Spinner(blizzardGrp, SWT.BORDER);
        gustSpdYSpnr = new Spinner(blizzardGrp, SWT.BORDER);
        
        createSpinnerControls(blizzardGrp, gustSpdRSpnr, gustSpdYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         * Peak Wind
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label peakWindLbl = new Label(blizzardGrp, SWT.RIGHT);
        peakWindLbl.setText("Peak Wind (kt):");
        peakWindLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Wind);
        
        peakWindRSpnr = new Spinner(blizzardGrp, SWT.BORDER);
        peakWindYSpnr = new Spinner(blizzardGrp, SWT.BORDER);
        
        createSpinnerControls(blizzardGrp, peakWindRSpnr, peakWindYSpnr, rd, true, rangeUtil.RValIsHigher);
    }
    
    private void createFreezingPrecipGroup()
    {
        RangeData rd;
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Group freezePrecipGrp = new Group(getDialogShell(), SWT.NONE);
        freezePrecipGrp.setLayout(gl);
        freezePrecipGrp.setLayoutData(gd);
        freezePrecipGrp.setText(" Freezing Precip ");
        
        createRYRangeLabels(freezePrecipGrp);
        
        /*
         * Temperature (F)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label tempLbl = new Label(freezePrecipGrp, SWT.RIGHT);
        tempLbl.setText("Temp (F):");
        tempLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Temp);
        
        tempRSpnr = new Spinner(freezePrecipGrp, SWT.BORDER);
        tempYSpnr = new Spinner(freezePrecipGrp, SWT.BORDER);
        
        createSpinnerControls(freezePrecipGrp, tempRSpnr, tempYSpnr, rd, true, rangeUtil.YValIsHigher);
        
        /*
         * Hourly Precip (in)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label hrPrecipLbl = new Label(freezePrecipGrp, SWT.RIGHT);
        hrPrecipLbl.setText("Hourly Precip (in):");
        hrPrecipLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Precip);
        
        hrPrecipRSpnr = new Spinner(freezePrecipGrp, SWT.BORDER);
        hrPrecipYSpnr = new Spinner(freezePrecipGrp, SWT.BORDER);
        
        createSpinnerControls(freezePrecipGrp, hrPrecipRSpnr, hrPrecipYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        // Since the Hour Precip is in decimals we need to override the control
        // setting from the call to createSpinnerControls.
        setupDecimalSpinners(hrPrecipRSpnr, hrPrecipYSpnr, rd);
    }
    
    private void createHeavySnowGroup()
    {
        RangeData rd;
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Group heavySnowGrp = new Group(getDialogShell(), SWT.NONE);
        heavySnowGrp.setLayout(gl);
        heavySnowGrp.setLayoutData(gd);
        heavySnowGrp.setText(" Heavy Snow Warning ");
        
        createRYRangeLabels(heavySnowGrp);
        
        /*
         * Snow Increasing Rapidly - Hour(in)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label snIncRapHrLbl = new Label(heavySnowGrp, SWT.RIGHT);
        snIncRapHrLbl.setText("SNINCR Hourly (in):");
        snIncRapHrLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_SnIncRHr);
        
        snincrHrRSpnr = new Spinner(heavySnowGrp, SWT.BORDER);
        snincrHrYSpnr = new Spinner(heavySnowGrp, SWT.BORDER);
        
        createSpinnerControls(heavySnowGrp, snincrHrRSpnr, snincrHrYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         * Snow Increasing Rapidly - Total (in)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label snIncRapTotLbl = new Label(heavySnowGrp, SWT.RIGHT);
        snIncRapTotLbl.setText("SNINCR Total (in):");
        snIncRapTotLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_SnIncRTot);
        
        snincrTotRSpnr = new Spinner(heavySnowGrp, SWT.BORDER);
        snincrTotYSpnr = new Spinner(heavySnowGrp, SWT.BORDER);
        
        createSpinnerControls(heavySnowGrp, snincrTotRSpnr, snincrTotYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         * Snow Depth (in)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label snowDepthLbl = new Label(heavySnowGrp, SWT.RIGHT);
        snowDepthLbl.setText("Snow Depth (in):");
        snowDepthLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Depth);
        
        snowDepthRSpnr = new Spinner(heavySnowGrp, SWT.BORDER);
        snowDepthYSpnr = new Spinner(heavySnowGrp, SWT.BORDER);
        
        createSpinnerControls(heavySnowGrp, snowDepthRSpnr, snowDepthYSpnr, rd, true, rangeUtil.RValIsHigher);
    }   

    @Override
    protected void updateControlsWithData()
    {
        double val = Double.NaN;
        
        /*
         * Blizzard Warning
         */
        val = sdpd.getBlizWrnVisR();
        this.visRIndex = rangeUtil.getIndexOfValue((int)val);
        this.visRTF.setText(rangeUtil.getVisString((int)val));
        
        val = sdpd.getBlizWrnVisY();
        this.visYIndex = rangeUtil.getIndexOfValue((int)val);
        this.visYTF.setText(rangeUtil.getVisString((int)val));
        
        windSpdRSpnr.setSelection((int) sdpd.getBlizWrnWindSpdR());
        windSpdYSpnr.setSelection((int) sdpd.getBlizWrnWindSpdY());
        
        gustSpdRSpnr.setSelection((int) sdpd.getBlizWrnGustSpdR());
        gustSpdYSpnr.setSelection((int) sdpd.getBlizWrnGustSpdY());
        
        peakWindRSpnr.setSelection((int) sdpd.getBlizWrnPeakWindR());
        peakWindYSpnr.setSelection((int) sdpd.getBlizWrnPeakWindY());
        
        /*
         * Freezing Precip
         */
        tempRSpnr.setSelection((int) sdpd.getFrzPrecipTempR());
        tempYSpnr.setSelection((int) sdpd.getFrzPrecipTempY());
        
        hrPrecipRSpnr.setSelection((int) (sdpd.getFrzPrecipHrlyPrcpR() * 10));
        hrPrecipYSpnr.setSelection((int) (sdpd.getFrzPrecipHrlyPrcpY() * 10));
        
        /*
         * Heavy Snow Warning
         */
        snincrHrRSpnr.setSelection((int) sdpd.getHvySnowSnincrHrR());
        snincrHrYSpnr.setSelection((int) sdpd.getHvySnowSnincrHrY());
        
        snincrTotRSpnr.setSelection((int) sdpd.getHvySnowSnincrTotR());
        snincrTotYSpnr.setSelection((int) sdpd.getHvySnowSnincrTotY());
        
        snowDepthRSpnr.setSelection((int) sdpd.getHvySnowDepthR());
        snowDepthYSpnr.setSelection((int) sdpd.getHvySnowDepthY());
    }

    @Override
    protected void updateData()
    {
        double val = Double.NaN;
        
        /*
         * Blizzard Warning
         */
        val = rangeUtil.getVisValueAtIndex(this.visRIndex);
        sdpd.setBlizWrnVisR(val);
        
        val = rangeUtil.getVisValueAtIndex(this.visYIndex);
        sdpd.setBlizWrnVisY(val);
        
        sdpd.setBlizWrnWindSpdR(windSpdRSpnr.getSelection());
        sdpd.setBlizWrnWindSpdY(windSpdYSpnr.getSelection());
        
        sdpd.setBlizWrnGustSpdR(gustSpdRSpnr.getSelection());
        sdpd.setBlizWrnGustSpdY(gustSpdYSpnr.getSelection());
        
        sdpd.setBlizWrnPeakWindR(peakWindRSpnr.getSelection());
        sdpd.setBlizWrnPeakWindY(peakWindYSpnr.getSelection());
        
        /*
         * Freezing Precip
         */        
        sdpd.setFrzPrecipTempR(tempRSpnr.getSelection());
        sdpd.setFrzPrecipTempY(tempYSpnr.getSelection());
        
        sdpd.setFrzPrecipHrlyPrcpR(getDecimalValueFromSpinner(hrPrecipRSpnr));
        sdpd.setFrzPrecipHrlyPrcpY(getDecimalValueFromSpinner(hrPrecipYSpnr));
        
        /*
         * Heavy Snow Warning
         */        
        sdpd.setHvySnowSnincrHrR(snincrHrRSpnr.getSelection());
        sdpd.setHvySnowSnincrHrY(snincrHrYSpnr.getSelection());
        
        sdpd.setHvySnowSnincrTotR(snincrTotRSpnr.getSelection());
        sdpd.setHvySnowSnincrTotY(snincrTotYSpnr.getSelection());
        
        sdpd.setHvySnowDepthR(snowDepthRSpnr.getSelection());
        sdpd.setHvySnowDepthY(snowDepthYSpnr.getSelection());
    }
    
    @Override
    protected void applyAction()
    {
        updateData();
        
        this.updateCB.updateThresholdData(sdpd);
    }
}
