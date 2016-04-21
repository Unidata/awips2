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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayMeteoData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

public class SnowDisplayMeteoEditDlg extends EditThresholdsDlg
{    
    
    private Spinner tempRSpnr;
    private Spinner tempYSpnr;
    
    private Spinner dewptRSpnr;
    private Spinner dewptYSpnr;
    
    private Text visRTF;
    private Button visIncR;
    private Button visDecR;
    private Text visYTF;
    private Button visIncY;
    private Button visDecY;
    
    private Spinner slpRSpnr;
    private Spinner slpYSpnr;
    
    private Spinner hrPrecipRSpnr;
    private Spinner hrPrecipYSpnr;
    
    private Spinner windChillRSpnr;
    private Spinner windChillYSpnr;
    
    private Spinner frostBiteRSpnr;
    private Spinner frostBiteYSpnr;
    
    private Spinner snowDepthRSpnr;
    private Spinner snowDepthYSpnr;
    
    private Spinner snincrHrRSpnr;
    private Spinner snincrHrYSpnr;
    private Spinner snincrTotRSpnr;
    private Spinner snincrTotYSpnr;
    
    private SnowDisplayMeteoData sdmd;
    private IUpdateDisplayMeteo updateCB;
    
    public SnowDisplayMeteoEditDlg(Shell parent, SnowDisplayMeteoData sdmd,
            IUpdateDisplayMeteo updateCB)
    {    
        super(parent, 0);
        
        this.sdmd = sdmd;
        this.updateCB = updateCB;
    }
    
    /**
     * Initialize the components on the display.
     */
    protected void initializeComponents()
    {
        shell.setText("SNOW: Display Edit Meteo");
        
        rangeUtil = RangesUtil.getInstance();
        
        createControls(); 
        addSeparator(getDialogShell());
    }
    
    private void createControls()
    {
        RangeData rd;
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Composite meteoComp = new Composite(getDialogShell(), SWT.NONE);
        meteoComp.setLayout(gl);
        meteoComp.setLayoutData(gd);
        
        createRYRangeLabels(meteoComp);
        
        /*
         * Temperature
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label tempLbl = new Label(meteoComp, SWT.RIGHT);
        tempLbl.setText("Temp (F):");
        tempLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Temp);
        
        tempRSpnr = new Spinner(meteoComp, SWT.BORDER);        
        tempYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, tempRSpnr, tempYSpnr, rd, true, rangeUtil.YValIsHigher);
        
        /*
         *  Dew Point
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label dewPtLbl = new Label(meteoComp, SWT.RIGHT);
        dewPtLbl.setText("Dewpt (F):");
        dewPtLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_DewPt);
        
        dewptRSpnr = new Spinner(meteoComp, SWT.BORDER);        
        dewptYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, dewptRSpnr, dewptYSpnr, rd, true, rangeUtil.YValIsHigher);
        
        /*
         * Visibility
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label visLbl = new Label(meteoComp, SWT.RIGHT);
        visLbl.setText("Vis (mi):");
        visLbl.setLayoutData(gd);
        
        Text[] textArray = createVisControls(meteoComp, visRTF, visIncR, visDecR, visYTF, visIncY, visDecY);
        visRTF = textArray[0];
        visYTF = textArray[1];
        
        /*
         *  SLP - Sea Level Pressure
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label slpLbl = new Label(meteoComp, SWT.RIGHT);
        slpLbl.setText("SLP (mb):");
        slpLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_SLP);
        
        slpRSpnr = new Spinner(meteoComp, SWT.BORDER);        
        slpYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, slpRSpnr, slpYSpnr, rd, true, rangeUtil.YValIsHigher);
        
        /*
         *  Hourly Precip
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label hrPrecipLbl = new Label(meteoComp, SWT.RIGHT);
        hrPrecipLbl.setText("Hourly Precip (in):");
        hrPrecipLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Precip);
        
        hrPrecipRSpnr = new Spinner(meteoComp, SWT.BORDER);        
        hrPrecipYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, hrPrecipRSpnr, hrPrecipYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        // Since the Hour Precip is in decimals we need to override the control
        // setting from the call to createSpinnerControls.
        setupDecimalSpinners(hrPrecipRSpnr, hrPrecipYSpnr, rd);
        
        /*
         *  Wind Chill
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windChillLbl = new Label(meteoComp, SWT.RIGHT);
        windChillLbl.setText("Wind Chill (F):");
        windChillLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Chill);
        
        windChillRSpnr = new Spinner(meteoComp, SWT.BORDER);
        windChillYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, windChillRSpnr, windChillYSpnr, rd, true, rangeUtil.YValIsHigher);
        
        /*
         *  Frost Bite Time
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label frostBiteLbl = new Label(meteoComp, SWT.RIGHT);
        frostBiteLbl.setText("Frost Bite Time (min):");
        frostBiteLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_FrstBt);
        
        frostBiteRSpnr = new Spinner(meteoComp, SWT.BORDER);
        frostBiteYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, frostBiteRSpnr, frostBiteYSpnr, rd, true, rangeUtil.YValIsHigher);
        
        /*
         * Snow Depth (in)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label snowDepthLbl = new Label(meteoComp, SWT.RIGHT);
        snowDepthLbl.setText("Snow Depth (in):");
        snowDepthLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Depth);
        
        snowDepthRSpnr = new Spinner(meteoComp, SWT.BORDER);
        snowDepthRSpnr.setData(rangeUtil.RValIsHigher);
        
        snowDepthYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, snowDepthRSpnr, snowDepthYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         * Snow Increasing Rapidly - Hour(in)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label snIncRapHrLbl = new Label(meteoComp, SWT.RIGHT);
        snIncRapHrLbl.setText("SNINCR Hourly (in):");
        snIncRapHrLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_SnIncRHr);
        
        snincrHrRSpnr = new Spinner(meteoComp, SWT.BORDER);
        snincrHrYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, snincrHrRSpnr, snincrHrYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         * Snow Increasing Rapidly - Total (in)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label snIncRapTotLbl = new Label(meteoComp, SWT.RIGHT);
        snIncRapTotLbl.setText("SNINCR Total (in):");
        snIncRapTotLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_SnIncRTot);
        
        snincrTotRSpnr = new Spinner(meteoComp, SWT.BORDER);
        snincrTotYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, snincrTotRSpnr, snincrTotYSpnr, rd, true, rangeUtil.RValIsHigher);
    }    
    
    @Override
    protected void updateControlsWithData()
    {
        /*
         * Temperature
         */
        tempRSpnr.setSelection((int) sdmd.getTempR());
        tempYSpnr.setSelection((int) sdmd.getTempY());
        
        /*
         * Dew point
         */
        dewptRSpnr.setSelection((int) sdmd.getDewpointR());
        dewptYSpnr.setSelection((int) sdmd.getDewpointY());
        
        /*
         * Visibility
         */
        double val = Double.NaN;
        
        val = sdmd.getVisR();
        this.visRIndex = rangeUtil.getIndexOfValue((int)val);
        this.visRTF.setText(rangeUtil.getVisString((int)val));
        
        val = sdmd.getVisY();
        this.visYIndex = rangeUtil.getIndexOfValue((int)val);
        this.visYTF.setText(rangeUtil.getVisString((int)val));
        
        /*
         * SLP
         */
        slpRSpnr.setSelection((int) sdmd.getSlpR());
        slpYSpnr.setSelection((int) sdmd.getSlpY());
        
        /*
         * Hourly precip
         */        
        hrPrecipRSpnr.setSelection((int) (sdmd.getHrPrecipR() * 10));
        hrPrecipYSpnr.setSelection((int) (sdmd.getHrPrecipY() * 10));
        
        /*
         * Wind Chill
         */
        windChillRSpnr.setSelection((int) sdmd.getWindChillR());
        windChillYSpnr.setSelection((int) sdmd.getWindChillY());
        
        /*
         * Frost Bite
         */
        frostBiteRSpnr.setSelection((int) sdmd.getFrostBiteR());
        frostBiteYSpnr.setSelection((int) sdmd.getFrostBiteY());
        
        /*
         * Snow Depth
         */
        snowDepthRSpnr.setSelection((int) sdmd.getSnowDepthR());
        snowDepthYSpnr.setSelection((int) sdmd.getSnowDepthY());
        
        /*
         * SNINCR Hourly
         */
        snincrHrRSpnr.setSelection((int) sdmd.getSnincrHrlyR());
        snincrHrYSpnr.setSelection((int) sdmd.getSnincrHrlyY());
        
        /*
         * SNINCR Total
         */
        snincrTotRSpnr.setSelection((int) sdmd.getSnincrTotR());
        snincrTotYSpnr.setSelection((int) sdmd.getSnincrTotY());
    }

    @Override
    protected void updateData()
    {
        /*
         * Temperature
         */
        sdmd.setTempR(tempRSpnr.getSelection());
        sdmd.setTempY(tempYSpnr.getSelection());
        
        /*
         * Dew point
         */
        sdmd.setDewpointR(dewptRSpnr.getSelection());
        sdmd.setDewpointY(dewptYSpnr.getSelection());
        
        /*
         * Visibility
         */
        double val = Double.NaN;
        
        val = rangeUtil.getVisValueAtIndex(this.visRIndex);
        sdmd.setVisR(val);
        
        val = rangeUtil.getVisValueAtIndex(this.visYIndex);
        sdmd.setVisY(val);
        
        /*
         * SLP
         */
        sdmd.setSlpR(slpRSpnr.getSelection());
        sdmd.setSlpY(slpYSpnr.getSelection());
        
        /*
         * Hourly Precip
         */
        sdmd.setHrPrecipR(getDecimalValueFromSpinner(hrPrecipRSpnr));
        sdmd.setHrPrecipY(getDecimalValueFromSpinner(hrPrecipYSpnr));
        
        /*
         * Wind Chill
         */
        sdmd.setWindChillR(windChillRSpnr.getSelection());
        sdmd.setWindChillY(windChillYSpnr.getSelection());
        
        /*
         * Frost Bite
         */
        sdmd.setFrostBiteR(frostBiteRSpnr.getSelection());
        sdmd.setFrostBiteY(frostBiteYSpnr.getSelection());
        
        /*
         * Snow Depth
         */
        sdmd.setSnowDepthR(snowDepthRSpnr.getSelection());
        sdmd.setSnowDepthY(snowDepthYSpnr.getSelection());
        
        /*
         * SNINCR Hourly
         */
        sdmd.setSnincrHrlyR(snincrHrRSpnr.getSelection());
        sdmd.setSnincrHrlyY(snincrHrYSpnr.getSelection());
        
        /*
         * SNINCR Total
         */
        sdmd.setSnincrTotR(snincrTotRSpnr.getSelection());
        sdmd.setSnincrTotY(snincrTotYSpnr.getSelection());
    }
    
    @Override
    protected void applyAction()
    {
        updateData();        
        this.updateCB.updateThresholdData(sdmd);
    }

}
