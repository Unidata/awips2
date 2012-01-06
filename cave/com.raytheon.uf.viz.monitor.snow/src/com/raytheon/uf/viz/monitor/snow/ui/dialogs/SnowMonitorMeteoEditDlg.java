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
import com.raytheon.uf.viz.monitor.snow.threshold.SnowMonitorMeteoData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

public class SnowMonitorMeteoEditDlg extends EditThresholdsDlg
{    
    
    private Spinner windSpdRSpnr;
    private Spinner windSpdYSpnr;
    private Spinner gustSpdRSpnr;
    private Spinner gustSpdYSpnr;
    private Spinner peakWindRSpnr;
    private Spinner peakWindYSpnr;
    
    private Spinner tempRSpnr;
    private Spinner tempYSpnr;
    
    private Spinner windChillRSpnr;
    private Spinner windChillYSpnr;
    
    private Text visRTF;
    private Button visIncR;
    private Button visDecR;
    private Text visYTF;
    private Button visIncY;
    private Button visDecY;
    
    private Spinner snowDepthRSpnr;
    private Spinner snowDepthYSpnr;
    
    private SnowMonitorMeteoData smmd;
    private IUpdateMonitorMeteo updateCB;
    
    public SnowMonitorMeteoEditDlg(Shell parent, SnowMonitorMeteoData smmd,
            IUpdateMonitorMeteo updateCB)
    {    
        super(parent, 0);
        
        this.smmd = smmd;
        this.updateCB = updateCB;
    }
    
    /**
     * Initialize the components on the display.
     */
    protected void initializeComponents()
    {
        shell.setText("SNOW: Monitor Edit Meteo");
        
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
         * Wind Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windSpdLbl = new Label(meteoComp, SWT.RIGHT);
        windSpdLbl.setText("Wind Speed (kt):");
        windSpdLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Wind);
        
        windSpdRSpnr = new Spinner(meteoComp, SWT.BORDER);
        windSpdYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, windSpdRSpnr, windSpdYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         * Peak Wind
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label peakWindLbl = new Label(meteoComp, SWT.RIGHT);
        peakWindLbl.setText("Peak Wind (kt):");
        peakWindLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Wind);
        
        peakWindRSpnr = new Spinner(meteoComp, SWT.BORDER);
        peakWindYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, peakWindRSpnr, peakWindYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         * Gust Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label gustLbl = new Label(meteoComp, SWT.RIGHT);
        gustLbl.setText("Gust Speed (kt):");
        gustLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Wind);
        
        gustSpdRSpnr = new Spinner(meteoComp, SWT.BORDER);
        gustSpdYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, gustSpdRSpnr, gustSpdYSpnr, rd, true, rangeUtil.RValIsHigher);
        
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
         * Snow Depth (in)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label snowDepthLbl = new Label(meteoComp, SWT.RIGHT);
        snowDepthLbl.setText("Snow Depth (in):");
        snowDepthLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SNOW_Depth);
        
        snowDepthRSpnr = new Spinner(meteoComp, SWT.BORDER);
        snowDepthYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, snowDepthRSpnr, snowDepthYSpnr, rd, true, rangeUtil.RValIsHigher);
    }   

    @Override
    protected void updateControlsWithData()
    {  
        /*
         * Wind Speed
         */
        windSpdRSpnr.setSelection((int)smmd.getWindSpeedR());
        windSpdYSpnr.setSelection((int)smmd.getWindSpeedY());
        
        /*
         * Peak Wind 
         */
        peakWindRSpnr.setSelection((int)smmd.getPeakWindR());
        peakWindYSpnr.setSelection((int)smmd.getPeakWindY());
        
        /*
         * Gust Speed
         */
        gustSpdRSpnr.setSelection((int)smmd.getGustSpeedR());
        gustSpdYSpnr.setSelection((int)smmd.getGustSpeedY());
        
        /*
         * Temperature
         */
        tempRSpnr.setSelection((int)smmd.getTempR());
        tempYSpnr.setSelection((int)smmd.getTempY());
        
        /*
         * Wind Chill
         */
        windChillRSpnr.setSelection((int)smmd.getWindChillR());
        windChillYSpnr.setSelection((int)smmd.getWindChillY());
        
        /*
         * Visibility
         */
        double val = Double.NaN;
        
        val = smmd.getVisR();
        this.visRIndex = rangeUtil.getIndexOfValue((int)val);
        this.visRTF.setText(rangeUtil.getVisString((int)val));
        
        val = smmd.getVisY();
        this.visYIndex = rangeUtil.getIndexOfValue((int)val);
        this.visYTF.setText(rangeUtil.getVisString((int)val));
        
        /*
         * Snow Depth
         */
        
        snowDepthRSpnr.setSelection((int)smmd.getSnowDepthR());
        snowDepthYSpnr.setSelection((int)smmd.getSnowDepthY());
    }

    @Override
    protected void updateData()
    {                   
        /*
         * Wind Speed
         */
        smmd.setWindSpeedR(windSpdRSpnr.getSelection());
        smmd.setWindSpeedY(windSpdYSpnr.getSelection());
        
        /*
         * Peak Wind
         */
        smmd.setPeakWindR(peakWindRSpnr.getSelection());
        smmd.setPeakWindY(peakWindYSpnr.getSelection());
        
        /*
         * Gust Speed
         */
        smmd.setGustSpeedR(gustSpdRSpnr.getSelection());
        smmd.setGustSpeedY(gustSpdYSpnr.getSelection());
        
        /*
         * Temperature
         */
        smmd.setTempR(tempRSpnr.getSelection());
        smmd.setTempY(tempYSpnr.getSelection());
        
        /*
         * Wind Chill
         */
        smmd.setWindChillR(windChillRSpnr.getSelection());
        smmd.setWindChillY(windChillYSpnr.getSelection());
        
        /*
         * Visibility
         */
        double val = Double.NaN;
        
        val = rangeUtil.getVisValueAtIndex(this.visRIndex);
        smmd.setVisR(val);
        
        val = rangeUtil.getVisValueAtIndex(this.visYIndex);
        smmd.setVisY(val);
        
        /*
         * Snow Depth
         */
        smmd.setSnowDepthR(snowDepthRSpnr.getSelection());
        smmd.setSnowDepthY(snowDepthYSpnr.getSelection());
    }
    
    @Override
    protected void applyAction()
    {
        updateData();
        
        this.updateCB.updateThresholdData(smmd);
    }
}
