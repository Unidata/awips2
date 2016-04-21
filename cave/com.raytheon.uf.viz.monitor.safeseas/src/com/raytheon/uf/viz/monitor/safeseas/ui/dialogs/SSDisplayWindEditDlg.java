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
package com.raytheon.uf.viz.monitor.safeseas.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDisplayWindData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

public class SSDisplayWindEditDlg extends EditThresholdsDlg
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
    
    private SSDisplayWindData ssdwd;
    private IUpdateDisplayWind updateCB;
    
    public SSDisplayWindEditDlg(Shell parent, SSDisplayWindData ssdwd,
            IUpdateDisplayWind updateCB)
    {    
        super(parent, 0);
        
        this.ssdwd = ssdwd;
        this.updateCB = updateCB;
    }
    
    /**
     * Initialize the components on the display.
     */
    protected void initializeComponents()
    {
        shell.setText("SAFESEAS: Display Edit Wind");
        
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
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Wind);
        
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
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Wind);
        
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
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Gust);
        
        gustSpdRSpnr = new Spinner(windComp, SWT.BORDER);
        gustSpdYSpnr = new Spinner(windComp, SWT.BORDER);
        
        createSpinnerControls(windComp, gustSpdRSpnr, gustSpdYSpnr, rd, true, rangeUtil.RValIsHigher);
    }
    
    /**
     * Create controls for wind or swell directions
     */
    private void createDirControls(){
        RangeData rd;
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(6, false);
        gl.verticalSpacing = 10;
        Composite windComp = new Composite(getDialogShell(), SWT.NONE);
        windComp.setLayout(gl);
        windComp.setLayoutData(gd);
        
        createYRRYRangeLabels(windComp);
        
        /*
         * Wind Direction (from)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windDirFromLbl = new Label(windComp, SWT.RIGHT);
        windDirFromLbl.setText("Wind Direction:");
        windDirFromLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_WindDirF);
        
        windDirFromYSpnr = new Spinner(windComp, SWT.BORDER);
        windDirFromRSpnr = new Spinner(windComp, SWT.BORDER);
        
        //createSpinnerControls(windComp, windDirFromRSpnr, windDirFromYSpnr, rd, false, rangeUtil.ryNotNeeded);
        //addIncrementListeners(windDirFromRSpnr, windDirFromYSpnr);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_WindDirT);
        
        windDirToRSpnr = new Spinner(windComp, SWT.BORDER);
        windDirToYSpnr = new Spinner(windComp, SWT.BORDER);
        
        createSpinnerControls(windComp, windDirFromYSpnr, windDirFromRSpnr, windDirToRSpnr, windDirToYSpnr, rd, false, rangeUtil.ryNotNeeded);
        addIncrementListeners(windDirFromRSpnr, windDirFromYSpnr);
        addIncrementListeners(windDirToRSpnr, windDirToYSpnr);
    }
    @Override
    protected void updateControlsWithData()
    {
        /*
         * Wind Speed
         */
        windSpdRSpnr.setSelection((int)ssdwd.getWindSpeedR());
        windSpdYSpnr.setSelection((int)ssdwd.getWindSpeedY());
        
        /*
         * Gust Speed
         */
        gustSpdRSpnr.setSelection((int)ssdwd.getGustSpeedR());
        gustSpdYSpnr.setSelection((int)ssdwd.getGustSpeedY());
        
        /*
         * Peak Wind
         */
        peakWindRSpnr.setSelection((int)ssdwd.getPeakWindR());
        peakWindYSpnr.setSelection((int)ssdwd.getPeakWindY());
        
        /*
         * Wind Direction From
         */
        windDirFromRSpnr.setSelection((int)ssdwd.getWindDirFromR());
        windDirFromYSpnr.setSelection((int)ssdwd.getWindDirFromY());
        
        /*
         * Wind Direction To
         */
        windDirToRSpnr.setSelection((int)ssdwd.getWindDirToR());
        windDirToYSpnr.setSelection((int)ssdwd.getWindDirToY());
    }
    
    @Override
    protected void updateData()
    {
        /*
         * Wind Speed
         */
        ssdwd.setWindSpeedR(windSpdRSpnr.getSelection());
        ssdwd.setWindSpeedY(windSpdYSpnr.getSelection());
        
        /*
         * Gust Speed
         */
        ssdwd.setGustSpeedR(gustSpdRSpnr.getSelection());
        ssdwd.setGustSpeedY(gustSpdYSpnr.getSelection());
        
        /*
         * Peak Wind
         */
        ssdwd.setPeakWindR(peakWindRSpnr.getSelection());
        ssdwd.setPeakWindY(peakWindYSpnr.getSelection());
        
        /*
         * Wind Direction From
         */
        ssdwd.setWindDirFromR(windDirFromRSpnr.getSelection());
        ssdwd.setWindDirFromY(windDirFromYSpnr.getSelection());
        
        /*
         * Wind Direction To
         */
        ssdwd.setWindDirToR(windDirToRSpnr.getSelection());
        ssdwd.setWindDirToY(windDirToYSpnr.getSelection());
    }
    
    protected void applyAction()
    {
        updateData();
        
        this.updateCB.updateThresholdData(ssdwd);
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
