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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.fog.threshold.FogDisplayMeteoData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

/**
 * Fog's Edit Meteo dialog for the Display type.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class FogDisplayMeteoEditDlg extends EditThresholdsDlg
{        
    /**
     * Visibility "R" text control.
     */
    private Text visRTF = null;
    
    /**
     * Visibility increase "R" button.
     */
    private Button visIncR = null;
    
    /**
     * Visibility decrease "R" button.
     */
    private Button visDecR = null;
    
    /**
     * Visibility "Y" text control.
     */
    private Text visYTF = null;
    
    /**
     * Visibility increase "Y" button.
     */
    private Button visIncY = null;
    
    /**
     * Visibility decrease "Y" button.
     */
    private Button visDecY = null;
    
    /**
     * Ceiling R spinner control.
     */
    private Spinner ceilingRSpnr = null;
    
    /**
     * Ceiling Y spinner control.
     */
    private Spinner ceilingYSpnr = null;
    
    /**
     * Temp R spinner control.
     */
    private Spinner tempRSpnr = null;
    
    /**
     * Temp Y spinner control.
     */
    private Spinner tempYSpnr = null;
    
    /**
     * Dewpoint R spinner control.
     */
    private Spinner dewptRSpnr = null;
    
    /**
     * Dewpoint Y spinner control.
     */
    private Spinner dewptYSpnr = null;
    
    /**
     * Depression R spinner control.
     */
    private Spinner depressionRSpnr = null;
    
    /**
     * Depression Y spinner control.
     */
    private Spinner depressionYSpnr = null;
    
    /**
     * Relative Humidity R spinner control.
     */
    private Spinner relHumidRSpnr = null;
    
    /**
     * Relative Humidity Y spinner control.
     */
    private Spinner relHumidYSpnr = null;
    
    private FogDisplayMeteoData fdmd;
    
    private IUpdateDisplayMeteo updateCB;
    
    /**
     * Constructor.
     * @param parent Parent shell.
     */
    public FogDisplayMeteoEditDlg(Shell parent, FogDisplayMeteoData fdmd, IUpdateDisplayMeteo updateCB)
    {    
        super(parent, 0);
        
        this.fdmd = fdmd;
        this.updateCB = updateCB;
    }
    
    /**
     * Initialize the components on the display.
     */
    protected void initializeComponents()
    {
        shell.setText("FOG: Display Edit Meteo");
        
        rangeUtil = RangesUtil.getInstance();
        
        createControls(); 
        addSeparator(getDialogShell());
    }
    
    /**
     * Create the controls.
     */
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
         *  Ceiling (100ft)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label ceilingLbl = new Label(meteoComp, SWT.RIGHT);
        ceilingLbl.setText("Ceiling (100ft):");
        ceilingLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.FOG_Ceil);
        
        ceilingRSpnr = new Spinner(meteoComp, SWT.BORDER);
        ceilingYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, ceilingRSpnr, ceilingYSpnr, rd, true, rangeUtil.YValIsHigher);
        
        /*
         * Temperature
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label tempLbl = new Label(meteoComp, SWT.RIGHT);
        tempLbl.setText("Temp (F):");
        tempLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.FOG_Temp);
        
        tempRSpnr = new Spinner(meteoComp, SWT.BORDER);
        tempYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, tempRSpnr, tempYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         *  Dew Point
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label dewPtLbl = new Label(meteoComp, SWT.RIGHT);
        dewPtLbl.setText("Dewpt (F):");
        dewPtLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.FOG_DewPt);
        
        dewptRSpnr = new Spinner(meteoComp, SWT.BORDER);
        dewptYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, dewptRSpnr, dewptYSpnr, rd, true, rangeUtil.RValIsHigher);
        
        /*
         *  Depression
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label depressionLbl = new Label(meteoComp, SWT.RIGHT);
        depressionLbl.setText("T-Td (F):");
        depressionLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.FOG_Depr);
        
        depressionRSpnr = new Spinner(meteoComp, SWT.BORDER);
        depressionYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, depressionRSpnr, depressionYSpnr, rd, true, rangeUtil.YValIsHigher);
        
        /*
         *  Relative Humidity
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label relHumidLbl = new Label(meteoComp, SWT.RIGHT);
        relHumidLbl.setText("Relative Humidity (%):");
        relHumidLbl.setLayoutData(gd);
        
        rd = rangeUtil.getRangeData(RangesUtil.RangeType.FOG_Humid);
        
        relHumidRSpnr = new Spinner(meteoComp, SWT.BORDER);
        relHumidYSpnr = new Spinner(meteoComp, SWT.BORDER);
        
        createSpinnerControls(meteoComp, relHumidRSpnr, relHumidYSpnr, rd, true, rangeUtil.RValIsHigher);
    }
    
    @Override
    protected void updateControlsWithData()
    {
        double val = Double.NaN;
        
        /*
         * Visibility
         */
        val = fdmd.getMeteoVisR();
        this.visRIndex = rangeUtil.getIndexOfValue((int)val);
        this.visRTF.setText(rangeUtil.getVisString((int)val));
        
        val = fdmd.getMeteoVisY();
        this.visYIndex = rangeUtil.getIndexOfValue((int)val);
        this.visYTF.setText(rangeUtil.getVisString((int)val));
        
        /*
         * Ceiling
         */
        this.ceilingRSpnr.setSelection((int)fdmd.getMeteoCeilingR());
        this.ceilingYSpnr.setSelection((int)fdmd.getMeteoCeilingY());
        
        /*
         * Temperature
         */
        this.tempRSpnr.setSelection((int)fdmd.getMeteoTempR());
        this.tempYSpnr.setSelection((int)fdmd.getMeteoTempY());
        
        /*
         * Dewpoint
         */
        this.dewptRSpnr.setSelection((int)fdmd.getMeteoDewpointR());
        this.dewptYSpnr.setSelection((int)fdmd.getMeteoDewpointY());
        
        /*
         * T-Td
         */
        this.depressionRSpnr.setSelection((int)fdmd.getMeteoTtdR());
        this.depressionYSpnr.setSelection((int)fdmd.getMeteoTtdY());
        
        /*
         * Relative Humidity
         */
        this.relHumidRSpnr.setSelection((int)fdmd.getMeteoRelHumR());
        this.relHumidYSpnr.setSelection((int)fdmd.getMeteoRelHumY());
    }
    
    /**
     * Update the data structure with the data in the controls.
     */
    @Override
    protected void updateData()
    {
        double val = Double.NaN;
        
        /*
         * Visibility
         */
        val = rangeUtil.getVisValueAtIndex(this.visRIndex);
        fdmd.setMeteoVisR(val);
        
        val = rangeUtil.getVisValueAtIndex(this.visYIndex);
        fdmd.setMeteoVisY(val);
        
        /*
         * Ceiling
         */
        fdmd.setMeteoCeilingR(this.ceilingRSpnr.getSelection());
        fdmd.setMeteoCeilingY(this.ceilingYSpnr.getSelection());
        
        /*
         * Temperature
         */
        fdmd.setMeteoTempR(this.tempRSpnr.getSelection());
        fdmd.setMeteoTempY(this.tempYSpnr.getSelection());
        
        /*
         * Dewpoint
         */
        fdmd.setMeteoDewpointR(this.dewptRSpnr.getSelection());
        fdmd.setMeteoDewpointY(this.dewptYSpnr.getSelection());
        
        /*
         * T-Td
         */
        fdmd.setMeteoTtdR(this.depressionRSpnr.getSelection());
        fdmd.setMeteoTtdY(this.depressionYSpnr.getSelection());
        
        /*
         * Relative Humidity
         */
        fdmd.setMeteoRelHumR(this.relHumidRSpnr.getSelection());
        fdmd.setMeteoRelHumY(this.relHumidYSpnr.getSelection());
    }

    /**
     * Handle the action when the Apply button is pressed.
     */
    @Override
    protected void applyAction()
    {
        updateData();
        
        this.updateCB.updateThresholdData(fdmd);
    }
}
