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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSMonitorMeteoData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

/**
 * Safeseas Monitor Meteo Edit Dialog
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
public class SSMonitorMeteoEditDlg extends EditThresholdsDlg
{
    private Spinner windSpdRSpnr;
    private Spinner windSpdYSpnr;
    private Spinner peakWindRSpnr;
    private Spinner peakWindYSpnr;
    private Spinner gustSpdRSpnr;
    private Spinner gustSpdYSpnr;
    private Spinner waveHgtRSpnr;
    private Spinner waveHgtYSpnr;

    private Text visRTF;
    private Button visIncR;
    private Button visDecR;
    private Text visYTF;
    private Button visIncY;
    private Button visDecY;

    private SSMonitorMeteoData ssmmd;
    private IUpdateMonitorMeteo updateCB;

    public SSMonitorMeteoEditDlg(Shell parent, SSMonitorMeteoData ssmmd,
            IUpdateMonitorMeteo updateCB)
    {
        super(parent);

        this.ssmmd = ssmmd;
        this.updateCB = updateCB;
    }

    /**
     * Initialize the components on the display.
     */
    @Override
    protected void initializeComponents()
    {
        shell.setText("SAFESEAS: Monitor Edit Meteo");

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
        Label windSpeedLbl = new Label(meteoComp, SWT.RIGHT);
        windSpeedLbl.setText("Wind Speed (kt):");
        windSpeedLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Wind);

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

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Wind);

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

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Gust);

        gustSpdRSpnr = new Spinner(meteoComp, SWT.BORDER);
        gustSpdYSpnr = new Spinner(meteoComp, SWT.BORDER);

        createSpinnerControls(meteoComp, gustSpdRSpnr, gustSpdYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         *  Wave Height
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label waveHgtLbl = new Label(meteoComp, SWT.RIGHT);
        waveHgtLbl.setText("Wave Height (ft):");
        waveHgtLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Wave);

        waveHgtRSpnr = new Spinner(meteoComp, SWT.BORDER);
        waveHgtYSpnr = new Spinner(meteoComp, SWT.BORDER);

        createSpinnerControls(meteoComp, waveHgtRSpnr, waveHgtYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Visibility
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windSpdLbl = new Label(meteoComp, SWT.RIGHT);
        windSpdLbl.setText("Vis (nm):");
        windSpdLbl.setLayoutData(gd);

        Text[] textArray = createVisControls(meteoComp, visRTF, visIncR, visDecR, visYTF, visIncY, visDecY);
        visRTF = textArray[0];
        visYTF = textArray[1];
    }

    @Override
    protected void updateControlsWithData()
    {
        /*
         * Wind Speed
         */
        windSpdRSpnr.setSelection((int)ssmmd.getWindSpeedR());
        windSpdYSpnr.setSelection((int)ssmmd.getWindSpeedY());

        /*
         * Peak Wind
         */
        peakWindRSpnr.setSelection((int)ssmmd.getPeakWindR());
        peakWindYSpnr.setSelection((int)ssmmd.getPeakWindY());

        /*
         * Gust Speed
         */
        gustSpdRSpnr.setSelection((int)ssmmd.getGustSpeedR());
        gustSpdYSpnr.setSelection((int)ssmmd.getGustSpeedY());

        /*
         * Wave Height
         */
        waveHgtRSpnr.setSelection((int)ssmmd.getWaveHgtR());
        waveHgtYSpnr.setSelection((int)ssmmd.getWaveHgtY());

        /*
         * Visibility
         */
        double val = Double.NaN;

        val = ssmmd.getVisR();
        this.visRIndex = rangeUtil.getIndexOfValue((int)val);
        this.visRTF.setText(rangeUtil.getVisString((int)val));

        val = ssmmd.getVisY();
        this.visYIndex = rangeUtil.getIndexOfValue((int)val);
        this.visYTF.setText(rangeUtil.getVisString((int)val));
    }

    @Override
    protected void updateData()
    {
        /*
         * Wind Speed
         */
        ssmmd.setWindSpeedR(this.windSpdRSpnr.getSelection());
        ssmmd.setWindSpeedY(this.windSpdYSpnr.getSelection());

        /*
         * Peak Wind
         */
        ssmmd.setPeakWindR(this.peakWindRSpnr.getSelection());
        ssmmd.setPeakWindY(this.peakWindYSpnr.getSelection());

        /*
         * Gust Speed
         */
        ssmmd.setGustSpeedR(this.gustSpdRSpnr.getSelection());
        ssmmd.setGustSpeedY(this.gustSpdYSpnr.getSelection());

        /*
         * Wave Height
         */
        ssmmd.setWaveHgtR(this.waveHgtRSpnr.getSelection());
        ssmmd.setWaveHgtY(this.waveHgtYSpnr.getSelection());

        /*
         * Visibility
         */
        double val = Double.NaN;

        val = rangeUtil.getVisValueAtIndex(this.visRIndex);
        ssmmd.setVisR(val);

        val = rangeUtil.getVisValueAtIndex(this.visYIndex);
        ssmmd.setVisY(val);

    }

    @Override
    protected void applyAction()
    {
        updateData();
        this.updateCB.updateThresholdData(ssmmd);
    }
}
