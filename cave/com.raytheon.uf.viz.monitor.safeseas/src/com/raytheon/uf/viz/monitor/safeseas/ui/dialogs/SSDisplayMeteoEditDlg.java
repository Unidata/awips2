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
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDisplayMeteoData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

/**
 * Safeseas Display Meteo Edit Dialog
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
public class SSDisplayMeteoEditDlg extends EditThresholdsDlg
{
    private Text visRTF;
    private Button visIncR;
    private Button visDecR;
    private Text visYTF;
    private Button visIncY;
    private Button visDecY;

    private Spinner tempRSpnr;
    private Spinner tempYSpnr;

    private Spinner dewptRSpnr;
    private Spinner dewptYSpnr;

    private Spinner slpRSpnr;
    private Spinner slpYSpnr;

    private Spinner sstRSpnr;
    private Spinner sstYSpnr;

    private Spinner waveHgtRSpnr;
    private Spinner waveHgtYSpnr;

    private Spinner waveSteepRSpnr;
    private Spinner waveSteepYSpnr;

    private SSDisplayMeteoData ssdmd;
    private IUpdateDisplayMeteo updateCB;

    public SSDisplayMeteoEditDlg(Shell parent, SSDisplayMeteoData ssdmd,
            IUpdateDisplayMeteo updateCB)
    {
        super(parent);

        this.ssdmd = ssdmd;
        this.updateCB = updateCB;
    }

    /**
     * Initialize the components on the display.
     */
    @Override
    protected void initializeComponents()
    {
        shell.setText("SAFESEAS: Display Edit Meteo");

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
         * Visibility
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windSpdLbl = new Label(meteoComp, SWT.RIGHT);
        windSpdLbl.setText("Vis (nm):");
        windSpdLbl.setLayoutData(gd);

        Text[] textArray = createVisControls(meteoComp, visRTF, visIncR, visDecR, visYTF,
                visIncY, visDecY);
        visRTF = textArray[0];
        visYTF = textArray[1];

        /*
         * Temperature
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label tempLbl = new Label(meteoComp, SWT.RIGHT);
        tempLbl.setText("Temp (F):");
        tempLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Temp);

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

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_DewPt);

        dewptRSpnr = new Spinner(meteoComp, SWT.BORDER);
        dewptYSpnr = new Spinner(meteoComp, SWT.BORDER);

        createSpinnerControls(meteoComp, dewptRSpnr, dewptYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         *  SLP - Sea Level Pressure
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label slpLbl = new Label(meteoComp, SWT.RIGHT);
        slpLbl.setText("SLP (mb):");
        slpLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_SLP);

        slpRSpnr = new Spinner(meteoComp, SWT.BORDER);
        slpYSpnr = new Spinner(meteoComp, SWT.BORDER);

        createSpinnerControls(meteoComp, slpRSpnr, slpYSpnr, rd, true, rangeUtil.YValIsHigher);

        /*
         *  SST - Sea Surface Temperature
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label sstLbl = new Label(meteoComp, SWT.RIGHT);
        sstLbl.setText("SST (F):");
        sstLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_SST);

        sstRSpnr = new Spinner(meteoComp, SWT.BORDER);
        sstYSpnr = new Spinner(meteoComp, SWT.BORDER);

        createSpinnerControls(meteoComp, sstRSpnr, sstYSpnr, rd, true, rangeUtil.RValIsHigher);

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
         *  Wave Steep
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label waveSteepLbl = new Label(meteoComp, SWT.RIGHT);
        waveSteepLbl.setText("Wave Steep (x1000):");
        waveSteepLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Steep);

        waveSteepRSpnr = new Spinner(meteoComp, SWT.BORDER);
        waveSteepYSpnr = new Spinner(meteoComp, SWT.BORDER);

        createSpinnerControls(meteoComp, waveSteepRSpnr, waveSteepYSpnr, rd, true, rangeUtil.RValIsHigher);
    }

    @Override
    protected void updateControlsWithData()
    {
        double val = Double.NaN;

        /*
         * Visibility
         */
        val = ssdmd.getVisR();
        this.visRIndex = rangeUtil.getIndexOfValue((int)val);
        this.visRTF.setText(rangeUtil.getVisString((int)val));

        val = ssdmd.getVisY();
        this.visYIndex = rangeUtil.getIndexOfValue((int)val);
        this.visYTF.setText(rangeUtil.getVisString((int)val));

        /*
         * Temperature
         */
        tempRSpnr.setSelection((int)ssdmd.getTempR());
        tempYSpnr.setSelection((int)ssdmd.getTempY());

        /*
         * Dew point
         */
        dewptRSpnr.setSelection((int)ssdmd.getDewpointR());
        dewptYSpnr.setSelection((int)ssdmd.getDewpointY());

        /*
         * SLP
         */
        slpRSpnr.setSelection((int)ssdmd.getSlpR());
        slpYSpnr.setSelection((int)ssdmd.getSlpY());

        /*
         * SST
         */
        sstRSpnr.setSelection((int)ssdmd.getSstR());
        sstYSpnr.setSelection((int)ssdmd.getSstY());

        /*
         * Wave Height
         */
        waveHgtRSpnr.setSelection((int)ssdmd.getWaveHgtR());
        waveHgtYSpnr.setSelection((int)ssdmd.getWaveHgtY());

        /*
         * Wave Steep
         */
        waveSteepRSpnr.setSelection((int)ssdmd.getWaveSteepR());
        waveSteepYSpnr.setSelection((int)ssdmd.getWaveSteepY());
    }

    @Override
    protected void updateData()
    {
        double val = Double.NaN;

        /*
         * Visibility
         */
        val = rangeUtil.getVisValueAtIndex(this.visRIndex);
        ssdmd.setVisR(val);

        val = rangeUtil.getVisValueAtIndex(this.visYIndex);
        ssdmd.setVisY(val);

        /*
         * Temperature
         */
        ssdmd.setTempR(tempRSpnr.getSelection());
        ssdmd.setTempY(tempYSpnr.getSelection());

        /*
         * Dew point
         */
        ssdmd.setDewpointR(dewptRSpnr.getSelection());
        ssdmd.setDewpointY(dewptYSpnr.getSelection());

        /*
         * SLP
         */
        ssdmd.setSlpR(slpRSpnr.getSelection());
        ssdmd.setSlpY(slpYSpnr.getSelection());

        /*
         * SST
         */
        ssdmd.setSstR(sstRSpnr.getSelection());
        ssdmd.setSstY(sstYSpnr.getSelection());

        /*
         * Wave Height
         */
        ssdmd.setWaveHgtR(waveHgtRSpnr.getSelection());
        ssdmd.setWaveHgtY(waveHgtYSpnr.getSelection());

        /*
         * Wave Steep
         */
        ssdmd.setWaveSteepR(waveSteepRSpnr.getSelection());
        ssdmd.setWaveSteepY(waveSteepYSpnr.getSelection());
    }

    @Override
    protected void applyAction()
    {
        updateData();

        this.updateCB.updateThresholdData(ssdmd);
    }
}
