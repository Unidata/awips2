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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDisplayProductData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

/**
 * Safeseas Display Product Edit Dialog
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
public class SSDisplayProductEditDlg extends EditThresholdsDlg
{
    private Spinner scaWindSpdRSpnr;
    private Spinner scaWindSpdYSpnr;
    private Spinner scaGustSpdRSpnr;
    private Spinner scaGustSpdYSpnr;
    private Spinner scaPeakWindRSpnr;
    private Spinner scaPeakWindYSpnr;
    private Spinner scaWaveHgtRSpnr;
    private Spinner scaWaveHgtYSpnr;

    private Spinner galeWindSpdRSpnr;
    private Spinner galeWindSpdYSpnr;
    private Spinner galeGustSpdRSpnr;
    private Spinner galeGustSpdYSpnr;
    private Spinner galePeakWindRSpnr;
    private Spinner galePeakWindYSpnr;

    private Spinner stormWindSpdRSpnr;
    private Spinner stormWindSpdYSpnr;
    private Spinner stormGustSpdRSpnr;
    private Spinner stormGustSpdYSpnr;
    private Spinner stormPeakWindRSpnr;
    private Spinner stormPeakWindYSpnr;

    private Spinner hfwwWindSpdRSpnr;
    private Spinner hfwwWindSpdYSpnr;
    private Spinner hfwwGustSpdRSpnr;
    private Spinner hfwwGustSpdYSpnr;
    private Spinner hfwwPeakWindRSpnr;
    private Spinner hfwwPeakWindYSpnr;

    private SSDisplayProductData ssdpd;
    private IUpdateDisplayProduct updateCB;

    public SSDisplayProductEditDlg(Shell parent, SSDisplayProductData ssdpd,
            IUpdateDisplayProduct updateCB)
    {
        super(parent);

        this.ssdpd = ssdpd;
        this.updateCB = updateCB;
    }

    /**
     * Initialize the components on the display.
     */
    @Override
    protected void initializeComponents()
    {
        shell.setText("SAFESEAS: Display Edit Product");

        rangeUtil = RangesUtil.getInstance();

        createSmallCraftAdvisoryGroup();
        createGaleWarningGroup();
        createStormWarningGroup();
        createHFWWGroup();
    }

    private void createSmallCraftAdvisoryGroup()
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Group scaGroup = new Group(getDialogShell(), SWT.NONE);
        scaGroup.setLayout(gl);
        scaGroup.setLayoutData(gd);
        scaGroup.setText(" Small Craft Advisory ");

        createRYRangeLabels(scaGroup);

        /*
         * Wind Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windSpdLbl = new Label(scaGroup, SWT.RIGHT);
        windSpdLbl.setText("Wind Speed (kt):");
        windSpdLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_ScaWind);

        scaWindSpdRSpnr = new Spinner(scaGroup, SWT.BORDER);
        scaWindSpdYSpnr = new Spinner(scaGroup, SWT.BORDER);

        createSpinnerControls(scaGroup, scaWindSpdRSpnr, scaWindSpdYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Gust Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label gustSpdLbl = new Label(scaGroup, SWT.RIGHT);
        gustSpdLbl.setText("Gust Speed (kt):");
        gustSpdLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_ScaGust);

        scaGustSpdRSpnr = new Spinner(scaGroup, SWT.BORDER);
        scaGustSpdYSpnr = new Spinner(scaGroup, SWT.BORDER);

        createSpinnerControls(scaGroup, scaGustSpdRSpnr, scaGustSpdYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Peak Wind
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label peakWindLbl = new Label(scaGroup, SWT.RIGHT);
        peakWindLbl.setText("Peak Wind (kt):");
        peakWindLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_ScaMax);

        scaPeakWindRSpnr = new Spinner(scaGroup, SWT.BORDER);
        scaPeakWindYSpnr = new Spinner(scaGroup, SWT.BORDER);

        createSpinnerControls(scaGroup, scaPeakWindRSpnr, scaPeakWindYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Wave Height
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label waveHgtLbl = new Label(scaGroup, SWT.RIGHT);
        waveHgtLbl.setText("Wave Height (ft):");
        waveHgtLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_Wave);

        scaWaveHgtRSpnr = new Spinner(scaGroup, SWT.BORDER);
        scaWaveHgtYSpnr = new Spinner(scaGroup, SWT.BORDER);

        createSpinnerControls(scaGroup, scaWaveHgtRSpnr, scaWaveHgtYSpnr, rd, true, rangeUtil.RValIsHigher);
    }

    private void createGaleWarningGroup()
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Group galeWarnGroup = new Group(getDialogShell(), SWT.NONE);
        galeWarnGroup.setLayout(gl);
        galeWarnGroup.setLayoutData(gd);
        galeWarnGroup.setText(" Gale Warning ");

        createRYRangeLabels(galeWarnGroup);

        /*
         * Wind Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windSpdLbl = new Label(galeWarnGroup, SWT.RIGHT);
        windSpdLbl.setText("Wind Speed (kt):");
        windSpdLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_GaleWind);

        galeWindSpdRSpnr = new Spinner(galeWarnGroup, SWT.BORDER);
        galeWindSpdYSpnr = new Spinner(galeWarnGroup, SWT.BORDER);

        createSpinnerControls(galeWarnGroup, galeWindSpdRSpnr, galeWindSpdYSpnr, rd, true,
                rangeUtil.RValIsHigher);

        /*
         * Gust Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label gustSpdLbl = new Label(galeWarnGroup, SWT.RIGHT);
        gustSpdLbl.setText("Gust Speed (kt):");
        gustSpdLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_GaleGust);

        galeGustSpdRSpnr = new Spinner(galeWarnGroup, SWT.BORDER);
        galeGustSpdYSpnr = new Spinner(galeWarnGroup, SWT.BORDER);

        createSpinnerControls(galeWarnGroup, galeGustSpdRSpnr, galeGustSpdYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Peak Wind
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label peakWindLbl = new Label(galeWarnGroup, SWT.RIGHT);
        peakWindLbl.setText("Peak Wind (kt):");
        peakWindLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_GaleMax);

        galePeakWindRSpnr = new Spinner(galeWarnGroup, SWT.BORDER);
        galePeakWindYSpnr = new Spinner(galeWarnGroup, SWT.BORDER);

        createSpinnerControls(galeWarnGroup, galePeakWindRSpnr, galePeakWindYSpnr, rd, true,
                rangeUtil.RValIsHigher);
    }

    private void createStormWarningGroup()
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Group stormWarnGroup = new Group(getDialogShell(), SWT.NONE);
        stormWarnGroup.setLayout(gl);
        stormWarnGroup.setLayoutData(gd);
        stormWarnGroup.setText(" Storm Warning ");

        createRYRangeLabels(stormWarnGroup);

        /*
         * Wind Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windSpdLbl = new Label(stormWarnGroup, SWT.RIGHT);
        windSpdLbl.setText("Wind Speed (kt):");
        windSpdLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_StormWind);

        stormWindSpdRSpnr = new Spinner(stormWarnGroup, SWT.BORDER);
        stormWindSpdYSpnr = new Spinner(stormWarnGroup, SWT.BORDER);

        createSpinnerControls(stormWarnGroup, stormWindSpdRSpnr, stormWindSpdYSpnr, rd, true,
                rangeUtil.RValIsHigher);

        /*
         * Gust Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label gustSpdLbl = new Label(stormWarnGroup, SWT.RIGHT);
        gustSpdLbl.setText("Gust Speed (kt):");
        gustSpdLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_StormGust);

        stormGustSpdRSpnr = new Spinner(stormWarnGroup, SWT.BORDER);
        stormGustSpdYSpnr = new Spinner(stormWarnGroup, SWT.BORDER);

        createSpinnerControls(stormWarnGroup, stormGustSpdRSpnr, stormGustSpdYSpnr, rd, true,
                rangeUtil.RValIsHigher);

        /*
         * Peak Wind
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label peakWindLbl = new Label(stormWarnGroup, SWT.RIGHT);
        peakWindLbl.setText("Peak Wind (kt):");
        peakWindLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_StormMax);

        stormPeakWindRSpnr = new Spinner(stormWarnGroup, SWT.BORDER);
        stormPeakWindYSpnr = new Spinner(stormWarnGroup, SWT.BORDER);

        createSpinnerControls(stormWarnGroup, stormPeakWindRSpnr, stormPeakWindYSpnr, rd, true,
                rangeUtil.RValIsHigher);
    }

    private void createHFWWGroup()
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Group hfwwGroup = new Group(getDialogShell(), SWT.NONE);
        hfwwGroup.setLayout(gl);
        hfwwGroup.setLayoutData(gd);
        hfwwGroup.setText(" HFWW - Hurricane Force Wind Warning ");

        createRYRangeLabels(hfwwGroup);

        /*
         * Wind Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label windSpdLbl = new Label(hfwwGroup, SWT.RIGHT);
        windSpdLbl.setText("Wind Speed (kt):");
        windSpdLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_HfwwWind);

        hfwwWindSpdRSpnr = new Spinner(hfwwGroup, SWT.BORDER);
        hfwwWindSpdYSpnr = new Spinner(hfwwGroup, SWT.BORDER);

        createSpinnerControls(hfwwGroup, hfwwWindSpdRSpnr, hfwwWindSpdYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Gust Speed
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label gustSpdLbl = new Label(hfwwGroup, SWT.RIGHT);
        gustSpdLbl.setText("Gust Speed (kt):");
        gustSpdLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_HfwwGust);

        hfwwGustSpdRSpnr = new Spinner(hfwwGroup, SWT.BORDER);
        hfwwGustSpdYSpnr = new Spinner(hfwwGroup, SWT.BORDER);

        createSpinnerControls(hfwwGroup, hfwwGustSpdRSpnr, hfwwGustSpdYSpnr, rd, true,
                rangeUtil.RValIsHigher);

        /*
         * Peak Wind
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label peakWindLbl = new Label(hfwwGroup, SWT.RIGHT);
        peakWindLbl.setText("Peak Wind (kt):");
        peakWindLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_HfwwMax);

        hfwwPeakWindRSpnr = new Spinner(hfwwGroup, SWT.BORDER);
        hfwwPeakWindYSpnr = new Spinner(hfwwGroup, SWT.BORDER);

        createSpinnerControls(hfwwGroup, hfwwPeakWindRSpnr, hfwwPeakWindYSpnr, rd, true,
                rangeUtil.RValIsHigher);
    }

    @Override
    protected void updateControlsWithData()
    {
        /*
         * Small Craft Advisory
         */
        scaWindSpdRSpnr.setSelection((int)ssdpd.getScaWindSpeedR());
        scaWindSpdYSpnr.setSelection((int)ssdpd.getScaWindSpeedY());
        scaGustSpdRSpnr.setSelection((int)ssdpd.getScaGustSpeedR());
        scaGustSpdYSpnr.setSelection((int)ssdpd.getScaGustSpeedY());
        scaPeakWindRSpnr.setSelection((int)ssdpd.getScaPeakWindR());
        scaPeakWindYSpnr.setSelection((int)ssdpd.getScaPeakWindY());
        scaWaveHgtRSpnr.setSelection((int)ssdpd.getScaWaveHgtR());
        scaWaveHgtYSpnr.setSelection((int)ssdpd.getScaWaveHgtY());

        /*
         * Gale Warning
         */
        galeWindSpdRSpnr.setSelection((int)ssdpd.getGaleWindSpeedR());
        galeWindSpdYSpnr.setSelection((int)ssdpd.getGaleWindSpeedY());
        galeGustSpdRSpnr.setSelection((int)ssdpd.getGaleGustSpeedR());
        galeGustSpdYSpnr.setSelection((int)ssdpd.getGaleGustSpeedY());
        galePeakWindRSpnr.setSelection((int)ssdpd.getGalePeakWindR());
        galePeakWindYSpnr.setSelection((int)ssdpd.getGalePeakWindY());

        /*
         * Storm Warning
         */
        stormWindSpdRSpnr.setSelection((int)ssdpd.getStormWrnWindSpeedR());
        stormWindSpdYSpnr.setSelection((int)ssdpd.getStormWrnWindSpeedY());
        stormGustSpdRSpnr.setSelection((int)ssdpd.getStormWrnGustSpeedR());
        stormGustSpdYSpnr.setSelection((int)ssdpd.getStormWrnGustSpeedY());
        stormPeakWindRSpnr.setSelection((int)ssdpd.getStormWrnPeakWindR());
        stormPeakWindYSpnr.setSelection((int)ssdpd.getStormWrnPeakWindY());

        /*
         * HFWW
         */
        hfwwWindSpdRSpnr.setSelection((int)ssdpd.getHfwwWindSpeedR());
        hfwwWindSpdYSpnr.setSelection((int)ssdpd.getHfwwWindSpeedY());
        hfwwGustSpdRSpnr.setSelection((int)ssdpd.getHfwwGustSpeedR());
        hfwwGustSpdYSpnr.setSelection((int)ssdpd.getHfwwGustSpeedY());
        hfwwPeakWindRSpnr.setSelection((int)ssdpd.getHfwwPeakWindR());
        hfwwPeakWindYSpnr.setSelection((int)ssdpd.getHfwwPeakWindY());
    }

    @Override
    protected void updateData()
    {
        /*
         * Small Craft Advisory
         */
        ssdpd.setScaWindSpeedR(scaWindSpdRSpnr.getSelection());
        ssdpd.setScaWindSpeedY(scaWindSpdYSpnr.getSelection());
        ssdpd.setScaGustSpeedR(scaGustSpdRSpnr.getSelection());
        ssdpd.setScaGustSpeedY(scaGustSpdYSpnr.getSelection());
        ssdpd.setScaPeakWindR(scaPeakWindRSpnr.getSelection());
        ssdpd.setScaPeakWindY(scaPeakWindYSpnr.getSelection());
        ssdpd.setScaWaveHgtR(scaWaveHgtRSpnr.getSelection());
        ssdpd.setScaWaveHgtY(scaWaveHgtYSpnr.getSelection());

        /*
         * Gale Warning
         */
        ssdpd.setGaleWindSpeedR(galeWindSpdRSpnr.getSelection());
        ssdpd.setGaleWindSpeedY(galeWindSpdYSpnr.getSelection());
        ssdpd.setGaleGustSpeedR(galeGustSpdRSpnr.getSelection());
        ssdpd.setGaleGustSpeedY(galeGustSpdYSpnr.getSelection());
        ssdpd.setGalePeakWindR(galePeakWindRSpnr.getSelection());
        ssdpd.setGalePeakWindY(galePeakWindYSpnr.getSelection());

        /*
         * Storm Warning
         */
        ssdpd.setStormWrnWindSpeedR(stormWindSpdRSpnr.getSelection());
        ssdpd.setStormWrnWindSpeedY(stormWindSpdYSpnr.getSelection());
        ssdpd.setStormWrnGustSpeedR(stormGustSpdRSpnr.getSelection());
        ssdpd.setStormWrnGustSpeedY(stormGustSpdYSpnr.getSelection());
        ssdpd.setStormWrnPeakWindR(stormPeakWindRSpnr.getSelection());
        ssdpd.setStormWrnPeakWindY(stormPeakWindYSpnr.getSelection());

        /*
         * HFWW
         */
        ssdpd.setHfwwWindSpeedR(hfwwWindSpdRSpnr.getSelection());
        ssdpd.setHfwwWindSpeedY(hfwwWindSpdYSpnr.getSelection());
        ssdpd.setHfwwGustSpeedR(hfwwGustSpdRSpnr.getSelection());
        ssdpd.setHfwwGustSpeedY(hfwwGustSpdYSpnr.getSelection());
        ssdpd.setHfwwPeakWindR(hfwwPeakWindRSpnr.getSelection());
        ssdpd.setHfwwPeakWindY(hfwwPeakWindYSpnr.getSelection());
    }

    @Override
    protected void applyAction()
    {
        updateData();
        this.updateCB.updateThresholdData(ssdpd);
    }
}
