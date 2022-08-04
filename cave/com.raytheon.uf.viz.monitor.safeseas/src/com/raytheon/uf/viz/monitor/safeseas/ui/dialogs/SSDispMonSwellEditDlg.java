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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDispMonSwellData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

/**
 * Safeseas Display Monitor Swell Edit Dialog
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
public class SSDispMonSwellEditDlg extends EditThresholdsDlg
{
    private Spinner psHeightRSpnr;
    private Spinner psHeightYSpnr;

    private Spinner psPeriodRSpnr;
    private Spinner psPeriodYSpnr;

    private Spinner psDirFromRSpnr;
    private Spinner psDirFromYSpnr;

    private Spinner psDirToRSpnr;
    private Spinner psDirToYSpnr;

    private Spinner ssHeightRSpnr;
    private Spinner ssHeightYSpnr;

    private Spinner ssPeriodRSpnr;
    private Spinner ssPeriodYSpnr;

    private Spinner ssDirFromRSpnr;
    private Spinner ssDirFromYSpnr;

    private Spinner ssDirToRSpnr;
    private Spinner ssDirToYSpnr;

    private boolean displayFlag = true;

    private SSDispMonSwellData sssd;
    private IUpdateDisplayMonitorSwell updateCB;

    public SSDispMonSwellEditDlg(Shell parent, SSDispMonSwellData sssd,
            IUpdateDisplayMonitorSwell updateCB, boolean displayFlag)
    {
        super(parent);

        this.displayFlag = displayFlag;

        this.sssd = sssd;
        this.updateCB = updateCB;
    }

    /**
     * Initialize the components on the display.
     */
    @Override
    protected void initializeComponents()
    {
        if (displayFlag == true)
        {
            shell.setText("SAFESEAS: Display Edit Swell");
        }
        else
        {
            shell.setText("SAFESEAS: Monitor Edit Swell");
        }


        rangeUtil = RangesUtil.getInstance();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 10;
        Group priSwGroup = new Group(getDialogShell(), SWT.NONE);
        priSwGroup.setLayout(gl);
        priSwGroup.setLayoutData(gd);
        priSwGroup.setText(" Primary Swell ");

        createPrimarySwellGroup(priSwGroup);
        createPrimarySwellDirGroup(priSwGroup);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        Group secSwGroup = new Group(getDialogShell(), SWT.NONE);
        gl.verticalSpacing = 10;
        secSwGroup.setLayout(gl);
        secSwGroup.setLayoutData(gd);
        secSwGroup.setText(" Secondary Swell ");

        createSecondarySwellGroup(secSwGroup);
        createSecondarySwellDirGroup(secSwGroup);
    }

    private void createPrimarySwellGroup(Group priSwGroup)
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Composite priSwComp = new Composite(priSwGroup, SWT.NONE);
        priSwComp.setLayout(gl);
        priSwComp.setLayoutData(gd);

        createRYRangeLabels(priSwComp);

        /*
         * Height (ft)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label heightLbl = new Label(priSwComp, SWT.RIGHT);
        heightLbl.setText("Height (ft):");
        heightLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_PSwellHgt);

        psHeightRSpnr = new Spinner(priSwComp, SWT.BORDER);
        psHeightYSpnr = new Spinner(priSwComp, SWT.BORDER);

        createSpinnerControls(priSwComp, psHeightRSpnr, psHeightYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Period(s)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label periodLbl = new Label(priSwComp, SWT.RIGHT);
        periodLbl.setText("Period(s):");
        periodLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_PSwellPer);

        psPeriodRSpnr = new Spinner(priSwComp, SWT.BORDER);
        psPeriodYSpnr = new Spinner(priSwComp, SWT.BORDER);

        if ( sssd.isRankSwellPeriodHigh() ) {
            createSpinnerControls(priSwComp, psPeriodRSpnr, psPeriodYSpnr, rd, true, rangeUtil.RValIsHigher);
        } else {
            createSpinnerControls(priSwComp, psPeriodRSpnr, psPeriodYSpnr, rd, true, rangeUtil.YValIsHigher);
        }
    }

    private void createPrimarySwellDirGroup(Group priSwGroup)
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(6, false);
        gl.verticalSpacing = 10;
        Composite priSwComp = new Composite(priSwGroup, SWT.NONE);
        priSwComp.setLayout(gl);
        priSwComp.setLayoutData(gd);

        createYRRYRangeLabels(priSwComp);

        /*
         * Direction
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label dirLbl = new Label(priSwComp, SWT.RIGHT);
        dirLbl.setText("Dir (deg):");
        dirLbl.setLayoutData(gd);

        psDirFromYSpnr = new Spinner(priSwComp, SWT.BORDER);
        psDirFromRSpnr = new Spinner(priSwComp, SWT.BORDER);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_PSwellDirT);

        psDirToRSpnr = new Spinner(priSwComp, SWT.BORDER);
        psDirToYSpnr = new Spinner(priSwComp, SWT.BORDER);

        createSpinnerControls(priSwComp, psDirFromYSpnr,psDirFromRSpnr,psDirToRSpnr, psDirToYSpnr, rd, false, rangeUtil.ryNotNeeded);
        addIncrementListeners(psDirFromRSpnr, psDirFromYSpnr);
        addIncrementListeners(psDirToRSpnr, psDirToYSpnr);
    }

    private void createSecondarySwellGroup(Group secSwGroup)
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Composite secSwComp = new Composite(secSwGroup, SWT.NONE);
        secSwComp.setLayout(gl);
        secSwComp.setLayoutData(gd);

        createRYRangeLabels(secSwComp);

        /*
         * Height (ft)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label heightLbl = new Label(secSwComp, SWT.RIGHT);
        heightLbl.setText("Height (ft):");
        heightLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_SSwellHgt);

        ssHeightRSpnr = new Spinner(secSwComp, SWT.BORDER);
        ssHeightYSpnr = new Spinner(secSwComp, SWT.BORDER);

        createSpinnerControls(secSwComp, ssHeightRSpnr, ssHeightYSpnr, rd, true, rangeUtil.RValIsHigher);

        /*
         * Period(s)
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label periodLbl = new Label(secSwComp, SWT.RIGHT);
        periodLbl.setText("Period(s):");
        periodLbl.setLayoutData(gd);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_SSwellPer);

        ssPeriodRSpnr = new Spinner(secSwComp, SWT.BORDER);
        ssPeriodYSpnr = new Spinner(secSwComp, SWT.BORDER);

        if ( sssd.isRankSwellPeriodHigh() ) {
            createSpinnerControls(secSwComp, ssPeriodRSpnr, ssPeriodYSpnr, rd, true, rangeUtil.RValIsHigher);
        } else {
            createSpinnerControls(secSwComp, ssPeriodRSpnr, ssPeriodYSpnr, rd, true, rangeUtil.YValIsHigher);
        }

    }
    private void createSecondarySwellDirGroup(Group secSwGroup)
    {
        RangeData rd;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(6, false);
        gl.verticalSpacing = 10;
        Composite secSwComp = new Composite(secSwGroup, SWT.NONE);
        secSwComp.setLayout(gl);
        secSwComp.setLayoutData(gd);

        createYRRYRangeLabels(secSwComp);

        /*
         * Direction
         */
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label dirLbl = new Label(secSwComp, SWT.RIGHT);
        dirLbl.setText("Dir (deg):");
        dirLbl.setLayoutData(gd);

        ssDirFromYSpnr = new Spinner(secSwComp, SWT.BORDER);
        ssDirFromRSpnr = new Spinner(secSwComp, SWT.BORDER);

        rd = rangeUtil.getRangeData(RangesUtil.RangeType.SS_SSwellDirT);

        ssDirToRSpnr = new Spinner(secSwComp, SWT.BORDER);
        ssDirToYSpnr = new Spinner(secSwComp, SWT.BORDER);

        createSpinnerControls(secSwComp, ssDirFromYSpnr, ssDirFromRSpnr, ssDirToRSpnr, ssDirToYSpnr, rd, false, rangeUtil.ryNotNeeded);
        addIncrementListeners(ssDirFromRSpnr, ssDirFromYSpnr);
        addIncrementListeners(ssDirToRSpnr, ssDirToYSpnr);
        System.out.println("kwz---debug inc="+ssDirFromYSpnr.getIncrement());
    }

    @Override
    protected void updateControlsWithData()
    {
        /*
         * Primary Swell
         */
        psHeightRSpnr.setSelection((int)sssd.getPriSwellHeightR());
        psHeightYSpnr.setSelection((int)sssd.getPriSwellHeightY());

        psPeriodRSpnr.setSelection((int)sssd.getPriSwellPeriodR());
        psPeriodYSpnr.setSelection((int)sssd.getPriSwellPeriodY());

        psDirFromRSpnr.setSelection((int)sssd.getPriSwellDirFromR());
        psDirFromYSpnr.setSelection((int)sssd.getPriSwellDirFromY());

        psDirToRSpnr.setSelection((int)sssd.getPriSwellDirToR());
        psDirToYSpnr.setSelection((int)sssd.getPriSwellDirToY());

        /*
         * Secondary Swell
         */
        ssHeightRSpnr.setSelection((int)sssd.getSecSwellHeightR());
        ssHeightYSpnr.setSelection((int)sssd.getSecSwellHeightY());

        ssPeriodRSpnr.setSelection((int)sssd.getSecSwellPeriodR());
        ssPeriodYSpnr.setSelection((int)sssd.getSecSwellPeriodY());

        ssDirFromRSpnr.setSelection((int)sssd.getSecSwellDirFromR());
        ssDirFromYSpnr.setSelection((int)sssd.getSecSwellDirFromY());

        ssDirToRSpnr.setSelection((int)sssd.getSecSwellDirToR());
        ssDirToYSpnr.setSelection((int)sssd.getSecSwellDirToY());

    }

    @Override
    protected void updateData()
    {
        /*
         * Primary Swell
         */
        sssd.setPriSwellHeightR(psHeightRSpnr.getSelection());
        sssd.setPriSwellHeightY(psHeightYSpnr.getSelection());

        sssd.setPriSwellPeriodR(psPeriodRSpnr.getSelection());
        sssd.setPriSwellPeriodY(psPeriodYSpnr.getSelection());

        sssd.setPriSwellDirFromR(psDirFromRSpnr.getSelection());
        sssd.setPriSwellDirFromY(psDirFromYSpnr.getSelection());

        sssd.setPriSwellDirToR(psDirToRSpnr.getSelection());
        sssd.setPriSwellDirToY(psDirToYSpnr.getSelection());

        /*
         * Secondary Swell
         */

        sssd.setSecSwellHeightR(ssHeightRSpnr.getSelection());
        sssd.setSecSwellHeightY(ssHeightYSpnr.getSelection());

        sssd.setSecSwellPeriodR(ssPeriodRSpnr.getSelection());
        sssd.setSecSwellPeriodY(ssPeriodYSpnr.getSelection());

        sssd.setSecSwellDirFromR(ssDirFromRSpnr.getSelection());
        sssd.setSecSwellDirFromY(ssDirFromYSpnr.getSelection());

        sssd.setSecSwellDirToR(ssDirToRSpnr.getSelection());
        sssd.setSecSwellDirToY(ssDirToYSpnr.getSelection());
    }

    @Override
    protected void applyAction()
    {
        updateData();
        this.updateCB.updateThresholdData(sssd);

    }

    @Override
    protected boolean determineError(){

        if (determinePSErrors()==true) {
            return true;
        } else {
            return determineSSErrors();
        }
    }

    private boolean determineSSErrors() {
        int wdRedFrom = this.ssDirFromRSpnr.getSelection();
        int wdRedTo = this.ssDirToRSpnr.getSelection();
        int wdYellowFrom = this.ssDirFromYSpnr.getSelection();
        int wdYellowTo = this.ssDirToYSpnr.getSelection();

        return determineError (wdYellowFrom,wdRedFrom,wdRedTo,wdYellowTo);
    }

    private boolean determinePSErrors() {
        int wdRedFrom = this.psDirFromRSpnr.getSelection();
        int wdRedTo = this.psDirToRSpnr.getSelection();
        int wdYellowFrom = this.psDirFromYSpnr.getSelection();
        int wdYellowTo = this.psDirToYSpnr.getSelection();

        return determineError (wdYellowFrom,wdRedFrom,wdRedTo,wdYellowTo);
    }
}
