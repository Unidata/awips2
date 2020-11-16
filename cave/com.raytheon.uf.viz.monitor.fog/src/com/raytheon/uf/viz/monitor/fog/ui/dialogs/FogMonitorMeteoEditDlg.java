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
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.fog.threshold.FogMonitorMeteoData;
import com.raytheon.uf.viz.monitor.ui.dialogs.EditThresholdsDlg;

/**
 * Fog monitor meteo edit dialog
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
public class FogMonitorMeteoEditDlg extends EditThresholdsDlg
{
    private Text visRTF;
    private Button visIncR;
    private Button visDecR;
    private Text visYTF;
    private Button visIncY;
    private Button visDecY;

    private FogMonitorMeteoData fmmd;
    private IUpdateMonitorMeteo updateCB;

    public FogMonitorMeteoEditDlg(Shell parent, FogMonitorMeteoData fmmd, IUpdateMonitorMeteo updateCB)
    {
        super(parent);

        this.fmmd = fmmd;
        this.updateCB = updateCB;
    }

    /**
     * Initialize the components on the display.
     */
    @Override
    protected void initializeComponents()
    {
        shell.setText("FOG: Display Edit Meteo");

        rangeUtil = RangesUtil.getInstance();

        createControls();
        addSeparator(getDialogShell());
    }

    private void createControls()
    {
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
    }

    @Override
    protected void updateControlsWithData()
    {
        double val = Double.NaN;

        /*
         * Visibility
         */
        val = fmmd.getMeteoVisR();
        this.visRIndex = rangeUtil.getIndexOfValue((int)val);
        this.visRTF.setText(rangeUtil.getVisString((int)val));

        val = fmmd.getMeteoVisY();
        this.visYIndex = rangeUtil.getIndexOfValue((int)val);
        this.visYTF.setText(rangeUtil.getVisString((int)val));
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
        fmmd.setMeteoVisR(val);

        val = rangeUtil.getVisValueAtIndex(this.visYIndex);
        fmmd.setMeteoVisY(val);
    }

    @Override
    protected void applyAction()
    {
        updateData();

        this.updateCB.updateThresholdData(fmmd);
    }
}
