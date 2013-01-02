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
package com.raytheon.viz.mpe.ui.dialogs.hourlyradar;

import java.util.ArrayList;
import java.util.Date;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.dataplugin.shef.tables.Rwradarresult;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;
import com.raytheon.viz.mpe.MPEDateFormatter;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Edit Bias Value dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009 2675       mpduff     Initial creation
 * Aug 13, 2009 2675       mpduff     TIM changes added
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class EditBiasDlg extends CaveSWTDialog {

    /**
     * Normal font.
     */
    private Font font = null;

    /**
     * The precip value spinner control.
     */
    private Spinner biasSpinner = null;

    /**
     * The precip slider control.
     */
    private Scale biasSlider = null;

    /**
     * The bias used value.
     */
    private String biasUsed = null;

    /**
     * The Radar ID.
     */
    private String radId = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            The parent shell for this dialog.
     */
    public EditBiasDlg(Shell parentShell, String radId, String biasUsed) {
        super(parentShell);
        setText("Edit Bias Value");

        this.biasUsed = biasUsed;
        this.radId = radId;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(new Double(-999.0));
        font = new Font(shell.getDisplay(), "Monospace", 11, SWT.NORMAL);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the gui widgets.
     */
    private void initializeComponents() {
        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, true);
        comp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false, 1, 1);
        comp.setLayoutData(gd);

        // Top Label
        Label selectLbl = new Label(comp, SWT.NONE);
        selectLbl.setText("Bias Value Used = " + biasUsed);

        getSliderComp(comp);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false, 2, 1);
        Label sepLbl = new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL);
        gd.widthHint = 265;
        sepLbl.setLayoutData(gd);

        createButtons(comp);
    }

    /**
     * Build the slider/spinner composite.
     * 
     * @param comp
     *            The parent composite
     */
    private void getSliderComp(Composite parentComp) {
        Composite comp = new Composite(parentComp, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(200, 30);
        biasSlider = new Scale(comp, SWT.HORIZONTAL);
        biasSlider.setMinimum(0);
        biasSlider.setMaximum(500);
        biasSlider.setIncrement(1);
        biasSlider.setLayoutData(gd);
        biasSlider.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                biasSpinner.setSelection(biasSlider.getSelection());
            }
        });

        // Create the Red color spinner.
        biasSpinner = new Spinner(comp, SWT.BORDER);
        gd = new GridData(30, SWT.DEFAULT);
        biasSpinner.setLayoutData(gd);
        biasSpinner.setMinimum(0);
        biasSpinner.setMaximum(500);
        biasSpinner.setSelection(biasSlider.getSelection());
        biasSpinner.setDigits(2);

        biasSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                biasSlider.setSelection(biasSpinner.getSelection());
            }
        });

        int value = (int) (Double.parseDouble(biasUsed) * 100);
        biasSlider.setSelection(value);
        biasSpinner.setSelection(value);
    }

    /**
     * Create the open and close buttons.
     */
    private void createButtons(Composite comp) {
        Composite buttonComp = new Composite(comp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false, 2, 1);
        buttonComp.setLayoutData(gd);

        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        gd = new GridData(75, SWT.DEFAULT);
        okBtn.setAlignment(SWT.CENTER);
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                applyBiasUpdate();
                shell.dispose();
            }
        });

        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        gd = new GridData(75, SWT.DEFAULT);
        cancelBtn.setAlignment(SWT.CENTER);
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Update the bias table with the new value.
     */
    private void applyBiasUpdate() {
        String where = "";
        float memspan = -99.0f;
        Date dt3 = MPEDisplayManager.getCurrent().getCurrentEditDate();
        String date = MPEDateFormatter.format_yyyyMMddHHmmss(dt3);

        ArrayList<Rwradarresult> rwr = new ArrayList<Rwradarresult>();
        Rwradarresult rwrr = new Rwradarresult();
        where = String.format("WHERE radid='%s' AND obstime='%s'", radId, date);
        rwr = (ArrayList<Rwradarresult>) IHFSDbGenerated
                .GetRWRadarResult(where);
        if (rwr.size() != 0) {
            rwrr = rwr.get(0);
        }
        rwrr.setEditBias("y");
        rwrr.setMemSpanUsed((double) memspan);
        System.out.println("Bias slider value = "
                + (double) biasSlider.getSelection() / 100);
        rwrr.setRwBiasValUsed((double) biasSlider.getSelection() / 100);
        IHFSDbGenerated.UpdateRWRadarResult(rwrr);

        MPEDataManager.getInstance().setRadarEditFlag(true);
        setReturnValue(new Double((double) biasSlider.getSelection() / 100));
    }
}
