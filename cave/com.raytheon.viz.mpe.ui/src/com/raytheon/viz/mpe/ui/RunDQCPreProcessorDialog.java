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
package com.raytheon.viz.mpe.ui;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcRunConfiguration;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.mpe.DQCPreProcessorConfigLocalCache;
import com.raytheon.viz.mpe.DQCPreProcessorExecuteJob;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.DateTimeSpinner;

/**
 * Allows a user to configure and trigger an on-demand execution of the DQC
 * PreProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2018  7184       bkowal      Initial creation
 * Jul 20, 2018 7386       smanoj      DQC PreProcessor open with 
 *                                     NO Run areas selected.
 *
 * </pre>
 *
 * @author bkowal
 */

public class RunDQCPreProcessorDialog extends CaveSWTDialog {

    private DateTimeSpinner runDateTimeSpinner;

    private Spinner numDaysSpinner;

    private Button setZeroCheckButton;

    private Table runAreasTable;

    public RunDQCPreProcessorDialog(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL, CAVE.DO_NOT_BLOCK);
        setText("Run DQC PreProcessor");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        final Composite composite = new Composite(shell, SWT.NONE);
        composite.setLayout(gl);
        composite.setLayoutData(gd);

        final Group configGroup = new Group(composite, SWT.BORDER);
        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        configGroup.setText("Run Configuration");
        configGroup.setLayout(gl);
        configGroup.setLayoutData(gd);

        final Label runDateTimeLbl = new Label(configGroup, SWT.NONE);
        runDateTimeLbl.setText("Run Date/Time:");
        runDateTimeLbl.setLayoutData(
                new GridData(SWT.RIGHT, SWT.CENTER, false, false));
        runDateTimeSpinner = new DateTimeSpinner(configGroup,
                TimeUtil.newGmtCalendar(), 6);

        final Label numDaysLbl = new Label(configGroup, SWT.NONE);
        numDaysLbl.setText("Number of Days:");
        numDaysLbl.setLayoutData(
                new GridData(SWT.RIGHT, SWT.CENTER, false, false));
        numDaysSpinner = new Spinner(configGroup, SWT.BORDER);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        GC gc = new GC(numDaysSpinner);
        gd.widthHint = gc.textExtent(StringUtils.repeat("9", 2)).x;
        gc.dispose();
        numDaysSpinner.setLayoutData(gd);
        numDaysSpinner.setMinimum(1);
        numDaysSpinner.setMaximum(15);

        final Label setOutputZeroLbl = new Label(configGroup, SWT.NONE);
        setOutputZeroLbl.setText("Set Output Zero:");
        setOutputZeroLbl.setLayoutData(
                new GridData(SWT.RIGHT, SWT.CENTER, false, false));
        setZeroCheckButton = new Button(configGroup, SWT.CHECK);

        /*
         * Run Area Selection
         */
        final Composite runAreasComposite = new Composite(configGroup,
                SWT.NONE);
        gl = new GridLayout(1, false);
        gl.marginTop = 0;
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        runAreasComposite.setLayout(gl);
        runAreasComposite.setLayoutData(gd);

        final Label runAreasLbl = new Label(runAreasComposite, SWT.NONE);
        runAreasLbl.setText("Run Areas");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        runAreasLbl.setLayoutData(gd);

        runAreasTable = new Table(runAreasComposite,
                SWT.CHECK | SWT.BORDER | SWT.HIDE_SELECTION | SWT.V_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = runAreasTable.getItemHeight() * 5;
        runAreasTable.setLayoutData(gd);
        runAreasTable.setLinesVisible(true);

        /*
         * Execute & Cancel Buttons.
         */
        final Composite buttonComp = new Composite(composite, SWT.NONE);
        gl = new GridLayout(2, false);
        gd = new GridData(SWT.CENTER, SWT.FILL, true, true);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        final int minimumButtonWidth = composite.getDisplay().getDPI().x;
        final Button executeButton = new Button(buttonComp, SWT.PUSH);
        executeButton.setText("Execute");
        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        gd.widthHint = minimumButtonWidth;
        executeButton.setLayoutData(gd);
        executeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                execute();
            }
        });

        final Button cancelButton = new Button(buttonComp, SWT.PUSH);
        cancelButton.setText("Cancel");
        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        gd.widthHint = minimumButtonWidth;
        cancelButton.setLayoutData(gd);
        cancelButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });

        /*
         * Populate the dialog.
         */
        DQCPreProcessorConfigLocalCache configLocalCache = DQCPreProcessorConfigLocalCache
                .getInstance();
        numDaysSpinner.setSelection(configLocalCache.getNumDays());
        setZeroCheckButton.setSelection(configLocalCache.isSetOutputZero());
        final Map<String, Boolean> runAreas = configLocalCache.getRunAreas();
        if (!runAreas.isEmpty()) {
            for (Entry<String, Boolean> entry : runAreas.entrySet()) {
                TableItem ti = new TableItem(runAreasTable, SWT.NONE);
                final String areaName = entry.getKey();
                ti.setData(areaName);
                ti.setText(areaName);
            }
        }
    }

    private void execute() {
        /*
         * Gather the information provided in the dialog to build a run
         * configuration for the DQC PreProcessor and to update the local CAVE
         * session cache.
         */
        DQCPreProcRunConfiguration runConfig = new DQCPreProcRunConfiguration();
        runConfig.setNumDays(numDaysSpinner.getSelection());
        runConfig.setRunDate(runDateTimeSpinner.getSelection());
        if (runAreasTable.getItemCount() > 0) {
            final List<String> runAreas = new ArrayList<>(
                    runAreasTable.getItemCount());
            for (TableItem ti : runAreasTable.getItems()) {
                if (ti.getChecked()) {
                    runAreas.add((String) ti.getData());
                }
            }
            runConfig.setAreas(runAreas);
        }
        runConfig.setSetZero(setZeroCheckButton.getSelection());
        DQCPreProcessorConfigLocalCache.getInstance().updateCache(runConfig);

        /*
         * Prepare the Eclipse job that will trigger the execution of the DQC
         * PreProcessor in the background and await notification of a successful
         * completion.
         */
        new DQCPreProcessorExecuteJob(runConfig).schedule();

        close();
    }
}