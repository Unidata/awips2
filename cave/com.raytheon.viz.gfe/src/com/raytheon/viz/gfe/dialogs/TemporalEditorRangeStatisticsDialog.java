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
package com.raytheon.viz.gfe.dialogs;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.PreferenceConstants;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditor.StatisticsMode;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The temporal editor range statistics dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * Feb 14, 2008            Eric Babin    Initial Creation
 * Jun 04, 2009 #2159      Richard Peter Updated
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class TemporalEditorRangeStatisticsDialog extends CaveJFACEDialog {

    private Composite top;

    private Composite comp;

    private Scale minScale;

    private Scale maxScale;

    private Label minLabel;

    private Label maxLabel;

    private Label lab1, lab2;

    private StatisticsMode origMode;

    private StatisticsMode mode;

    private int origModeratedMin;

    private int moderatedMin;

    private int origModeratedMax;

    private int moderatedMax;

    private double origStdDevMin;

    private double stdDevMin;

    private double origStdDevMax;

    private double stdDevMax;

    public TemporalEditorRangeStatisticsDialog(Shell parent) {
        super(parent);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        origMode = StatisticsMode
                .valueOf(GFEPreference
                        .getPreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE));
        mode = origMode;
        origModeratedMin = GFEPreference
                .getIntPreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MIN);
        moderatedMin = origModeratedMin;
        origModeratedMax = GFEPreference
                .getIntPreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MAX);
        moderatedMax = origModeratedMax;
        origStdDevMin = GFEPreference
                .getDoublePreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MIN);
        stdDevMin = origStdDevMin;
        origStdDevMax = GFEPreference
                .getDoublePreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MAX);
        stdDevMax = origStdDevMax;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout mainLayout = new GridLayout(2, false);

        top.setLayout(mainLayout);

        initializeComponents();
        parent.layout();

        return top;
    }

    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case IDialogConstants.OK_ID:
        case IDialogConstants.PROCEED_ID:
            GFEPreference.setPreference(
                    PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE,
                    mode.toString());
            GFEPreference
                    .setPreference(
                            PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MIN,
                            moderatedMin);
            GFEPreference
                    .setPreference(
                            PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MAX,
                            moderatedMax);
            GFEPreference
                    .setPreference(
                            PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MIN,
                            stdDevMin);
            GFEPreference
                    .setPreference(
                            PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MAX,
                            stdDevMax);
            break;
        case IDialogConstants.CANCEL_ID:
            GFEPreference.setPreference(
                    PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE,
                    origMode.toString());
            GFEPreference
                    .setPreference(
                            PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MIN,
                            origModeratedMin);
            GFEPreference
                    .setPreference(
                            PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MAX,
                            origModeratedMax);
            GFEPreference
                    .setPreference(
                            PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MIN,
                            origStdDevMin);
            GFEPreference
                    .setPreference(
                            PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MAX,
                            origStdDevMax);
            break;
        }

        super.buttonPressed(buttonId);
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
                true);
        createButton(parent, IDialogConstants.PROCEED_ID, "Apply", false);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    private void initializeComponents() {
        // moderated is 0 - 50 Percent..
        // standard is 0 - 3.0 Std. Deviation
        GridData data = new GridData();
        data.horizontalSpan = 2;
        Button absoluteButton = new Button(top, SWT.RADIO);
        absoluteButton.setText("Absolute");
        absoluteButton.setLayoutData(data);
        absoluteButton.setSelection(StatisticsMode.ABSOLUTE.equals(mode));
        absoluteButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                mode = StatisticsMode.ABSOLUTE;
                updateLayout();
                getShell().setSize(320, 174);
            }
        });
        Button moderatedButton = new Button(top, SWT.RADIO);
        moderatedButton.setText("Moderated");
        moderatedButton.setSelection(StatisticsMode.MODERATED.equals(mode));
        moderatedButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                mode = StatisticsMode.MODERATED;
                updateLayout();
                getShell().setSize(320, 280);
            }
        });
        data = new GridData();
        data.horizontalSpan = 2;
        moderatedButton.setLayoutData(data);

        Button standardDeviation = new Button(top, SWT.RADIO);
        standardDeviation.setText("Standard Deviation");
        standardDeviation.setSelection(StatisticsMode.STANDARD_DEVIATION
                .equals(mode));
        standardDeviation.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                mode = StatisticsMode.STANDARD_DEVIATION;
                updateLayout();
                getShell().setSize(320, 280);
            }
        });
        data = new GridData();
        data.horizontalSpan = 2;
        standardDeviation.setLayoutData(data);

        comp = new Composite(top, SWT.NONE);
        GridLayout gLayout = new GridLayout(2, false);
        comp.setLayout(gLayout);
        data = new GridData();
        data.horizontalSpan = 2;
        comp.setLayoutData(data);

        lab1 = new Label(comp, SWT.NONE);
        data = new GridData();
        data.horizontalSpan = 2;
        lab1.setLayoutData(data);
        minLabel = new Label(comp, SWT.BORDER);
        data = new GridData(50, SWT.DEFAULT);
        minLabel.setLayoutData(data);
        minScale = new Scale(comp, SWT.HORIZONTAL);
        minScale.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent arg0) {
                updateLabels();
            }
        });
        data = new GridData(150, SWT.DEFAULT);
        minScale.setLayoutData(data);

        lab2 = new Label(comp, SWT.NONE);
        data = new GridData();
        data.horizontalSpan = 2;
        lab2.setLayoutData(data);
        maxLabel = new Label(comp, SWT.BORDER);
        data = new GridData(50, SWT.DEFAULT);
        maxLabel.setLayoutData(data);

        maxScale = new Scale(comp, SWT.HORIZONTAL);
        data = new GridData(150, SWT.DEFAULT);
        maxScale.setLayoutData(data);
        maxScale.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent arg0) {
                updateLabels();
            }
        });

        updateLayout();
    }

    private void updateLayout() {
        GridData layoutData = (GridData) comp.getLayoutData();
        switch (mode) {
        case ABSOLUTE:
            comp.setVisible(false);
            layoutData.exclude = true;
            break;
        case MODERATED:
            comp.setVisible(true);
            layoutData.exclude = false;
            minScale.setMinimum(0);
            minScale.setMaximum(50);
            minScale.setSelection(moderatedMin);
            maxScale.setMinimum(0);
            maxScale.setMaximum(50);
            maxScale.setSelection(moderatedMax);
            lab1.setText("Minimum Moderated Percent");
            lab2.setText("Maximum Moderated Percent");
            break;
        case STANDARD_DEVIATION:
            comp.setVisible(true);
            layoutData.exclude = false;
            minScale.setMinimum(0);
            minScale.setMaximum(30);
            minScale.setSelection((int) (stdDevMin * 10.0));
            maxScale.setMinimum(0);
            maxScale.setMaximum(30);
            maxScale.setSelection((int) (stdDevMax * 10.0));
            lab1.setText("Minimum Standard Deviation");
            lab2.setText("Maximum Standard Deviation");
            break;
        }

        updateLabels();
        top.layout();
    }

    private void updateLabels() {
        switch (mode) {
        case MODERATED:
            moderatedMin = minScale.getSelection();
            moderatedMax = maxScale.getSelection();
            minLabel.setText(moderatedMin + "");
            maxLabel.setText(moderatedMax + "");
            break;
        case STANDARD_DEVIATION:
            stdDevMin = (float) (minScale.getSelection() / 10.0);
            stdDevMax = (float) (maxScale.getSelection() / 10.0);
            minLabel.setText(String.format("%1.2f", stdDevMin));
            maxLabel.setText(String.format("%1.2f", stdDevMax));
            break;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Temporal Editor Range Statistics");
    }

}
