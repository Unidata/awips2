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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState.InterpMode;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Grid Interpolation Dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer        Description
 * ------------ ----------  --------------  --------------------------
 * Feb 26, 2008             Eric Babin      Initial Creation
 * Jun  4, 2008    #1161    Ron Anderson    Reworked
 * Oct 25, 2012 #1287       rferrel         Code clean for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class GridsInterpolateDialog extends CaveJFACEDialog {
    private final int MAX_INTERVAL = 24;

    private Composite top;

    private Map<Button, InterpMode> modeMap;

    private Scale intervalScale, durationScale;

    private Label durationLabel, intervalLabel;

    private InterpMode interpMode;

    private int interval = 1;

    private int duration = 1;

    private int quantum;

    private boolean displayInterval;

    private boolean displayDuration;

    /**
     * Constructor taking parent shell
     * 
     * @param parent
     */
    public GridsInterpolateDialog(Shell parent) {
        super(parent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(1, false);
        top.setLayout(layout);

        computeTimeConstraints();

        getDefaults();

        initializeComponents();

        return top;
    }

    private void getDefaults() {
        PythonPreferenceStore pythonPreferenceStore = Activator.getDefault()
                .getPreferenceStore();
        interpMode = readInterpMode(pythonPreferenceStore);
        interval = readInterval(pythonPreferenceStore);
        duration = readDuration(pythonPreferenceStore);
    }

    private InterpMode readInterpMode(PythonPreferenceStore p) {
        InterpMode mode = InterpMode.GAPS;
        if (p.contains("InterpolateDialogMode")) {
            String modeStr = p.getString("InterpolateDialogMode");

            try {
                mode = InterpMode.valueOf(modeStr.toUpperCase());
            } catch (IllegalArgumentException ex) {
                // mode = InterpMode.GAPS;
                // invalid value entered, no-op
            } catch (NullPointerException ex) {
                // mode = InterpMode.GAPS;
                // invalid value entered, no-op
            }
        }

        return mode;
    }

    private int readInterval(PythonPreferenceStore p) {
        int startint = quantum;
        if (p.contains("InterpolateDefaultInterval")) {
            int intv = p.getInt("InterpolateDefaultInterval");
            if (intv > 0) {
                startint = intv;
            }
        }
        return startint;
    }

    private int readDuration(PythonPreferenceStore p) {
        int startdur = quantum;
        if (p.contains("InterpolateDefaultDuration")) {
            int durv = p.getInt("InterpolateDefaultDuration");
            if (durv > 0 && durv <= interval) {
                startdur = durv;
            }
        }
        return startdur;
    }

    private void initializeComponents() {

        if (interpMode == null) {
            interpMode = InterpMode.values()[0];
        }
        modeMap = new HashMap<Button, InterpMode>(InterpMode.values().length);
        for (InterpMode mode : InterpMode.values()) {
            Button button = new Button(top, SWT.RADIO);
            button.setText(mode.getDisplayString());
            button.setSelection(mode.equals(interpMode));
            modeMap.put(button, mode);

            button.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent arg0) {
                    interpMode = modeMap.get(arg0.getSource());
                }
            });
        }

        if (displayInterval) {
            Composite intervalComp = new Composite(top, SWT.NONE);
            GridLayout intervalLayout = new GridLayout(2, false);
            intervalComp.setLayout(intervalLayout);

            GridData data = new GridData();
            data.horizontalSpan = 2;
            Label lab = new Label(intervalComp, SWT.NONE);
            lab.setText("Interpolation Interval In Hours");
            lab.setLayoutData(data);

            data = new GridData(200, SWT.DEFAULT);
            intervalScale = new Scale(intervalComp, SWT.HORIZONTAL);
            intervalScale.setLayoutData(data);

            intervalScale.setMinimum(quantum);
            intervalScale.setMaximum(MAX_INTERVAL);
            intervalScale.setSelection(interval);
            intervalScale.setIncrement(quantum);
            intervalScale.setPageIncrement(quantum);
            // since SWT won't allow us to set a Scale's minimum equal to the
            // maximum, we're going to disable the control in the case quantum
            // >= MAX_INTERVAL. This will mimic AWIPS1's behavior in this edge
            // case.
            if (quantum >= MAX_INTERVAL) {
                intervalScale.setEnabled(false);
            }

            intervalScale.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent arg0) {
                }

                public void widgetSelected(SelectionEvent arg0) {
                    interval = intervalScale.getSelection();
                    intervalScale.setSelection(interval);
                    intervalLabel.setText(Integer.toString(interval));
                    if (durationScale != null) {
                        if (duration > interval) {
                            duration = interval;
                            durationScale.setSelection(duration);
                            durationLabel.setText(Integer.toString(duration));
                        }
                    }
                }
            });
            data = new GridData(50, SWT.DEFAULT);
            intervalLabel = new Label(intervalComp, SWT.NONE);
            intervalLabel.setLayoutData(data);
            intervalLabel
                    .setText(Integer.toString(intervalScale.getSelection()));
        }

        if (displayDuration) {
            Composite durationComp = new Composite(top, SWT.NONE);
            GridLayout durationLayout = new GridLayout(2, false);
            durationComp.setLayout(durationLayout);

            GridData data = new GridData();
            data.horizontalSpan = 2;
            Label lab2 = new Label(durationComp, SWT.NONE);
            lab2.setText("Duration of Grids In Hours");
            lab2.setLayoutData(data);

            data = new GridData(200, SWT.DEFAULT);
            durationScale = new Scale(durationComp, SWT.HORIZONTAL);
            durationScale.setLayoutData(data);
            durationScale.setMinimum(quantum);
            durationScale.setMaximum(MAX_INTERVAL);
            durationScale.setSelection(duration);
            durationScale.setIncrement(quantum);
            durationScale.setPageIncrement(quantum);
            // since SWT won't allow us to set a Scale's minimum equal to the
            // maximum, we're going to disable the control in the case quantum
            // >= MAX_INTERVAL. This will mimic AWIPS1's behavior in this edge
            // case.
            if (quantum >= MAX_INTERVAL) {
                durationScale.setEnabled(false);
            }

            durationScale.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent arg0) {
                }

                public void widgetSelected(SelectionEvent arg0) {
                    // duration is limited by max of interval
                    duration = durationScale.getSelection();
                    if (duration > interval) {
                        duration = interval;
                        durationScale.setSelection(duration);
                    }
                    durationLabel.setText(Integer.toString(duration));
                }
            });
            data = new GridData(50, SWT.DEFAULT);
            durationLabel = new Label(durationComp, SWT.NONE);
            durationLabel.setLayoutData(data);
            durationLabel
                    .setText(Integer.toString(durationScale.getSelection()));
        }
    }

    private void computeTimeConstraints() {
        Parm[] parms = DataManager.getCurrentInstance().getParmManager()
                .getSelectedParms();

        int minRepeatInterval = 24 * 3600;
        displayInterval = true;
        displayDuration = true;
        int compositeRepeat = 0;
        int compositeDuration = 0;

        for (Parm parm : parms) {
            TimeConstraints tc = parm.getGridInfo().getTimeConstraints();
            if (tc.getRepeatInterval() != tc.getDuration()) {
                displayDuration = false;
            }
            int repeatInterval = tc.getRepeatInterval();
            if (repeatInterval < minRepeatInterval) {
                minRepeatInterval = repeatInterval;
            }
            if (compositeRepeat == 0) {
                compositeRepeat = repeatInterval;
                compositeDuration = tc.getDuration();
            }
            if (compositeRepeat != repeatInterval) {
                displayInterval = false;
            }
            if (compositeDuration != tc.getDuration()) {
                displayDuration = false;
            }
        }
        quantum = minRepeatInterval / 3600;
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

        shell.setText("Interpolation Dialog");
    }

    /**
     * Gets the user selected interval
     * 
     * @return interval in hours
     */
    public int getInterval() {
        return interval;
    }

    /**
     * Gets the user selected duration
     * 
     * @return duration in hours
     */
    public int getDuration() {
        return duration;
    }

    /**
     * Gets the user selected interpolation mode
     * 
     * @return interpolation mode
     */
    public InterpMode getInterpMode() {
        return interpMode;
    }

}
