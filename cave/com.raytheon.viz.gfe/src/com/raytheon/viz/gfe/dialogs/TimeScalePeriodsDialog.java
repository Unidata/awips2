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

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.ISelectTimeRangeManager;
import com.raytheon.viz.gfe.preferences.TimeScaleDisplayedPeriodsPreference;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2009            randerso     Initial creation
 * Oct 29, 2012 1287       rferrel     Code clean up for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TimeScalePeriodsDialog extends CaveJFACEDialog {

    private ISelectTimeRangeManager selectTRmgr;

    private Button[] buttonList;

    /**
     * @param parentShell
     */
    public TimeScalePeriodsDialog(Shell parent,
            ISelectTimeRangeManager selectTRmgr) {
        super(parent);

        this.setShellStyle(SWT.DIALOG_TRIM);
        this.selectTRmgr = selectTRmgr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Time Scale Displayed Periods");
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
        Composite comp = (Composite) super.createDialogArea(parent);

        Group group = new Group(comp, SWT.SHADOW_ETCHED_IN);
        GridLayout layout = new GridLayout(1, true);
        layout.verticalSpacing = 0;
        group.setLayout(layout);
        group.setText("Available Periods");

        String[] displayed = TimeScaleDisplayedPeriodsPreference
                .getTimeScaleDisplayedPeriods();
        Arrays.sort(displayed);

        buttonList = new Button[selectTRmgr.inventory().length];
        int i = 0;
        for (String name : selectTRmgr.inventory()) {

            Button button = new Button(group, SWT.CHECK);
            button.setText(name);
            if (Arrays.binarySearch(displayed, name) >= 0) {
                button.setSelection(true);
            }

            buttonList[i++] = button;
        }
        return comp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @Override
    protected void okPressed() {
        ArrayList<String> displayed = new ArrayList<String>();
        for (Button button : buttonList) {
            if (button.getSelection()) {
                displayed.add(button.getText());
            }
        }

        TimeScaleDisplayedPeriodsPreference
                .setTimeScaleDisplayedPeriods(displayed
                        .toArray(new String[displayed.size()]));

        super.okPressed();
    }

}
