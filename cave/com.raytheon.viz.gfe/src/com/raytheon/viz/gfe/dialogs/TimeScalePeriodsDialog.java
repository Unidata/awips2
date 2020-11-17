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
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.PreferenceConstants;
import com.raytheon.viz.gfe.core.ISelectTimeRangeManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Time Scale Periods Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 23, 2009           randerso  Initial creation
 * Oct 29, 2012  1287     rferrel   Code clean up for non-blocking dialog.
 * Jan 25, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class TimeScalePeriodsDialog extends CaveJFACEDialog {

    private ISelectTimeRangeManager selectTRmgr;

    private Button[] buttonList;

    /**
     * Constructor
     *
     * @param parent
     * @param selectTRmgr
     */
    public TimeScalePeriodsDialog(Shell parent,
            ISelectTimeRangeManager selectTRmgr) {
        super(parent);

        this.setShellStyle(SWT.DIALOG_TRIM);
        this.selectTRmgr = selectTRmgr;
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Time Scale Displayed Periods");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);

        Group group = new Group(comp, SWT.SHADOW_ETCHED_IN);
        GridLayout layout = new GridLayout(1, true);
        layout.verticalSpacing = 0;
        group.setLayout(layout);
        group.setText("Available Periods");

        String[] displayed = GFEPreference.getStringArray(
                PreferenceConstants.GFE_TIME_SCALE_DISPLAY_PERIODS);
        Arrays.sort(displayed);

        buttonList = new Button[selectTRmgr.inventory().length];
        int i = 0;
        for (String name : selectTRmgr.inventory()) {

            Button button = new Button(group, SWT.CHECK);
            button.setText(name);
            if (Arrays.binarySearch(displayed, name) >= 0) {
                button.setSelection(true);
            }

            buttonList[i] = button;
            i++;
        }
        return comp;
    }

    @Override
    protected void okPressed() {
        List<String> displayed = new ArrayList<>();
        for (Button button : buttonList) {
            if (button.getSelection()) {
                displayed.add(button.getText());
            }
        }

        GFEPreference.setPreference(
                PreferenceConstants.GFE_TIME_SCALE_DISPLAY_PERIODS,
                displayed.toArray(new String[displayed.size()]));

        super.okPressed();
    }

}
