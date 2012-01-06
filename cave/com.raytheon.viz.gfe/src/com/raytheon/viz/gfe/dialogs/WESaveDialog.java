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
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.UIFormat;
import com.raytheon.viz.gfe.core.UIFormat.FilterType;
import com.raytheon.viz.gfe.core.msgs.ISCSendStatusChangedMsg;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.ui.HazardUIUtils;

/**
 * The save forecast dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2011            randerso    Complete rework
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class WESaveDialog extends AbstractSaveParameterDialog implements
        DisposeListener {

    private static Boolean IscState;

    private List<Button> buttons;

    public WESaveDialog(Shell parentShell, DataManager dataManager) {
        super(parentShell, dataManager);

        // remove Hazards from the modified list
        // if there are temporary haz parms
        if (this.hazardsModified && this.tempHazGrids) {
            this.modparms.remove(this.hazParm);
        }
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        shell.setText("Save Forecast");
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        if (modparms.size() > 0) {
            createButton(parent, IDialogConstants.OK_ID, "Save Forecast", true);
        }
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void initializeComponents() {
        GridLayout layout = (GridLayout) master.getLayout();
        layout.verticalSpacing = 0;

        String t;
        if (this.modparms.size() > 0) {
            t = "Select the weather elements:";
        } else {
            t = "No modified weather elements can be saved ";
        }
        Label label = new Label(master, SWT.BORDER | SWT.CENTER);
        label.setText(t);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        label.setLayoutData(layoutData);

        if (this.modparms.size() > 0) {
            // add the ISC note
            this.addISCNote(master);

            // make the Weather Element part of the dialog
            this.makeWEBox();

            // put all of the frames together
            // this.weFrame.pack(side=Tkinter.TOP, expand=Tkinter.YES,
            // anchor=Tkinter.N, fill=Tkinter.BOTH)
        }

        // display warning dialog about unsaved hazard grids in necessary
        if (this.hazardsModified && this.tempHazGrids) {
            HazardUIUtils.displayWarningDialog(this.master);
        }
    }

    private void addISCNote(Composite master) {
        // get new state
        ISCSendStatusChangedMsg msg = Message
                .inquireLastMessage(ISCSendStatusChangedMsg.class);
        boolean newState = msg.isEnabled();
        if ((IscState != null && IscState.booleanValue() == newState)
                || !this.dataManager.requestISC()) {
            return; // no message needed
        }
        IscState = newState; // save new state
        String t;
        if (newState) {
            // enabled
            if (this.dataManager.sendIscOnSave()) {
                t = "ISC Send is now ENABLED. These saved grids will be \n"
                        + "automatically sent via ISC.";
            } else {
                t = "ISC Send is now ENABLED.  These grids will be 'remembered'\n"
                        + "and can be later sent via ISC using the \n"
                        + "Consistency->Send Intersite Grids dialog.";
            }
        } else {
            // disabled
            if (this.dataManager.sendIscOnSave()) {
                t = "ISC Send is now DISABLED. These saved grids will not be\n"
                        + "sent via ISC after saving.";
            } else {
                t = "ISC Send is now DISABLED. These grids will not be 'remembered'\n"
                        + "and won't be sent by the Auto option on the \n"
                        + "Consistency->Send Intersite Grids dialog.";
            }
            t = t + " If desired, use the\n"
                    + "Consistency->ISC Send Enable menu before saving grids.";
        }

        Label notice = new Label(master, SWT.BORDER);
        notice.setText("Notice: " + t);
        notice.setBackground(notice.getDisplay()
                .getSystemColor(SWT.COLOR_WHITE));
        notice.setForeground(notice.getDisplay().getSystemColor(SWT.COLOR_RED));
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false,
                1, 1);
        notice.setLayoutData(layoutData);
    }

    @Override
    protected void okPressed() {
        List<Parm> parmsToSave = new ArrayList<Parm>();
        for (Button button : this.buttons) {
            if (button.getSelection()) {
                parmsToSave.add((Parm) button.getData());
            }
        }

        saveParms(parmsToSave);
    }

    private void makeWEBox() {
        // make the frame for the Weather Element
        Group weFrame = new Group(this.master, SWT.BORDER);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        weFrame.setLayoutData(layoutData);
        GridLayout layout = new GridLayout(1, false);
        layout.verticalSpacing = 0;
        weFrame.setLayout(layout);

        // Create a UIFormat in order to obtain a uniform ui format
        UIFormat uiFormat = new UIFormat(this.dataManager.getParmManager(),
                FilterType.MODIFIED, FilterType.MODIFIED);

        this.buttons = new ArrayList<Button>();
        for (Parm parm : modparms) {
            Button button = new Button(weFrame, SWT.CHECK);
            button.setFont(font);
            button.setText(uiFormat.uiParmID(parm.getParmID()));
            button.setSelection(true);
            button.setData(parm);
            layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            button.setLayoutData(layoutData);
            this.buttons.add(button);
        }

    }

}
