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
import java.util.Collections;
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.UIFormat;
import com.raytheon.viz.gfe.core.UIFormat.FilterType;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.ui.HazardUIUtils;

/**
 * Save Parameters on Exit dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2011            randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SaveParameterDialog extends AbstractSaveParameterDialog implements
        DisposeListener {

    public SaveParameterDialog(Shell parentShell, DataManager dataManager) {
        super(parentShell, dataManager);
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
        newShell.setText("Save Weather Element(s)");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        // create the three buttons associated with this dialog

        // set the state of the yes button depending on the hazard situation
        createButton(parent, IDialogConstants.OK_ID, "Yes", true);
        if (this.hazardsModified && this.tempHazGrids) {
            getButton(IDialogConstants.OK_ID).setEnabled(false);
        }
        createButton(parent, IDialogConstants.CLIENT_ID, "No", false);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void initializeComponents() {
        // Make a list of weather elements in alphabetical order.
        List<String> parmList = new ArrayList<String>(modparms.size());

        // Make a UIFormat to obtain the uniform ui format
        UIFormat uiFormat = new UIFormat(this.dataManager.getParmManager(),
                FilterType.MODIFIED, FilterType.MODIFIED);
        for (Parm parm : this.modparms) {
            parmList.add(uiFormat.uiParmID(parm.getParmID()));
        }

        Collections.sort(parmList);

        // Compose the dialog text.
        Label l = new Label(this.master, SWT.LEFT);
        l.setText("The following Weather Elements have been modified:\n");
        StringBuilder dialogText = new StringBuilder();
        for (String parm : parmList) {
            dialogText.append(' ').append(parm).append('\n');
        }

        Label info = new Label(this.master, SWT.LEFT);
        info.setFont(font);
        info.setText(dialogText.toString());

        l = new Label(this.master, SWT.LEFT);
        l.setText("Do you want to save them?");

        // add a warning dialog about unsaved hazard grids if necessary
        if (this.hazardsModified && this.tempHazGrids) {
            HazardUIUtils.displayWarningDialog(this.master);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case IDialogConstants.OK_ID: // Yes
            saveParms(this.modparms);
            break;
        case IDialogConstants.CLIENT_ID: // No
            exitWithoutSaving();
            break;
        case IDialogConstants.CANCEL_ID:
            super.cancelPressed();
            break;
        }
    }

    private void exitWithoutSaving() {
        for (Parm parm : modparms) {
            parm.looseLocks();
        }
        super.okPressed();
    }

}
