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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.UIFormat;
import com.raytheon.viz.gfe.core.UIFormat.FilterType;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * The hidden weather element dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Feb 22, 2008           Eric Babin  Initial Creation
 *  Dec 02, 2009     #945  randerso    reworked
 *  Oct 25, 2012 #1287     rferrel     Code clean up for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class HiddenWeatherElementDialog extends CaveJFACEDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveDeleteSelectTRDialog.class);

    private final int UNLOAD = IDialogConstants.CLIENT_ID + 1;

    private final int MAKE_VISIBLE = IDialogConstants.CLIENT_ID + 2;

    private Composite top;

    private DataManager dataManager;

    private ToggleSelectList lbox;

    private ParmID[] parmIDs;

    private String[] parmNames;

    public HiddenWeatherElementDialog(Shell parent, DataManager dataManager) {
        super(parent);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);

        this.dataManager = dataManager;

        // make a list of weather elements in alpha order
        Parm[] parms = dataManager.getParmManager().getUndisplayedParms();
        parmIDs = new ParmID[parms.length];
        parmNames = new String[parms.length];
        UIFormat uiFormat = new UIFormat(dataManager.getParmManager(),
                FilterType.UNDISPLAYED, FilterType.UNDISPLAYED);
        for (int i = 0; i < parms.length; i++) {
            parmIDs[i] = parms[i].getParmID();
        }
        Arrays.sort(parmIDs);
        for (int i = 0; i < parmIDs.length; i++) {
            parmNames[i] = uiFormat.uiParmID(parmIDs[i]);
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

        shell.setText("Manage Hidden Weather Elements");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout();
        top.setLayout(layout);

        initializeComponents();

        return top;
    }

    private void initializeComponents() {

        Label label = new Label(top, SWT.NONE);
        label.setText("Weather Element Name(s)");
        GridData layoutData = new GridData(SWT.CENTER, SWT.DEFAULT, false,
                false);
        label.setLayoutData(layoutData);

        layoutData = new GridData(GridData.FILL_BOTH);
        lbox = new ToggleSelectList(top, SWT.BORDER | SWT.MULTI);
        lbox.setLayoutData(layoutData);
        lbox.setItems(parmNames);
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, UNLOAD, "Unload", false);
        super.createButton(parent, MAKE_VISIBLE, "Make Visible", false);
        super.createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case IDialogConstants.CANCEL_ID:
            cancelPressed();
            break;
        case UNLOAD:
            unload();
            okPressed();
            break;
        case MAKE_VISIBLE:
            makeVisible();
            okPressed();
            break;
        }
    }

    protected void unload() {
        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            StringBuilder sb = new StringBuilder("UnloadHidden: ");
            for (String s : this.lbox.getSelection()) {
                sb.append(s).append("\n");
            }
            statusHandler.debug(sb.toString());
        }
        ParmID[] unload = parmIDsFromUINames(this.lbox.getSelection());
        Parm[] unloadParms = dataManager.getParmManager().getParms(unload);
        dataManager.getParmManager().deleteParm(unloadParms);
    }

    protected void makeVisible() {
        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            StringBuilder sb = new StringBuilder("MakeVisibleHidden: ");
            for (String s : this.lbox.getSelection()) {
                sb.append(s).append("\n");
            }
            statusHandler.debug(sb.toString());
        }
        List<ParmID> visParmIDs = new ArrayList<ParmID>(
                Arrays.asList(dataManager.getParmManager().getParmIDs(
                        dataManager.getParmManager().getDisplayedParms())));
        ParmID[] load = parmIDsFromUINames(this.lbox.getSelection());
        for (int i = 0; i < load.length; i++) {
            visParmIDs.add(load[i]);
        }
        dataManager.getParmManager().setDisplayedParms(
                visParmIDs.toArray(new ParmID[visParmIDs.size()]));
    }

    protected ParmID[] parmIDsFromUINames(String[] uinames) {
        ParmID[] ret = new ParmID[uinames.length];
        for (int i = 0; i < uinames.length; i++) {
            int idx = Arrays.asList(parmNames).indexOf(uinames[i]);
            ret[i] = parmIDs[idx]; // parallel array
        }
        return ret;
    }
}
