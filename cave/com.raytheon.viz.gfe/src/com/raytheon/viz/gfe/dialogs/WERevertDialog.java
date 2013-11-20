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
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
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
import com.raytheon.viz.gfe.ui.HazardUIUtils;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The revert forecast dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2011            randerso    Initial creation
 * Oct 25, 2012 1287       rferrel     Code clean up for non-blocking dialog.
 * Nov 20, 2013 2488       randerso    Changed to use DejaVu font
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class WERevertDialog extends CaveJFACEDialog implements DisposeListener {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WERevertDialog.class);

    private Composite master;

    private DataManager dataManager;

    private Parm[] modparms;

    private boolean hazardsModified;

    protected Font font;

    private List<Button> buttons;

    public WERevertDialog(Shell parent, DataManager dataManager) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
        this.dataManager = dataManager;
        this.modparms = dataManager.getParmManager().getModifiedParms();

        this.hazardsModified = HazardUIUtils
                .hazardsWEModified(this.dataManager) != null;
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        shell.setText("Revert Forecast");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        master = (Composite) super.createDialogArea(parent);
        GridLayout layout = (GridLayout) master.getLayout();
        layout.verticalSpacing = 0;
        master.addDisposeListener(this);

        FontData fd = master.getDisplay().getSystemFont().getFontData()[0];
        // TODO not have hard coded font name
        fd.setName("DejaVu Sans Mono");
        font = new Font(master.getDisplay(), fd);

        String t;
        if (this.modparms.length > 0) {
            t = "Select the weather elements:";
        } else {
            t = "No modified weather elements     ";
        }

        Label label = new Label(master, SWT.BORDER | SWT.CENTER);
        label.setText(t);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        label.setLayoutData(layoutData);

        // Make a Checkbutton list for each modified parm
        Group frame = new Group(master, SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        frame.setLayoutData(layoutData);
        layout = new GridLayout();
        layout.verticalSpacing = 0;
        frame.setLayout(layout);
        this.buttons = new ArrayList<Button>();

        // Create a UIFormat in order to obtain a uniform ui format
        UIFormat uiFormat = new UIFormat(this.dataManager.getParmManager(),
                FilterType.MODIFIED, FilterType.MODIFIED);

        for (Parm parm : this.modparms) {
            Button button = new Button(frame, SWT.CHECK);
            button.setFont(font);
            button.setText(uiFormat.uiParmID(parm.getParmID()));
            button.setData(parm);
            button.setSelection(true);
            this.buttons.add(button);
        }

        return master;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt
     * .events.DisposeEvent)
     */
    @Override
    public void widgetDisposed(DisposeEvent e) {
        if (this.font != null) {
            this.font.dispose();
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        if (modparms.length > 0) {
            createButton(parent, IDialogConstants.OK_ID, "Revert Forecast",
                    true);
        }
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == IDialogConstants.OK_ID) {
            doOp();
        }

        super.buttonPressed(buttonId);
    }

    /**
     * Callback for reverting the parameters
     */
    private void doOp() {
        boolean hazardsSelected = false;
        // Cursor.toggleCursor(0, self)

        for (Button button : this.buttons) {
            if (button.getSelection()) {
                Parm parm = (Parm) button.getData();
                ParmID parmId = parm.getParmID();

                statusHandler.handle(Priority.DEBUG, "Revert: " + parmId);
                parm.revertParameter();
                if (parmId.getParmName().equals("Hazards")) {
                    hazardsSelected = true;
                }
            }
        }

        // remove temporary haz WEs if "Hazards" is modified and selected
        boolean removeTempHaz = hazardsSelected && this.hazardsModified;

        Parm[] displayedParms = this.dataManager.getParmManager()
                .getDisplayedParms();
        List<ParmID> newDisplayedParms = new ArrayList<ParmID>();
        for (Parm dp : displayedParms) {
            ParmID parmId = dp.getParmID();
            boolean hazParm = parmId.getParmName().startsWith("haz");
            if (!(hazParm && removeTempHaz)) {
                newDisplayedParms.add(parmId);
            }
        }

        // set the new displayed parms list
        this.dataManager.getParmManager()
                .setDisplayedParms(
                        newDisplayedParms.toArray(new ParmID[newDisplayedParms
                                .size()]));

        // Cursor.toggleCursor(1, self)
    }
}
