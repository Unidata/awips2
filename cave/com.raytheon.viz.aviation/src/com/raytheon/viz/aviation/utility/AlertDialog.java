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

package com.raytheon.viz.aviation.utility;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Alert Dialog class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 1/24/2008    817         grichard    Initial creation.
 * 20121010           1229  jkorman     Added DO_NOT_BLOCK so dialog does not block on open.     
 * Dec 15, 2017 7108        tgurney     Remove deiconify feature
 * 
 * </pre>
 * 
 * @author grichard
 */
public class AlertDialog extends CaveSWTDialog {

    private Combo raiseAlertCombo;

    private Combo playAlertCombo;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public AlertDialog(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Alert Dialog");
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        createComposite();
    }

    /**
     * Create the Composite that will contain the parts of the dialog.
     */
    private void createComposite() {

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        // Create the top composite widget
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        labelComp.setLayout(gl);
        labelComp.setLayoutData(gd);

        // Create the "alert" label
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label alertLabel = new Label(labelComp, SWT.CENTER);
        alertLabel.setText("Alert Options");
        alertLabel.setLayoutData(gd);

        // Create the bottom composite widget
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout layoutBC = new GridLayout(2, false);
        controlComp.setLayout(layoutBC);

        // Create the "raise" label
        // gd = new GridData(SWT.LEAD);
        Label raiseLabel = new Label(controlComp, SWT.NONE);
        raiseLabel.setText("raise");
        // raiseLabel.setLayoutData(gd);

        // Create the "raise" combo
        raiseAlertCombo = new Combo(controlComp, SWT.DROP_DOWN);
        raiseAlertCombo.setItems(configMgr
                .getComboValues(ResourceTag.NotifyRaise));
        raiseAlertCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
                configMgr.setDataAsString(ResourceTag.NotifyRaise,
                        raiseAlertCombo.getItem(raiseAlertCombo
                                .getSelectionIndex()));
            }
        });
        int index = raiseAlertCombo
                .indexOf(configMgr
                .getDataAsString(ResourceTag.NotifyRaise));
        raiseAlertCombo.select(index);

        // Create the "play" label
        // gd = new GridData(SWT.LEAD);
        Label playLabel = new Label(controlComp, SWT.NONE);
        playLabel.setText("play");
        // playLabel.setLayoutData(gd);

        // Create the "play" combo
        playAlertCombo = new Combo(controlComp, SWT.DROP_DOWN);
        playAlertCombo.setItems(configMgr
                .getComboValues(ResourceTag.NotifyPlay));
        playAlertCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
                configMgr.setDataAsString(ResourceTag.NotifyPlay,
                        playAlertCombo.getItem(playAlertCombo
                                .getSelectionIndex()));
            }
        });
        index = playAlertCombo.indexOf(configMgr
                .getDataAsString(ResourceTag.NotifyPlay));
        playAlertCombo.select(index);
    }
}
