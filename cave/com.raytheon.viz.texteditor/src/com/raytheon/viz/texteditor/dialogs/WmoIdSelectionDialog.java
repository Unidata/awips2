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
package com.raytheon.viz.texteditor.dialogs;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.texteditor.msgs.IWmoIdSelectionCallback;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Contains the WmoIdSelectionDialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2009 2924       rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class WmoIdSelectionDialog extends CaveJFACEDialog {

    private final IWmoIdSelectionCallback callback;

    private java.util.List<String> ttaaiiIds;

    private java.util.List<String> ccccIds;

    private List wmoIdList;

    /**
     * Constructor
     * 
     * @param parentShell
     *            parent shell
     * @param dataManager
     *            DataManager for the associated window
     */
    public WmoIdSelectionDialog(Shell parentShell,
            IWmoIdSelectionCallback callback, java.util.List<String> ttaaiiIds,
            java.util.List<String> ccccIds) {
        super(parentShell);
        this.callback = callback;
        this.ttaaiiIds = ttaaiiIds;
        this.ccccIds = ccccIds;
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
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
        newShell.setText("WMO ID Selection");
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
        createButton(parent, IDialogConstants.OK_ID, "Close", false);
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
        Composite top = (Composite) super.createDialogArea(parent);
        GridLayout layout = new GridLayout(1, false);
        top.setLayout(layout);
        GridData data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label label = new Label(top, SWT.CENTER);
        label.setText("AFOS ID maps to multiple WMO IDs;");
        label.setLayoutData(data);
        label = new Label(top, SWT.CENTER);
        label.setText("Please select a WMO ID");
        data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        label.setLayoutData(data);

        wmoIdList = new List(top, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL);
        data = new GridData(SWT.BEGINNING, SWT.DEFAULT, true, false);
        wmoIdList.setLayoutData(data);

        for (int i = 0; i < ttaaiiIds.size(); i++) {
            wmoIdList.add(ttaaiiIds.get(i) + " " + ccccIds.get(i));
        }

        wmoIdList.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                int index = wmoIdList.getSelectionIndex();

                if (index >= 0) {
                    callback.setWmoId(ttaaiiIds.get(index), ccccIds.get(index));
                    close();
                }
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = wmoIdList.getSelectionIndex();

                if (index >= 0) {
                    callback.setWmoId(ttaaiiIds.get(index), ccccIds.get(index));
                }
            }

        });

        wmoIdList.select(0);
        callback.setWmoId(ttaaiiIds.get(0), ccccIds.get(0));

        top.layout();
        return top;
    }
}
