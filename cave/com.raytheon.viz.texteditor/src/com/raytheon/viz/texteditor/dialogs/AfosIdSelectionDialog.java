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

import com.raytheon.viz.texteditor.msgs.IAfosIdSelectionCallback;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Contains the AfosIdSelectionDialog
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

public class AfosIdSelectionDialog extends CaveJFACEDialog {

    private final IAfosIdSelectionCallback callback;

    private java.util.List<String> afosIds;

    private List afosIdList;

    /**
     * Constructor
     * 
     * @param parentShell
     *            parent shell
     * @param dataManager
     *            DataManager for the associated window
     */
    public AfosIdSelectionDialog(Shell parentShell,
            IAfosIdSelectionCallback callback, java.util.List<String> afosIds) {
        super(parentShell);
        this.callback = callback;
        this.afosIds = afosIds;
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
        newShell.setText("AFOS ID Selection");
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
        label.setText("WMO ID maps to multiple AFOS IDs;");
        label.setLayoutData(data);
        label = new Label(top, SWT.CENTER);
        label.setText("Please select an AFOS ID");
        data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        label.setLayoutData(data);

        afosIdList = new List(top, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL);
        data = new GridData(SWT.BEGINNING, SWT.DEFAULT, true, false);
        afosIdList.setLayoutData(data);

        for (String id : afosIds) {
            afosIdList.add(id);
        }

        afosIdList.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                int index = afosIdList.getSelectionIndex();

                if (index >= 0) {
                    String afosId = afosIdList.getItem(index);
                    callback.setAfosId(afosId);
                    close();
                }
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = afosIdList.getSelectionIndex();

                if (index >= 0) {
                    String afosId = afosIdList.getItem(index);
                    callback.setAfosId(afosId);
                }
            }

        });

        afosIdList.select(0);
        callback.setAfosId(afosIdList.getItem(0));

        top.layout();
        return top;
    }
}
