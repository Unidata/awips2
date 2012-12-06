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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IWEGroupManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The weather element group dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/22/2008              Eric Babin  Initial Creation
 * 04/17/2009   #2282      rjpeter     Added confirmation message on delete.
 * 11/20/2012   DR 15532   jzeng       Added popup dialog to make sure group saved with 
 * 									   valid characters
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class WeatherElementGroupDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WeatherElementGroupDialog.class);

    private Composite top;

    private java.util.List<String> names;

    private java.util.List<String> protectedNames;

    private boolean saveType = true;

    private List groupList;

    private Text groupField;

    private String selectedItem;

    private DataManager dataManager;

    private IWEGroupManager wegManager;

    public WeatherElementGroupDialog(Shell parent, DataManager dataManager,
            boolean saveType) {
        super(parent);
        this.dataManager = dataManager;
        this.wegManager = this.dataManager.getWEGroupManager();
        this.saveType = saveType;

        this.names = new ArrayList<String>();
        this.protectedNames = new ArrayList<String>();
        java.util.List<String> ids;
        if (this.saveType) {
            ids = wegManager.getInventory();
        } else {
            ids = wegManager.getUserInventory();
        }
        for (String id : ids) {
            if (wegManager.isProtected(id)) {
                protectedNames.add(id);
            } else {
                names.add(id);
            }
        }
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

        GridData data = new GridData(GridData.FILL_BOTH);

        groupList = new List(top, SWT.BORDER | SWT.SINGLE);
        groupList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent arg0) {
                int index = groupList.getSelectionIndex();
                if (index >= 0) {
                    selectedItem = groupList.getItem(index);
                    groupField.setText(selectedItem);
                    groupField.setSelection(selectedItem.length(),
                            selectedItem.length());
                    groupField.setFocus();
                }
            }
        });
        groupList.setItems(names.toArray(new String[names.size()]));
        groupList.setSelection(0);
        groupList.deselectAll();
        groupList.setLayoutData(data);
        groupField = new Text(top, SWT.BORDER);
        data = new GridData(GridData.FILL_HORIZONTAL);
        groupField.setLayoutData(data);
        groupField.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent arg0) {
                selectedItem = groupField.getText();
            }
        });

        groupField.setFocus();
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

        if (saveType) {
            shell.setText("Save Weather Element Group");
        } else {
            shell.setText("Delete Weather Element Group");
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        String label;
        if (saveType) {
            label = "Save";
        } else {
            label = "Delete";
        }
        createButton(parent, IDialogConstants.OK_ID, label, true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    public String getSelectedItem() {
        return selectedItem;
    }

    public void setSelectedItem(String selectedItem) {
        this.selectedItem = selectedItem;
    }

    @Override
    protected void okPressed() {
        boolean ok = true;
        String groupName = getSelectedItem();
        
        if (!saveType) {            
            if (!FileUtil.isValidFilename(groupName)) {
                MessageBox mb = new MessageBox(super.getShell(), SWT.ICON_ERROR
                        | SWT.OK);
                mb.setText("Invalid Group Name");
                mb.setMessage("Group name may only contain the following characters: "
                        + FileUtil.VALID_FILENAME_CHARS);
                ok = false;
            } else if (!protectedNames.contains(groupName)
                    && names.contains(groupName)) {
                MessageBox mb = new MessageBox(super.getShell(),
                        SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
                mb.setText("Item Delete");
                mb.setMessage(getSelectedItem() + " will be Deleted.");

                if (mb.open() == SWT.CANCEL) {
                    ok = false;
                }
            } else {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Weather Element Group " + groupName
                                + " is protected or an invalid name.");
                ok = false;
            }
        } else {
            if (protectedNames.contains(getSelectedItem())) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Weather Element Group " + getSelectedItem()
                                + " is protected or an invalid name.");
                ok = false;
            }

            if (!FileUtil.isValidFilename(groupName)) {
                MessageBox mb = new MessageBox(super.getShell(), SWT.ICON_ERROR
                        | SWT.OK);
                mb.setText("Invalid Group Name");
                mb.setMessage("Group name may only contain the following characters: "
                        + FileUtil.VALID_FILENAME_CHARS);
                mb.open();
                ok = false;
            }

        }

        if (ok) {
            super.okPressed();
        }
    }
}
