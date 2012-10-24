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
import java.util.Collections;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.reference.GroupID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.ui.AccessMgr;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * Dialog to perform a save or delete of an Edit Area Group.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Mar 11, 2008					Eric Babin Initial Creation
 * Oct 24, 2012 1287       rferrel     Code clean part of non-blocking dialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class SaveDeleteEditAreaGroupDialog extends CaveJFACEDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveDeleteEditAreaGroupDialog.class);

    private IReferenceSetManager refSetManager;

    private String optionStr;

    private java.util.List<String> groupNames;

    private java.util.List<String> protectedGroupNames;

    private List groupList;

    private ToggleSelectList areaListbox;

    private Text identifer;

    private Button deleteAreas;

    private Button withVerify;

    private java.util.List<ReferenceID> refIds;

    public SaveDeleteEditAreaGroupDialog(Shell parent,
            IReferenceSetManager refSetManager, String optionStr) {
        super(parent);
        this.refSetManager = refSetManager;
        this.optionStr = optionStr;

        this.refIds = refSetManager.getAvailableSets();

        // get the list of Group Names
        this.groupNames = new ArrayList<String>();
        this.protectedGroupNames = new ArrayList<String>();
        java.util.List<GroupID> ids = refSetManager.getGroupIds();

        // save
        if (optionStr.equals("Save")) {
            for (GroupID id : ids) {
                if (!id.isProtected()) {
                    this.groupNames.add(id.getName());
                } else {
                    this.protectedGroupNames.add(id.getName());
                }
            }
        }

        // delete
        else {
            for (GroupID id : ids) {
                if (!id.isProtected()
                        && id.getAccess().equals(LocalizationLevel.USER)) {
                    this.groupNames.add(id.getName());
                } else {
                    this.protectedGroupNames.add(id.getName());
                }
            }
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

        if (optionStr.equals("Save")) {
            shell.setText("Save Edit Area Group");
        } else {
            shell.setText("Delete Edit Area Group");
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);

        // Set up Group Box
        Collections.sort(this.groupNames);
        Group leftGroup = new Group(comp, SWT.NONE);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        leftGroup.setLayoutData(layoutData);
        leftGroup.setLayout(new GridLayout(1, false));
        leftGroup.setText("Group Name");

        groupList = new List(leftGroup, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        groupList.setLayoutData(layoutData);
        groupList
                .setItems(this.groupNames.toArray(new String[groupNames.size()]));
        groupList.select(0);
        groupList.deselectAll();
        groupList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateAreaList();
            }
        });

        Label label = new Label(leftGroup, SWT.LEFT);
        label.setText("Identifier");
        identifer = new Text(leftGroup, SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        identifer.setLayoutData(layoutData);

        // Set up Edit Area Box
        if (this.optionStr.equals("Save")) {
            String[] eaNames = new String[this.refIds.size()];
            int i = 0;
            for (ReferenceID id : this.refIds) {
                eaNames[i++] = id.getName();
            }
            Arrays.sort(eaNames);

            ((GridLayout) comp.getLayout()).numColumns = 2;

            Group rightGroup = new Group(comp, SWT.NONE);
            layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
            rightGroup.setLayoutData(layoutData);
            rightGroup.setLayout(new GridLayout(1, false));
            rightGroup.setText("includes Edit Area(s)");

            areaListbox = new ToggleSelectList(rightGroup, SWT.BORDER
                    | SWT.MULTI | SWT.V_SCROLL);
            layoutData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
            layoutData.heightHint = areaListbox.getItemHeight() * 15;
            areaListbox.setLayoutData(layoutData);
            areaListbox.setItems(eaNames);
            areaListbox.select(0);
            areaListbox.deselectAll();
        } else {
            deleteAreas = new Button(leftGroup, SWT.CHECK);
            layoutData = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
            deleteAreas.setLayoutData(layoutData);
            deleteAreas.setText("Delete All Areas Within Group");
            deleteAreas.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    withVerify.setSelection(deleteAreas.getSelection());
                }
            });

            withVerify = new Button(leftGroup, SWT.CHECK);
            layoutData = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
            withVerify.setLayoutData(layoutData);
            withVerify.setText("with Verification");
        }

        return comp;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        if (optionStr.equals("Save")) {
            super.createButton(parent, Window.OK, "Save Group", false);
        } else {
            super.createButton(parent, Window.OK, "Delete Group", false);
        }
        super.createButton(parent, CANCEL, "Cancel", true);
    }

    @Override
    protected void okPressed() {
        if (optionStr.equals("Save")) {
            saveGroup();
        } else {
            deleteGroup();
        }
        super.okPressed();
    }

    /**
     * 
     */
    protected void updateAreaList() {
        int i = groupList.getSelectionIndex();
        String groupName = (i < 0 ? "" : groupList.getItem(i));
        identifer.setText(groupName);

        // Update the selections in the AreaList
        // For each group (there will only be one),
        // select the areas in the areaList
        if (this.optionStr.equals("Save")) {
            java.util.List<String> selAreas = this.refSetManager
                    .getGroupData(groupName);

            areaListbox.setSelection(selAreas.toArray(new String[selAreas
                    .size()]));
        }
    }

    /**
     * Callback for saving a group
     */
    protected void saveGroup() {
        String groupName = identifer.getText();
        String[] areas = areaListbox.getSelection();
        statusHandler.handle(Priority.DEBUG, "Save Edit Area Group: "
                + groupName + " sel:" + areas);

        if (!groupName.isEmpty() && !groupName.equals("Misc")
                && !protectedGroupNames.contains(groupName)) {
            refSetManager.saveGroup(groupName, Arrays.asList(areas));

        } else {
            String message = "Group Name " + groupName
                    + " is protected or an invalid name";
            statusHandler.handle(Priority.SIGNIFICANT, message);
        }
    }

    /**
     * Callback for deleting a group
     */
    private void deleteGroup() {
        final String groupName = identifer.getText();
        if (!groupName.isEmpty() && !protectedGroupNames.contains(groupName)
                && groupNames.contains(groupName)) {
            // valid name to delete
            boolean verify = AccessMgr.verifyDelete(groupName,
                    LocalizationType.COMMON_STATIC, false);
            if (verify) {
                java.util.List<String> areas = refSetManager
                        .getGroupData(groupName);

                statusHandler.handle(Priority.DEBUG, "Delete Edit Area Group: "
                        + groupName);
                refSetManager.deleteGroup(groupName);

                // Delete areas within the group if desired
                if (deleteAreas.getSelection()) {
                    for (String area : areas) {
                        if (okToDeleteEA(area)) {
                            deleteArea(area, withVerify.getSelection());
                        }
                    }
                }
            }
        } else {
            String message = "Group Name " + groupName
                    + " is protected or an invalid name";

            statusHandler.handle(Priority.SIGNIFICANT, message);
        }
    }

    /**
     * @param area
     * @return
     */
    private boolean okToDeleteEA(String area) {
        // Given an edit area name, returns true if ok to delete it.
        // OK if at same level (base/site/user) and not-protected.

        // find match
        for (ReferenceID a : this.refIds) {
            if (a.getName().equals(area)) {
                if (!a.isProtected()
                        && a.getAccess().equals(LocalizationLevel.USER)) {
                    return true;
                }
            }
        }
        return false; // no match found, can't delete it, should never get here
    }

    /**
     * @param area
     * @param withVerify
     */
    private void deleteArea(String areaName, boolean withVerify) {
        // Note: Area will be deleted from all groups by
        // UIReferenceSetMgr which processes deletions for
        // incoming ReferenceSetInventoryChanged messages

        ReferenceID id = new ReferenceID(areaName);
        if (id.isValid()) {
            refSetManager.deleteRefSet(id, withVerify);
        }
    }
}
