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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.RefType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * Dialog for performing a save or delete of an Edit Area.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2010            randerso     Initial creation
 * Oct 24, 2012 1287       rferrel     Code clean up part of non-blocking dialog.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SaveDeleteRefDialog extends CaveJFACEDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveDeleteRefDialog.class);

    private final Pattern pythonVariable = Pattern.compile("\\p{Alpha}\\w*");

    private final int NUM_ITEMS = 10;

    private IReferenceSetManager refSetMgr;

    private String optionStr;

    private java.util.List<String> sets;

    private java.util.List<String> protectedSets;

    private org.eclipse.swt.widgets.List editAreaList;

    private Text identifier;

    private ToggleSelectList groupListBox;

    private Text groupText;

    /**
     * @param parentShell
     */
    protected SaveDeleteRefDialog(Shell parentShell,
            IReferenceSetManager refSetMgr, String optionStr) {
        super(parentShell);
        this.refSetMgr = refSetMgr;
        this.optionStr = optionStr;

        this.sets = new ArrayList<String>();
        this.protectedSets = new ArrayList<String>();
        java.util.List<ReferenceID> refIds = this.refSetMgr.getAvailableSets();

        if (this.optionStr.equals("Save")) {
            for (ReferenceID id : refIds) {
                if (!id.isProtected()) {
                    this.sets.add(id.getName());
                } else {
                    this.protectedSets.add(id.getName());
                }
            }
        }
        // delete mode
        else {
            for (ReferenceID id : refIds) {
                if (!id.isProtected()
                        && id.getAccess().equals(LocalizationLevel.USER)) {
                    this.sets.add(id.getName());
                } else {
                    this.protectedSets.add(id.getName());
                }
            }
        }

        Collections.sort(sets);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);

        if (this.optionStr.equals("Save")) {
            newShell.setText("Save Active Edit Area");
        } else {
            newShell.setText("Delete Selected Edit Area");
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        ((GridLayout) comp.getLayout()).numColumns = 2;

        Group areaFrame = new Group(comp, SWT.NONE);
        areaFrame.setLayout(new GridLayout());
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        areaFrame.setLayoutData(layoutData);
        areaFrame.setLayout(new GridLayout(1, false));
        areaFrame.setText("Edit Area Name");

        editAreaList = new org.eclipse.swt.widgets.List(areaFrame, SWT.BORDER
                | SWT.V_SCROLL | SWT.SINGLE);

        Rectangle rect = editAreaList.computeTrim(0, 0,
                this.convertWidthInCharsToPixels(24),
                editAreaList.getItemHeight() * NUM_ITEMS);
        layoutData = new GridData(GridData.FILL_BOTH);
        layoutData.minimumWidth = rect.width;
        layoutData.heightHint = rect.height;
        editAreaList.setLayoutData(layoutData);
        editAreaList.setItems(this.sets.toArray(new String[sets.size()]));
        editAreaList.select(0);
        editAreaList.deselectAll();
        editAreaList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateGroupList();
            }
        });

        Label idLabel = new Label(areaFrame, SWT.NONE);
        idLabel.setText("Identifier");
        identifier = new Text(areaFrame, SWT.BORDER | SWT.SINGLE);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        identifier.setLayoutData(layoutData);

        if (this.optionStr.equals("Save")) {
            // If the Active Reference Set is a query,
            // Set a default name for saving
            ReferenceData activeRefSet = this.refSetMgr.getActiveRefSet();
            if (activeRefSet.isQuery()) {
                String queryStr = activeRefSet.getQuery().replace(" ", "");
                // Check for reserved name
                if (this.checkReserved(queryStr)) {
                    queryStr = "";
                    this.identifier.setText(queryStr);
                }
            }

            // Set up Group Box
            // Get the list of Group Names
            java.util.List<String> groupNames = this.refSetMgr
                    .getGroupInventory();

            // Create Listbox allowing multiple selections
            Group groupFrame = new Group(comp, SWT.NONE);
            layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
            groupFrame.setLayoutData(layoutData);
            groupFrame.setLayout(new GridLayout(1, false));
            groupFrame.setText("included in Group Name(s)");

            groupListBox = new ToggleSelectList(groupFrame, SWT.BORDER
                    | SWT.V_SCROLL | SWT.MULTI);
            rect = groupListBox.computeTrim(0, 0,
                    this.convertWidthInCharsToPixels(24),
                    groupListBox.getItemHeight() * NUM_ITEMS);
            layoutData = new GridData(GridData.FILL_BOTH);
            layoutData.minimumWidth = rect.width;
            layoutData.heightHint = rect.height;
            groupListBox.setLayoutData(layoutData);
            groupListBox.setItems(groupNames.toArray(new String[groupNames
                    .size()]));

            Label groupLabel = new Label(groupFrame, SWT.NONE);
            groupLabel.setText("New Group");
            groupText = new Text(groupFrame, SWT.BORDER | SWT.SINGLE);
            layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            groupText.setLayoutData(layoutData);
        }

        return comp;
    }

    /**
     * 
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        String label;
        if (this.optionStr.equals("Save")) {
            label = "Save Active Area";
        } else {
            label = "Delete Selected Area";
        }
        createButton(parent, IDialogConstants.OK_ID, label, true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void buttonPressed(int buttonId) {
        if (IDialogConstants.OK_ID == buttonId) {
            if (optionStr.equals("Save")) {
                if (!saveRef()) {
                    return;
                }
            } else {
                if (!deleteRef()) {
                    return;
                }
            }
        }
        super.buttonPressed(buttonId);
    }

    private boolean saveRef() {
        // Cannot save Empty reference area
        ReferenceData refData = this.refSetMgr.getActiveRefSet();
        if (refData.refType().equals(RefType.NONE)) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Cannot Save Empty Edit Area");
            return false;
        }

        String name = this.identifier.getText();
        // LogStream.logUse("Save", name);

        // Check reserved name
        if (this.checkReserved(name)) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Reserved Name: QuickSet. Please rename.");
            return false;
        }

        // Check that name is a legal Python variable so
        // it can be used subsequently in queries
        if (!pythonVariable.matcher(name).matches()) {
            statusHandler
                    .handle(Priority.SIGNIFICANT,
                            "Illegal Name: Must begin with a letter and be alphanumeric or '_'. Please rename.");
            return false;
        }

        // Check for recursion
        if (refData.isQuery()) {
            if (this.refSetMgr.willRecurse(name, refData.getQuery())) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Saving a query which refers to "
                                + "itself will cause infinte loop. "
                                + "Use: Convert to location.");
                return false;
            }
        }

        ReferenceID id = new ReferenceID(name);
        if (id.isValid() && !this.protectedSets.contains(name)) {
            if (!this.refSetMgr.saveActiveRefSet(id)) {
                return false;
            }
        } else {
            String message = "Edit Area " + name + " is protected or invalid.";
            statusHandler.handle(Priority.SIGNIFICANT, message);
            return false;
        }

        // Update all groups
        // Updating groups for additions is a local operation
        // (compared to deletions) since group definitions are
        // per user.
        List<String> groups = new ArrayList<String>();
        for (String s : this.groupListBox.getSelection()) {
            groups.add(s);
        }

        String groupName = this.groupText.getText();
        if (!groupName.isEmpty() && !groupName.equals("Misc")) {
            groups.add(groupName);
        }
        this.updateGroups(name, groups);

        return true;
    }

    private boolean deleteRef() {
        // Note: Area will be deleted from all groups by
        // UIReferenceSetMgr which processes deletions for
        // incoming ReferenceSetInventoryChanged messages

        ReferenceID id = new ReferenceID(this.identifier.getText());
        if (id.isValid() && !this.protectedSets.contains(id.getName())
                && this.sets.contains(id.getName())) {
            // Check to see if deleting active reference set
            // If so, clear the active reference set
            // LogStream.logUse("Delete", this.lbox.name())
            return this.refSetMgr.deleteRefSet(id, true);

        } else {
            String message = "Edit Area " + id.getName()
                    + " is protected or invalid.";
            statusHandler.handle(Priority.SIGNIFICANT, message);
            return false;
        }
    }

    private boolean checkReserved(String name) {
        // Check to make sure name is not a reserved name.
        // Reserved names are QuickSet<n>
        // Note -- this could be done more elegantly with
        // a regular expression, but I didn't want to introduce
        // that complication.
        if (name.startsWith("QuickSet") && name.length() >= 8
                && name.length() <= 10) {
            return true;
        } else {
            return false;
        }
    }

    protected void updateGroupList() {
        String[] areaNames = this.editAreaList.getSelection();
        if (areaNames.length > 0) {
            this.identifier.setText(areaNames[0]);
        } else {
            this.identifier.setText("");
        }

        // Update the selections in the GroupList
        // For each areaName, select any group that contains it.
        if (!this.optionStr.equals("Save")) {
            return;
        }

        List<String> groupNames = this.refSetMgr.getGroupInventory();
        Set<String> selectedGroups = new HashSet<String>();
        for (String groupName : groupNames) {
            List<String> areas = this.refSetMgr.getGroupData(groupName);
            for (String area : areaNames) {
                if (areas.contains(area)) {
                    selectedGroups.add(groupName);
                    break;
                }
            }
        }
        for (int i = 0; i < this.groupListBox.getItemCount(); i++) {
            String name = this.groupListBox.getItem(i);
            if (selectedGroups.contains(name)) {
                this.groupListBox.select(i);
            } else {
                this.groupListBox.deselect(i);
            }
        }
    }

    private void updateGroups(String areaName, List<String> groupNames) {
        // Make sure the area is in the given groups and no others
        // Add new group if not there

        List<String> invNames = this.refSetMgr.getGroupInventory();
        for (String invName : invNames) {
            List<String> invAreas = this.refSetMgr.getGroupData(invName);
            if (groupNames.contains(invName)) {
                // Make sure area is in the inventory group
                if (!invAreas.contains(areaName)) {
                    invAreas.add(areaName);
                    this.refSetMgr.saveGroup(invName, invAreas);
                }
            } else {
                // Make sure area is not in inventory group
                if (invAreas.contains(areaName)) {
                    invAreas.remove(areaName);
                    this.refSetMgr.saveGroup(invName, invAreas);
                }
            }
        }

        // Create new group if necessary
        for (String groupName : groupNames) {
            if (!invNames.contains(groupName)) {
                this.refSetMgr.saveGroup(groupName, Arrays.asList(areaName));
            }
        }
    }
}
