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
package com.raytheon.uf.viz.datadelivery.common.ui;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.viz.datadelivery.subscription.GroupDefinitionManager;
import com.raytheon.viz.ui.presenter.components.ComboBoxConf;

/**
 * This is the subscription group information composite. This class is intended
 * to be extended so common classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012   702      jpiatt      Initial creation.
 * Aug 29, 2012   223      mpduff      REfactor change.
 * Dec 18, 2012  1440      mpduff      Ignore "None" group.
 * Jan 02, 2013  1441      djohnson    Add isGroupSelected(), separate NO_GROUP constant to reusable location.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class GroupSelectComp extends Composite {

    /** Group Name combo box. */
    private Combo groupNameCombo;

    /** Create/edit flag */
    private final boolean create;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param create
     *            create flag
     */
    public GroupSelectComp(Composite parent, boolean create) {
        super(parent, SWT.NONE);
        this.create = create;
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        /*
         * Setup the layout for the composite
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createGroupInfo();
        loadGroupNames();
    }

    /**
     * Create the optional Group Name information.
     */
    private void createGroupInfo() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Group groupNameInfo = new Group(this, SWT.NONE);
        groupNameInfo.setLayout(gl);
        groupNameInfo.setLayoutData(gd);
        groupNameInfo.setText("  Group Information  ");

        Composite groupTopComp = new Composite(groupNameInfo, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(2, false);
        groupTopComp.setLayoutData(gd);
        groupTopComp.setLayout(gl);

        Label groupName = new Label(groupTopComp, SWT.NONE);
        groupName.setText("Group Name: ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        groupNameCombo = new Combo(groupTopComp, SWT.READ_ONLY);
        if (!create) {
            groupNameCombo.select(0);
        }
        groupNameCombo.setLayoutData(gd);
    }

    /**
     * Load the list of group names.
     * 
     * Default is "None" or "All Subscriptions" depending on whether in create
     * mode or not
     */
    public void loadGroupNames() {

        List<String> groupNameList = GroupDefinitionManager.getGroupNames();
        String firstGroupAllOption = (create) ? GroupDefinition.NO_GROUP
                : "All Subscriptions";
        groupNameList.add(0, firstGroupAllOption);

        groupNameCombo.setItems(groupNameList.toArray(new String[0]));
        groupNameCombo.select(0);
    }

    /**
     * Select the group name.
     */
    private void selectGroupName(String groupNameSel) {

        if (groupNameSel != null) {
            int i = 0;
            for (String group : groupNameCombo.getItems()) {
                if (groupNameSel.equals(group)) {
                    groupNameCombo.select(i);
                    break;
                }
                i++;
            }

        } else {
            groupNameCombo.select(0);
        }
    }

    /**
     * Set Group Name
     * 
     * @param groupName
     */
    public void setGroupName(String groupName) {
        selectGroupName(groupName);
    }

    /**
     * Get the group name combo box text value.
     * 
     * @return group name
     */
    public String getGroupName() {
        return groupNameCombo.getText().trim();
    }

    /**
     * @param groupNameComboConf
     *            the groupNameComboConf to set
     */
    public void setGroupNameComboConf(final ComboBoxConf groupNameComboConf) {
        if (groupNameComboConf != null) {
            this.groupNameCombo.setToolTipText(groupNameComboConf
                    .getToolTipText());
            groupNameCombo.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (isGroupSelected()) {
                        groupNameComboConf.getOnSelectionAction().run();
                    }
                }
            });
        }
    }

    /**
     * Return true if a valid group is selected.
     * 
     * @return
     */
    public boolean isGroupSelected() {
        return !GroupDefinition.NO_GROUP.equalsIgnoreCase(groupNameCombo
                .getText());
    }
}
