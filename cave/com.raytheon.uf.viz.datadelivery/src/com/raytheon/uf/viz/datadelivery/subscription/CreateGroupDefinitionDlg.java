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
package com.raytheon.uf.viz.datadelivery.subscription;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.viz.datadelivery.common.ui.IGroupAction;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * The Data Delivery Create and Edit Subscription Group Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -------------------------
 * Jan 08, 2013 1453       djohnson     Split creation and edit dialogs.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class CreateGroupDefinitionDlg extends BaseGroupDefinitionDlg {

    /** Group name text field */
    private Text groupNameTxt;

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     * @param create
     * @param callback
     *            callback to subscription manager
     */
    public CreateGroupDefinitionDlg(Shell parent,
            IGroupAction callback) {
        super(parent, callback);
    }

    @Override
    protected String getDialogTitle() {
        return "Create Group";
    }

    /**
     * Create the Group information.
     */
    @Override
    protected void createGroupInfo() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        Group groupNameInfo = new Group(mainComp, SWT.NONE);
        groupNameInfo.setLayout(gl);
        groupNameInfo.setLayoutData(gd);
        groupNameInfo.setText("  Group Information  ");

        Label groupName = new Label(groupNameInfo, SWT.NONE);
        groupName.setText("Group Name: ");

        Composite groupComp = new Composite(groupNameInfo, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        groupComp.setLayoutData(gd);
        groupComp.setLayout(gl);

        groupNameTxt = new Text(groupComp, SWT.BORDER);
        groupNameTxt.setLayoutData(new GridData(285, SWT.DEFAULT));
        groupName.setToolTipText("Enter Group name");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getGroupName() {
        return groupNameTxt.getText().trim();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void saveGroupDefinition(GroupDefinition groupDefinition)
            throws RegistryHandlerException {
        DataDeliveryHandlers.getGroupDefinitionHandler().store(groupDefinition);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean validateGroupName(String groupName) {
        // Check for a group name
        if (groupName == null || groupName.isEmpty()) {
            DataDeliveryUtils.showMessage(shell, SWT.ERROR,
                    "Invalid Group Name", "No Group Name detected. \n\n"
                            + "Please enter a Group Name.\n");
            return false;
        } else if (GroupDefinition.NO_GROUP.equals(groupName)) {
            DataDeliveryUtils.showMessage(shell, SWT.ERROR,
                    "Invalid Group Name", "Invalid Group Name detected. \n\n"
                            + "Please enter a valid Group Name.\n");
            return false;
        }
        return true;
    }
}
