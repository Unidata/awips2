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
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetQuery;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.viz.datadelivery.common.ui.GroupSelectComp;
import com.raytheon.uf.viz.datadelivery.common.ui.IGroupAction;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.presenter.components.ComboBoxConf;

/**
 * The Data Delivery Create and Edit Subscription Group Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -------------------------
 * Jul 2, 2012    702      jpiatt       Initial creation.
 * Aug 02, 2012 955        djohnson     Type-safe registry query/responses.
 * Aug 10, 2012 1022       djohnson     {@link DataSetQuery} requires provider name.
 * Aug 20, 2012 0743       djohnson     Finish making registry type-safe.
 * Aug 29, 2012   223      mpduff       Renamed some methods.
 * Aug 31, 2012   702      jpiatt       Correct group data population.
 * Oct 03, 2012 1241       djohnson     Use {@link DataDeliveryPermission} and registry handlers.
 * Dec 18, 2012 1440       mpduff       Made non-blocking
 * Dec 10, 2012 1259       bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Jan 02, 2013 1441       djohnson     Access GroupDefinitionManager in a static fashion.
 * Jan 08, 2013 1453       djohnson     Split creation and edit dialogs.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class EditGroupDefinitionDlg extends BaseGroupDefinitionDlg {

    /** The Subscription Group Information Composite */
    private GroupSelectComp groupSelectComp;

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     * @param create
     * @param callback
     *            callback to subscription manager
     */
    public EditGroupDefinitionDlg(Shell parent, IGroupAction callback) {
        super(parent, callback);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();

        Runnable populate = new Runnable() {
            @Override
            public void run() {
                populate(groupSelectComp.getGroupName());
            }
        };
        ComboBoxConf groupComboConf = new ComboBoxConf(true,
                "Select a Group", populate);
        groupSelectComp.setGroupNameComboConf(groupComboConf);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getGroupName() {
        return groupSelectComp.getGroupName();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getDialogTitle() {
        return "Edit Group";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void createGroupInfo() {
        groupSelectComp = new GroupSelectComp(mainComp, true);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void saveGroupDefinition(GroupDefinition groupDefinition)
            throws RegistryHandlerException {
        DataDeliveryHandlers.getGroupDefinitionHandler()
                .update(groupDefinition);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected boolean validateGroupName(String groupName) {
        // Check for a group name
        if (GroupDefinition.NO_GROUP.equals(groupName)) {
            DataDeliveryUtils.showMessage(shell, SWT.ERROR,
                    "Invalid Group Name", "No Group Name detected. \n\n"
                            + "Please select a Group Name.\n");
            return false;
        }
        return true;
    }

}
