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
package com.raytheon.uf.viz.datadelivery.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.datadelivery.services.DataDeliveryServices;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionManagerFilter;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionManagerDlg;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionManagerFilters;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * Subscription Manager Dialog Action class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2012            mpduff       Initial creation
 * Oct 03, 2012 1241       djohnson     Use {@link DataDeliveryPermission}.
 * May 28, 2013 1650       djohnson     Allow using filters for the Subscription Manager Dialog.
 * Jul 26, 2031 2232       mpduff       Refactored Data Delivery permissions.
 * Sep 04, 2013 2330       bgonzale     execute now filters subscriptions by current site id.
 * Feb 11, 2014 2771       bgonzale     Use Data Delivery ID instead of Site.
 * 
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionManagerAction extends AbstractHandler {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionManagerAction.class);

    /** Dialog instance */
    private SubscriptionManagerDlg dlg = null;

    /** Permission String */
    private final String permission = DataDeliveryPermission.SUBSCRIPTION_VIEW
            .toString();

    /**
     * Constructor.
     */
    public SubscriptionManagerAction() {
    }

    @Override
    public Object execute(ExecutionEvent arg0) {
        return loadSubscriptionManager(SubscriptionManagerFilters
                .getBySiteId(DataDeliveryUtils.getDataDeliveryId()));
    }

    /**
     * Load the SubscriptionManager dialog with the specified filter.
     * 
     * @param filter
     *            the filter
     */
    public Object loadSubscriptionManager(ISubscriptionManagerFilter filter) {
        try {
            // check if user is authorized
            IUser user = UserController.getUserObject();
            String msg = user.uniqueId()
                    + " is not authorized to access Data Delivery\nPermission: "
                    + permission;

            if (DataDeliveryServices.getPermissionsService()
                    .checkPermission(user, msg, permission).isAuthorized()) {
                if ((dlg == null) || (dlg.isDisposed() == true)) {
                    Shell shell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();
                    dlg = new SubscriptionManagerDlg(shell, filter);
                    dlg.open();
                } else {
                    dlg.bringToTop();
                }
            }
        } catch (AuthException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return null;
    }
}
