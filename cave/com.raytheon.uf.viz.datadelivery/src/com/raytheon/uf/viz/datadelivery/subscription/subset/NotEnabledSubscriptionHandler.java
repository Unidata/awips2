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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import org.eclipse.swt.SWT;

import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * Placeholder shared subscription handler that will display a notice the
 * functionality is not available. It should never be invoked unless the
 * 5-Data_Delivery phase 3 code is available, but provides another layer in case
 * it somehow is invoked.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2013 1841       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class NotEnabledSubscriptionHandler implements
        ISharedSubscriptionHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    public void launchCreateSharedSubscriptionGui(SubsetManagerDlg<?, ?, ?> subsetManagerDlg) {
        DataDeliveryUtils.showMessage(subsetManagerDlg.getShell(), SWT.OK,
                "Unavailable option", "Shared subscriptions are not enabled.");
    }

}
