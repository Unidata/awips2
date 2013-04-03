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
package com.raytheon.uf.edex.datadelivery.service.verify;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.edex.datadelivery.service.verify.SubscriptionIntegrityVerifier.IVerificationResponse;
import com.raytheon.uf.edex.datadelivery.service.verify.SubscriptionIntegrityVerifier.IVerificationStrategy;

/**
 * Simple implementation of {@link IVerificationStrategy}. Intentionally
 * package-private to enforce dependency injection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 07, 2012 1104       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class BaseSubscriptionVerifier implements IVerificationStrategy {

    private static class VerificationResponse implements IVerificationResponse {
        private final String message;

        public VerificationResponse(String message) {
            this.message = message;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean hasFailedVerification() {
            return message != null;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String getNotificationMessage() {
            return message;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IVerificationResponse verify(DataSet dataSet,
            Subscription subscription) {

        Collection<Parameter> dataSetParams = dataSet.getParameters()
                .values();
        if (dataSetParams == null) {
            dataSetParams = new ArrayList<Parameter>(0);
        }

        ArrayList<Parameter> subParams = subscription.getParameter();
        if (subParams == null) {
            subParams = new ArrayList<Parameter>(0);
        }
        subParams.removeAll(dataSetParams);

        final boolean invalidSubscriptionParameters = !subParams.isEmpty();

        if (invalidSubscriptionParameters) {
            StringBuilder sb = new StringBuilder();
            sb.append("Subscription ").append(subscription.getName())
                    .append(" has failed verification.  The following parameters are no longer available: ");
            for (Iterator<Parameter> iter = subParams.iterator(); iter
                    .hasNext();) {
                Parameter param = iter.next();
                sb.append(param.getName());
                if (iter.hasNext()) {
                    sb.append(", ");
                }
            }

            return new VerificationResponse(sb.toString());
        } else {
            return new VerificationResponse(null);
        }
    }

}
