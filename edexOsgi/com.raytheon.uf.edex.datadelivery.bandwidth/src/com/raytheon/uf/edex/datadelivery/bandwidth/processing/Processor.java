package com.raytheon.uf.edex.datadelivery.bandwidth.processing;

import java.util.ArrayList;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.IProcessSubscription;

/**
 * Process Available Subscriptions for bundling
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2012            dhladky     Initial creation
 * Jun 21, 2012 736        djohnson    Change OPERATION_STATUS to OperationStatus.
 * Aug 20, 2012 0743       djohnson    Finish making registry type-safe.
 * Oct 05, 2012 1241       djohnson    Replace RegistryManager calls with registry handler calls.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class Processor implements IProcessSubscription {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Processor.class);

    public Processor() {

    }

    @Override
    public ArrayList<SubscriptionBundle> process(
            ArrayList<Subscription> subscriptions) {
        // either a new subscription/adhoc or all subscriptions for a given
        // dataset

        ArrayList<SubscriptionBundle> bundles = null;

        // TODO: When we start aggregating the bundles will matter
        if (subscriptions != null) {
            bundles = new ArrayList<SubscriptionBundle>(subscriptions.size());

            for (Subscription sub : subscriptions) {
                statusHandler.info("Processing Subscription  NAME: "
                        + sub.getDescription() + " DATASET: "
                        + sub.getDataSetName());
                SubscriptionBundle bundle = new SubscriptionBundle();
                Provider provider = getProvider(sub.getProvider());

                bundle.setBundleId(sub.getSubscriptionId());
                bundle.setPriority(1);
                bundle.setProvider(provider);
                bundle.setConnection(provider.getConnection());
                bundle.setSubscription(sub);
                // when aggregated set source subscriptions to bundle

                bundles.add(bundle);
            }
        } else {
            statusHandler.info("No mature subscriptions available.");
        }

        return bundles;
    }

    public Provider getProvider(String providerName) {

        try {
            return DataDeliveryHandlers.getProviderHandler().getByName(
                    providerName);
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to look up the provider by its name.", e);
            return null;
        }
    }

}
