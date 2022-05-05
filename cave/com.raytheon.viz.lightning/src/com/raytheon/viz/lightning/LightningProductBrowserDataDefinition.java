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
package com.raytheon.viz.lightning;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.productbrowser.Activator;
import com.raytheon.uf.viz.productbrowser.datalisting.DataListingProductBrowserDefinition;

/**
 * Product browser implementation for lightning
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2010            mnash       Initial creation
 * Oct 26, 2017 6402       mapeters    Support multiple sources, refactored to
 *                                     extend DataListingProductBrowserDefinition
 *
 * </pre>
 *
 * @author mnash
 */

public class LightningProductBrowserDataDefinition
        extends DataListingProductBrowserDefinition {

    public LightningProductBrowserDataDefinition() {
        this("Lightning");
    }

    public LightningProductBrowserDataDefinition(String displayName) {
        super(displayName, new LightningDataListing(Arrays.asList(
                LightningConstants.SOURCE, LightningDataListing.START_TIME,
                LightningDataListing.TYPE)));

        /*
         * TODO: The following code is only needed temporarily and should
         * eventually be removed (see comment below).
         */
        String[] order = getOrderedKeys();
        if (order.length == 2) {
            /*
             * Order previously only contained 2 keys (startTime and type), but
             * a third key has been added (source). Old preference store values
             * that only have the original 2 keys will prevent the source level
             * from being shown in the product browser. To fix this, we prepend
             * the source key to the old order, so that it contains all 3 keys.
             *
             * The first time this if-block is executed for a user, it will fix
             * the invalid preference store value, so it will never be executed
             * again. Once this has been done for all necessary users, this code
             * can be removed.
             */
            List<String> newOrder = new ArrayList<>();
            newOrder.add(LightningConstants.SOURCE);
            for (String orderItem : order) {
                newOrder.add(orderItem);
            }
            order = newOrder.toArray(new String[0]);

            // Persist the fixed order so we don't have to do this again
            IPreferenceStore store = Activator.getDefault()
                    .getPreferenceStore();
            store.putValue(orderPreference.getLabel() + displayName,
                    String.join(",", order));
            orderPreference.setValue(order);
        }
    }

    @Override
    protected AbstractResourceData createResourceData(
            Map<String, String> keyVals) {
        LightningResourceData resourceData = new LightningResourceData();

        // Copy so we can safely modify
        Map<String, String> keyValsCopy = new HashMap<>(keyVals);

        // Remove non-metadataMap entries and handle them
        String type = keyValsCopy.remove(LightningDataListing.TYPE);
        boolean positiveNegative = LightningDataListing.POSITIVE_NEGATIVE
                .equals(type);
        resourceData.setHandlingPositiveStrikes(positiveNegative
                || LightningDataListing.POSITIVE.equals(type));
        resourceData.setHandlingNegativeStrikes(positiveNegative
                || LightningDataListing.NEGATIVE.equals(type));
        resourceData.setHandlingCloudFlashes(
                LightningDataListing.CLOUD_FLASH.equals(type));
        resourceData.setHandlingPulses(
                LightningDataListing.PULSE.equals(type));

        int negativeOffset = Integer
                .valueOf(keyValsCopy.remove(LightningDataListing.START_TIME));
        resourceData.setBinOffset(new BinOffset(0, negativeOffset));

        // Create metadataMap from remaining keys
        resourceData.setMetadataMap(
                new HashMap<>(listing.getRequestConstraints(keyValsCopy)));

        return resourceData;
    }
}
