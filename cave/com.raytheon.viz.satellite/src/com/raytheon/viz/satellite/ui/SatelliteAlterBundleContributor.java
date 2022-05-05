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
package com.raytheon.viz.satellite.ui;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.d2d.core.procedures.AlterBundleContributorAdapter;
import com.raytheon.viz.satellite.SatDataListing;
import com.raytheon.viz.satellite.rsc.SatResourceData;

/**
 * Class to handle alter bundles for satellite.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21 2019  70118      ksunil      Initial creation
 *
 * </pre>
 *
 * @author ksunil
 * @version 1.0
 */

public class SatelliteAlterBundleContributor
        extends AlterBundleContributorAdapter {

    private static final IUFStatusHandler handler = UFStatus
            .getHandler(SatelliteAlterBundleContributor.class);

    private SatDataListing sats = new SatDataListing(Arrays
            .asList(SatDataListing.SECTOR_ID, SatDataListing.PHYSICAL_ELEMENT));

    // This also serves as the display label in the alter GUI.
    private static final String PHYSICAL_ELEMENT_KEY = "Satellite Physical Elem";

    private static final String SECTOR_ID_KEY = "Satellite Sector";

    @Override
    public Map<String, String[]> getAlterables() {
        Map<String, String[]> alterables = new HashMap<>();

        try {
            Collection<String> col = sats.getValues(SatDataListing.SECTOR_ID,
                    Collections.emptyMap());
            alterables.put(SECTOR_ID_KEY,
                    col.stream().sorted().toArray(String[]::new));
            col = sats.getValues(SatDataListing.PHYSICAL_ELEMENT,
                    Collections.emptyMap());
            alterables.put(PHYSICAL_ELEMENT_KEY,
                    col.stream().sorted().toArray(String[]::new));

        } catch (Exception e1) {
            handler.handle(Priority.ERROR,
                    "Error occurred looking up satellite values", e1);
        }
        return alterables;
    }

    @Override
    public void alterBundle(Bundle bundleToAlter, String alterKey,
            String alterValue) {
        if (SECTOR_ID_KEY.equals(alterKey)
                || PHYSICAL_ELEMENT_KEY.equals(alterKey)) {
            for (AbstractRenderableDisplay display : bundleToAlter
                    .getDisplays()) {
                for (ResourcePair rp : display.getDescriptor()
                        .getResourceList()) {
                    AbstractResourceData rData = rp.getResourceData();
                    if (rData instanceof SatResourceData) {
                        Map<String, RequestConstraint> reqMap = ((SatResourceData) rData)
                                .getMetadataMap();
                        RequestConstraint rc = null;
                        if (SECTOR_ID_KEY.equals(alterKey)) {
                            rc = reqMap.get(SatDataListing.SECTOR_ID);
                        } else if (PHYSICAL_ELEMENT_KEY.equals(alterKey)) {
                            rc = reqMap.get(SatDataListing.PHYSICAL_ELEMENT);
                        }

                        if (rc != null) {
                            rc.setConstraintValue(alterValue);
                        }
                    }
                }
            }
        }

    }
}
