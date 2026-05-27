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
package com.raytheon.viz.radar.ui;

import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.localization.LocalizationConstants;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.groups.BestResResourceData;
import com.raytheon.uf.viz.d2d.core.procedures.AlterBundleContributorAdapter;
import com.raytheon.viz.radar.util.RadarAsGridUtil;

/**
 * Class to handle alter bundles for radar.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2010            mschenke    Initial creation
 * Oct 03, 2012 1248       rferrel     Change to use adapter.
 * Jun 24, 2022 8869       mapeters    Alter "info.datasetId" constraint,
 *                                     prevent listing duplicate sites
 *
 * </pre>
 *
 * @author mschenke
 */
public class RadarAlterBundleContributor extends AlterBundleContributorAdapter {

    private static final String RADAR_KEY = "Radar";

    private void alterResourceList(ResourceList list, String selectedString) {
        for (ResourcePair rp : list) {
            AbstractResourceData rData = rp.getResourceData();
            if (rData instanceof AbstractRequestableResourceData) {
                alterResource((AbstractRequestableResourceData) rData,
                        selectedString);
            } else if (rData instanceof IResourceGroup) {
                alterResourceList(((IResourceGroup) rData).getResourceList(),
                        selectedString);
            } else if (rData instanceof BestResResourceData) {
                alterResourceList(
                        ((BestResResourceData) rData).getResourceList(),
                        selectedString);
                alterResource((BestResResourceData) rData, selectedString);
            }
        }
    }

    private void alterResource(AbstractRequestableResourceData data,
            String selectedString) {
        Map<String, RequestConstraint> reqMap = data.getMetadataMap();
        RequestConstraint rc = reqMap.get("info.datasetId");
        if (rc != null
                && RadarAsGridUtil.isRadarModelName(rc.getConstraintValue())) {
            rc.setConstraintValue(
                    RadarAsGridUtil.getModelNameForIcao(selectedString));
        }
        rc = reqMap.get("icao");
        if (rc != null) {
            rc.setConstraintValue(selectedString);
        }
    }

    @Override
    public Map<String, String[]> getAlterables() {
        Map<String, String[]> alterables = new HashMap<>();

        String site = LocalizationManager.getInstance().getLocalizationStore()
                .getString(LocalizationConstants.P_LOCALIZATION_SITE_NAME);
        SortedSet<String> radars = new TreeSet<>(
                RadarsInUseUtil.getSite(site, RadarsInUseUtil.LOCAL_CONSTANT));
        radars.addAll(
                RadarsInUseUtil.getSite(site, RadarsInUseUtil.DIAL_CONSTANT));
        radars.addAll(
                RadarsInUseUtil.getSite(site, RadarsInUseUtil.ARSR_CONSTANT));
        radars.addAll(
                RadarsInUseUtil.getSite(site, RadarsInUseUtil.ASR_CONSTANT));

        alterables.put(RADAR_KEY, radars.toArray(new String[radars.size()]));

        return alterables;
    }

    @Override
    public void alterBundle(Bundle bundleToAlter, String alterKey,
            String alterValue) {
        if (RADAR_KEY.equals(alterKey)) {
            for (AbstractRenderableDisplay display : bundleToAlter
                    .getDisplays()) {
                alterResourceList(display.getDescriptor().getResourceList(),
                        alterValue);
            }
        }
    }
}
