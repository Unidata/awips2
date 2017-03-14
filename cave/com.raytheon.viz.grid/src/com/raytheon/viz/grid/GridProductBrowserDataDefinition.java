package com.raytheon.viz.grid;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.datalisting.DataListing;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.GridLoadProperties;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference.PreferenceType;
import com.raytheon.uf.viz.productbrowser.datalisting.DataListingProductBrowserDefinition;
import com.raytheon.uf.viz.productbrowser.pref.ProductBrowserPreferenceListener;
import com.raytheon.viz.grid.inv.VizGridInventory;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.grid.xml.FieldDisplayTypesFactory;

/**
 * Product browser implementation for grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 21, 2010           bsteffen  Initial creation
 * May 26, 2010  6305     mnash     Used ProductBrowserLabel implementation
 *                                  instead of requery
 * May 02, 2013  1949     bsteffen  Switch Product Browser from uengine to
 *                                  DbQueryRequest.
 * Sep 19, 2013  2391     mpduff    refactored some methods to common class.
 * Jan 23, 2014  2711     bsteffen  Get all levels from LevelFactory.
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * Jun 09, 2015  4153     bsteffen  Switch to use a datalisting.
 * Mar 03, 2016  5439     bsteffen  Rename inventory class
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridProductBrowserDataDefinition extends DataListingProductBrowserDefinition {

    private static final String SHOW_DERIVED_PARAMS = "Show Derived Parameters";

    public GridProductBrowserDataDefinition() {
        super(new DerivedPreferenceDataListing());
        if (listing instanceof DerivedPreferenceDataListing) {
            new ProductBrowserPreferenceListener(displayName,
                    ((DerivedPreferenceDataListing) listing).getDerivedPreference());
        }
    }

    @Override
    protected GridResourceData createResourceData(Map<String, String> keyVals) {
        GridResourceData resourceData = new GridResourceData();
        Map<String, RequestConstraint> constraints = listing.getRequestConstraints(keyVals);
        resourceData.setMetadataMap(new HashMap<>(constraints));
        return resourceData;
    }

    @Override
    protected ResourcePair createResourcePair(AbstractResourceData resourceData, DisplayType displayType) {
        ResourcePair pair = new ResourcePair();
        pair.setResourceData(resourceData);
        GridLoadProperties loadProperties = new GridLoadProperties();
        if (displayType != null) {
            loadProperties.setDisplayType(displayType);
        }
        pair.setLoadProperties(loadProperties);
        pair.setProperties(new ResourceProperties());
        return pair;
    }

    @Override
    protected AbstractRenderableDisplay createRenderableDisplay(ResourcePair resourcePair) throws VizException {
        AbstractResourceData resourceData = resourcePair.getResourceData();
        if (resourceData instanceof GridResourceData) {
            VizGridInventory inventory = (VizGridInventory) DataCubeContainer
                    .getInventory(GridConstants.GRID);
            GridResourceData gridResourceData = (GridResourceData) resourceData;
            List<String> ensembles = inventory.getEnsembles(gridResourceData.getMetadataMap());
            if (ensembles != null && ensembles.size() > 1) {
                Collections.sort(ensembles);
                DisplayType displayType = null;
                if (resourcePair.getLoadProperties() instanceof GridLoadProperties) {
                    displayType = ((GridLoadProperties) resourcePair.getLoadProperties()).getDisplayType();
                }
                AbstractRenderableDisplay display = null;
                for (String ensemble : ensembles) {
                    GridResourceData ensembleResourceData = new GridResourceData();
                    HashMap<String, RequestConstraint> newParameters = new HashMap<String, RequestConstraint>(
                            gridResourceData.getMetadataMap());
                    newParameters.put(GridConstants.ENSEMBLE_ID, new RequestConstraint(ensemble));
                    ensembleResourceData.setMetadataMap(newParameters);
                    ResourcePair ensemblePair = createResourcePair(gridResourceData, displayType);
                    if (display == null) {
                        display = createRenderableDisplay(ensemblePair);
                    } else {
                        display.getDescriptor().getResourceList().add(ensemblePair);
                    }
                }
                return display;
            }
        }
        return super.createRenderableDisplay(resourcePair);
    }

    @Override
    public Collection<DisplayType> getValidDisplayTypes(String[] selection) {
        Map<String, String> keyValMap = createKeyValMap(selection);
        if (keyValMap.containsKey(GridConstants.PARAMETER_ABBREVIATION)) {
            return FieldDisplayTypesFactory.getInstance().getDisplayTypes(
                    keyValMap.get(GridConstants.PARAMETER_ABBREVIATION));
        }
        return super.getValidDisplayTypes(selection);
    }

    @Override
    public void loadResource(String[] selection, DisplayType displayType) {
        if (displayType == null) {
            Collection<DisplayType> types = getValidDisplayTypes(selection);
            if (types != null && !types.isEmpty()) {
                displayType = types.iterator().next();
            }
        }
        super.loadResource(selection, displayType);
    }

    @Override
    public List<ProductBrowserPreference> getPreferences() {
        List<ProductBrowserPreference> prefs = new ArrayList<>(super.getPreferences());
        if (listing instanceof DerivedPreferenceDataListing) {
            prefs.add(((DerivedPreferenceDataListing) listing).getDerivedPreference());
        }
        return prefs;
    }

    private static class DerivedPreferenceDataListing implements DataListing {

        private final ProductBrowserPreference derivedPreference;

        private final DataListing listing;

        private final DataListing derivedListing;

        public DerivedPreferenceDataListing() {
            derivedPreference = new ProductBrowserPreference();
            derivedPreference.setLabel(SHOW_DERIVED_PARAMS);
            derivedPreference.setPreferenceType(PreferenceType.BOOLEAN);
            derivedPreference.setTooltip("Show derived parameters in the Product Browser");
            derivedPreference.setValue(true);
            this.listing = new GridDataListing(Arrays.asList(GridConstants.DATASET_ID,
                    GridConstants.PARAMETER_ABBREVIATION, GridConstants.MASTER_LEVEL_NAME, GridConstants.LEVEL_ID));
            this.derivedListing = new DerivedGridDataListing(Arrays.asList(GridConstants.DATASET_ID,
                    GridConstants.PARAMETER_ABBREVIATION, GridConstants.MASTER_LEVEL_NAME, GridConstants.LEVEL_ID));
        }

        public ProductBrowserPreference getDerivedPreference() {
            return derivedPreference;
        }

        private DataListing getListing() {
            if (Boolean.TRUE.equals(derivedPreference.getValue())) {
                return derivedListing;
            } else {
                return listing;
            }
        }

        @Override
        public String getPluginName() {
            return getListing().getPluginName();
        }

        @Override
        public Collection<String> getKeys() {
            return getListing().getKeys();
        }

        @Override
        public Collection<String> getValues(String key, Map<String, String> keyVals) throws Exception {
            return getListing().getValues(key, keyVals);
        }

        @Override
        public Map<String, String> getFormattedValues(String key, Map<String, String> keyVals) throws Exception {
            return getListing().getFormattedValues(key, keyVals);
        }

        @Override
        public Map<String, RequestConstraint> getRequestConstraints(Map<String, String> keyVals) {
            return getListing().getRequestConstraints(keyVals);
        }

    }

}
