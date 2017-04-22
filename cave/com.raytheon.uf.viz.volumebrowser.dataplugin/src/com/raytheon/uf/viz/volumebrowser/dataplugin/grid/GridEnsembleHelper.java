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
package com.raytheon.uf.viz.volumebrowser.dataplugin.grid;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.grid.inv.VizGridInventory;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.loader.ProductCreator;

/**
 * 
 * Provides static methods to allow multiple {@link ProductCreator} to share the
 * same logic for loading multiple resources when the selected source is an
 * ensemble model.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Initial creation
 * Mar 03, 2016  5439     bsteffen  Rename inventory class
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridEnsembleHelper {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridEnsembleHelper.class);

    public static interface GridProductCreator extends ProductCreator{
        public Collection<ResourcePair> getBaseResourcesToLoad(
                IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry,
                DisplayType displayType);
    }
    
    public static Collection<ResourcePair> getResourcesToLoad(
            GridProductCreator creator, IDataCatalog dataCatalog,
            IDataCatalogEntry catalogEntry, DisplayType displayType) {
        List<String> ensembles = GridEnsembleHelper.getEnsembles(dataCatalog,
                catalogEntry);
        if (ensembles != null) {
            Collection<ResourcePair> full = new ArrayList<ResourcePair>();
            for (String ensemble : ensembles) {
                Collection<ResourcePair> single = creator
                        .getBaseResourcesToLoad(dataCatalog, catalogEntry,
                                displayType);
                GridEnsembleHelper.addEnsemble(single, ensemble);
                full.addAll(single);
            }
            return full;
        } else {
            return creator.getBaseResourcesToLoad(dataCatalog, catalogEntry,
                    displayType);
        }
    }

    private static List<String> getEnsembles(IDataCatalog dataCatalog,
            IDataCatalogEntry catalogEntry) {
        Map<String, RequestConstraint> metadataMap = dataCatalog
                .getProductParameters(catalogEntry);
        List<String> ensembles = null;
        try {
            ensembles = getGridInventory().getEnsembles(metadataMap);
            if (ensembles != null && ensembles.size() > 1) {
                Collections.sort(ensembles);
            } else {
                ensembles = null;
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occured during perturbation query.", e);
        }
        return ensembles;
    }

    private static void addEnsemble(Collection<ResourcePair> resources,
            String ensemble) {
        for (ResourcePair pair : resources) {
            AbstractRequestableResourceData data = ((AbstractRequestableResourceData) pair
                    .getResourceData());
            data.getMetadataMap().put(GridConstants.ENSEMBLE_ID,
                    new RequestConstraint(ensemble.toString()));
        }
    }

    private static VizGridInventory getGridInventory() {
        return (VizGridInventory) DataCubeContainer
                .getInventory(GridConstants.GRID);
    }
}
