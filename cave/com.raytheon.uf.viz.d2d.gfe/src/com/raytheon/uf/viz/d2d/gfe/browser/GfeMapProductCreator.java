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
package com.raytheon.uf.viz.d2d.gfe.browser;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.gfe.dataaccess.GFEDataAccessUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.d2d.gfe.rsc.GFEGridResourceData;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.loader.AbstractMapProductCreator;

/**
 * 
 * Creates {@link GFEGridResourceData} for use on a {@link MapRenderableDisplay}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Initial Creation
 * May 25, 2018  6609     njensen   Overrode getResourcesToLoad() to ensure
 *                                  Fcst db has correct dbType set
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GfeMapProductCreator extends AbstractMapProductCreator {

    @Override
    protected GFEGridResourceData createNewResourceData(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry, DisplayType displayType) {
        GFEGridResourceData data = new GFEGridResourceData();
        data.setLegendString(dataCatalog.getName(catalogEntry, displayType));
        return data;
    }

    @Override
    protected Collection<ResourcePair> getResourcesToLoad(
            IDataCatalog dataCatalog, IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        GFEGridResourceData resourceData = createNewResourceData(
                dataCatalog, catalogEntry, displayType);
        HashMap<String, RequestConstraint> metadataMap = dataCatalog
                .getProductParameters(catalogEntry);
        String modelName = metadataMap.get(GFEDataAccessUtil.MODEL_NAME)
                .getConstraintValue();
        if (GFEDataAccessUtil.FCST.equals(modelName)) {
            /*
             * If they selected the Fcst db, we need to make sure we get the
             * right Fcst data since there's three different dbIds (operational,
             * practice, test) with that name per site.
             */
            String dbtype = GFEProductBrowserDataListing.getDbType();
            metadataMap.put(GFEDataAccessUtil.DB_TYPE,
                    new RequestConstraint(dbtype));
            // empty string is for operational/default
            if (!dbtype.isEmpty()) {
                modelName = dbtype + " " + modelName;
            }
            String newLegend = resourceData.getLegendString().replace("GFE",
                    "GFE " + modelName);
            resourceData.setLegendString(newLegend);
        }
        resourceData.setMetadataMap(metadataMap);

        ResourcePair pair = new ResourcePair();
        pair.setResourceData(resourceData);
        pair.setLoadProperties(createNewLoadProperties(displayType));
        pair.setProperties(new ResourceProperties());
        return Arrays.asList(pair);
    }

}
