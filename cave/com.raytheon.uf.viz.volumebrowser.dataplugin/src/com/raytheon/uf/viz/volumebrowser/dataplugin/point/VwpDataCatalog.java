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
package com.raytheon.uf.viz.volumebrowser.dataplugin.point;

import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;

/**
 * 
 * Catalog for radar VWP data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Dec 01, 2009           bsteffen  Initial creation
 * Aug 03, 2015  3861     bsteffen  Move resource creation to ProductCreators
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class VwpDataCatalog extends PointDataCatalog {

    @Override
    protected String[] getPlugins() {
        return new String[] { "radar" };
    }

    @Override
    protected SurfaceObsLocation[] getStationLocations(String sourceKey) {
        if (availableStations.containsKey(sourceKey)) {
            return availableStations.get(sourceKey);
        }
        DbQuery query = new DbQuery(getDefaultPlugin());
        query.setDistinctField("icao");
        query.addColumn("location.lat");
        query.addColumn("location.lon");
        query.addOrderBy("location.lat");
        try {
            List<Object[]> result = query.performQuery();
            SurfaceObsLocation[] locs = new SurfaceObsLocation[result.size()];
            for (int i = 0; i < result.size(); i++) {
                String icao = (String) result.get(i)[0];
                Float lat = (Float) result.get(i)[1];
                Float lon = (Float) result.get(i)[2];
                locs[i] = new SurfaceObsLocation(icao);
                locs[i].assignLocation(lat, lon);
            }
            availableStations.put(sourceKey, locs);
            return locs;
        } catch (VizException e) {
            return null;
        }
    }

    @Override
    protected void addProductParameters(IDataCatalogEntry catalogEntry,
            HashMap<String, RequestConstraint> productParameters) {
        String sourceKey = catalogEntry.getSelectedData().getSourcesKey();
        productParameters.put("pluginName", new RequestConstraint(
                getPlugin(sourceKey)));
        addLineOrPointStationParameter(catalogEntry, productParameters, "icao");
        productParameters.put("productCode", new RequestConstraint("48"));
    }


}
