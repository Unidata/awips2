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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.datalisting.DataListing;
import com.raytheon.uf.common.datalisting.impl.DefaultDataListing;
import com.raytheon.uf.common.dataplugin.gfe.dataaccess.GFEDataAccessUtil;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmStorageInfo;
import com.raytheon.uf.common.dataplugin.gfe.request.GetActiveSitesRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * {@link DataListing} for GFE data in the Product Browser. It will remove
 * non-active GFE sites, database ids of type EditTopo, and parms of type
 * discrete and weather. If the model name is Fcst, it will constrain a dbType
 * of "", "Prac", or "Test" based on the CAVE mode.
 *
 * Note: This is not a "true" data listing providing listings of all available
 * data in GFE. This implementation removes GFE data that cannot be displayed in
 * D2D.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2018 6609       njensen     Initial creation
 * Dec 02, 2019 71896      tjensen     Add support for Discrete products
 * Dec 13, 2019 72475      tjensen     Add support for Weather products
 *
 * </pre>
 *
 * @author njensen
 */
public class GFEProductBrowserDataListing extends DefaultDataListing {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEProductBrowserDataListing.class);

    private static final String INFO_PREFIX = "gridParmInfo.";

    private static final String INFO_GRID_TYPE = INFO_PREFIX + "gridType";

    private static final String INFO_PARM_ID = INFO_PREFIX + "parmID.";

    private static final String INFO_PARM_NAME = INFO_PARM_ID + "parmName";

    private static final String INFO_DB_ID = INFO_PARM_ID + "dbId.";

    private static final String INFO_TIME_INDEPENDENT = INFO_PREFIX
            + "timeIndependentParm";

    public GFEProductBrowserDataListing(String pluginName,
            List<String> keySet) {
        super(pluginName, keySet);
    }

    @Override
    public Collection<String> getValues(String key, Map<String, String> keyVals)
            throws Exception {
        // add in dbType if the keyVals include Fcst as modelName
        checkForFcstDb(keyVals);

        // do the actual query of available data
        Collection<String> values = super.getValues(key, keyVals);

        // remove data that can't be displayed.
        if (GFEDataAccessUtil.SITE_ID.equals(key)) {
            /*
             * We've got the list of sites with data actually in the system. But
             * we need to filter out sites that not active because GFE does not
             * support requesting data from an inactive site.
             */
            retainActiveSites(values);
        } else if (GFEDataAccessUtil.MODEL_NAME.equals(key)) {
            /*
             * We've got the list of model names with data actually in the
             * system. But we need to filter out Topo models because there are
             * no displayable parms in it.
             */
            retainNonTopoModels(values);
        } else if (GFEDataAccessUtil.PARM_NAME.equals(key)) {
            /*
             * We've got the list of parms with data actually in the system. But
             * we need to filter out discrete and weather types since those
             * cannot be displayed outside of GFE. Also need to filter out time
             * independent parms because AbstractGridResource does not support
             * time agnostic grids.
             */
            retainSupportedGridTypes(keyVals, values);
        }

        return values;
    }

    /**
     * Removes sites that aren't active (for GFE) from the collection passed in
     * as an argument
     *
     * @param sitesWithData
     *            the collection that will potentially be shrunk to only include
     *            data for active GFE sites
     */
    private void retainActiveSites(Collection<String> sitesWithData) {
        GetActiveSitesRequest request = new GetActiveSitesRequest();
        try {
            ServerResponse<?> sr = (ServerResponse<?>) ThriftClient
                    .sendRequest(request);
            if (sr.isOkay()) {
                Collection<String> activeSites = (Collection<String>) sr
                        .getPayload();
                sitesWithData.retainAll(activeSites);
            } else {
                statusHandler.warn(sr.message());
                sitesWithData.clear();
            }
        } catch (VizException e) {
            statusHandler.error("Unable to determine active GFE sites", e);
            sitesWithData.clear();
        }
    }

    /**
     * Removes models that are of dbType EditTopo from the collection passed in
     * as an argument
     *
     * @param modelNames
     *            the collection that will potentially be shrunk to only include
     *            data for non-topo models
     */
    private void retainNonTopoModels(Collection<String> modelNames) {
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(DatabaseID.class);
        request.addRequestField("modelName");
        request.addConstraint("dbType",
                new RequestConstraint("EditTopo", ConstraintType.NOT_EQUALS));
        request.setDistinct(true);

        try {
            DbQueryResponse response = (DbQueryResponse) RequestRouter
                    .route(request);
            List<Map<String, Object>> resultList = response.getResults();
            List<String> filteredModels = new ArrayList<>(resultList.size());
            for (Map<String, Object> modelResult : resultList) {
                filteredModels.add((String) modelResult.get("modelName"));
            }
            modelNames.retainAll(filteredModels);
        } catch (Exception e) {
            /*
             * Failed to remove topo models, but continue on with the model
             * names as it will just be a tree leaf (no sub-nodes).
             */
            statusHandler.error("Unable to determine which models are topo", e);
        }
    }

    /**
     * Removes parmNames that aren't grid type scalar or vector or time
     * independent parms from the second argument
     *
     * @param keyVals
     *            the items that will become request constraints to limit the
     *            parms
     * @param parmNames
     *            the collection that will potentially be shrunk to only include
     *            scalar and vector parm names
     */
    private void retainSupportedGridTypes(Map<String, String> keyVals,
            Collection<String> parmNames) {
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(ParmStorageInfo.class);
        request.addRequestField(INFO_GRID_TYPE);
        request.addRequestField(INFO_PARM_NAME);
        request.addRequestField(INFO_TIME_INDEPENDENT);
        request.setDistinct(true);
        /*
         * The capital ID in gridParmInfo.parmID forces us to manipulate the
         * constraints and replace parmId with parmID. Many of the other queries
         * are querying against the entity class GFERecord while we need to
         * query against the entity class ParmStorageInfo to get both the
         * parmName and the gridType in one query.
         */
        for (Entry<String, String> entry : keyVals.entrySet()) {
            String constraintKey = INFO_PREFIX
                    + entry.getKey().replace("parmId", "parmID");
            request.addConstraint(constraintKey,
                    new RequestConstraint(entry.getValue()));
        }

        // only request those parms that have data
        request.addConstraint(INFO_PARM_NAME, new RequestConstraint(
                String.join(",", parmNames), ConstraintType.IN));

        try {
            DbQueryResponse response = (DbQueryResponse) RequestRouter
                    .route(request);
            List<Map<String, Object>> resultList = response.getResults();
            List<String> filteredNames = new ArrayList<>(resultList.size());

            for (Map<String, Object> parmResult : resultList) {
                GridType gridType = (GridType) parmResult.get(INFO_GRID_TYPE);
                switch (gridType) {
                case SCALAR:
                case VECTOR:
                case DISCRETE:
                case WEATHER:
                    boolean timeIndependent = (Boolean) parmResult
                            .get(INFO_TIME_INDEPENDENT);
                    if (!timeIndependent) {
                        filteredNames
                                .add(parmResult.get(INFO_PARM_NAME).toString());
                    }
                    break;
                }
            }
            parmNames.retainAll(filteredNames);
        } catch (Exception e) {
            statusHandler.error("Unable to determine grid types of parms", e);
            parmNames.clear();
        }
    }

    /**
     * Gets the valid display types for a particular parm at a specific site,
     * level, and model
     *
     * @param keyValMap
     *            constraints used to find the valid display types
     * @return the valid display types for the parm in the Map argument
     */
    public Collection<DisplayType> getValidDisplayTypes(
            Map<String, String> keyValMap) {
        String siteID = keyValMap.get(GFEDataAccessUtil.SITE_ID);
        String modelName = keyValMap.get(GFEDataAccessUtil.MODEL_NAME);
        String parmName = keyValMap.get(GFEDataAccessUtil.PARM_NAME);
        String parmLevel = keyValMap.get(GFEDataAccessUtil.PARM_LEVEL);

        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(ParmStorageInfo.class);
        request.addRequestField(INFO_GRID_TYPE);
        request.addConstraint(INFO_DB_ID + "siteId",
                new RequestConstraint(siteID));
        request.addConstraint(INFO_DB_ID + "modelName",
                new RequestConstraint(modelName));
        request.addConstraint(INFO_PARM_NAME, new RequestConstraint(parmName));
        request.addConstraint(INFO_PARM_ID + "parmLevel",
                new RequestConstraint(parmLevel));
        if (GFEDataAccessUtil.FCST.equals(modelName)) {
            request.addConstraint(INFO_DB_ID + "dbType",
                    new RequestConstraint(getDbType()));
        }

        DbQueryResponse response = null;
        try {
            response = (DbQueryResponse) RequestRouter.route(request);
        } catch (Exception e) {
            statusHandler.debug(
                    "Unable to determine display types for parm " + parmName,
                    e);
            return Collections.emptyList();
        }
        List<Map<String, Object>> resultList = response.getResults();
        if (!resultList.isEmpty()) {
            Collection<DisplayType> displayTypes = new ArrayList<>(2);
            GridType gridType = (GridType) resultList.get(0)
                    .get(INFO_GRID_TYPE);
            switch (gridType) {
            case SCALAR:
                displayTypes.add(DisplayType.CONTOUR);
                displayTypes.add(DisplayType.IMAGE);
                break;
            case VECTOR:
                displayTypes.add(DisplayType.BARB);
                displayTypes.add(DisplayType.ARROW);
                break;
            case DISCRETE:
            case WEATHER:
                displayTypes.add(DisplayType.IMAGE);
                break;
            }
            return displayTypes;
        }

        return Collections.emptyList();
    }

    /**
     * Checks if the constraints include modelName and if the model name is
     * Fcst, and if so adds the appropriate dbType to the Map of constraints
     * passed in as an argument.
     *
     * @param keyVals
     *            constraints that may potentially have dbType added to them
     */
    protected void checkForFcstDb(Map<String, String> keyVals) {
        if (keyVals.containsKey(GFEDataAccessUtil.MODEL_NAME)
                && GFEDataAccessUtil.FCST
                        .equals(keyVals.get(GFEDataAccessUtil.MODEL_NAME))) {
            /*
             * If they selected the Fcst db, we need to make sure we get the
             * right Fcst data since there's three different dbIds (operational,
             * practice, test) with that name per site.
             */
            keyVals.put(GFEDataAccessUtil.DB_TYPE, getDbType());
        }
    }

    /**
     * Gets the dbType for the Fcst database based on the CAVE mode
     *
     * @return the string representing the dbType for the Fcst database
     */
    protected static String getDbType() {
        // operational is empty string
        String dbtype = "";
        CAVEMode mode = CAVEMode.getMode();
        switch (mode) {
        case TEST:
            dbtype = "Test";
            break;
        case PRACTICE:
            dbtype = "Prac";
            break;
        }
        return dbtype;
    }

}
