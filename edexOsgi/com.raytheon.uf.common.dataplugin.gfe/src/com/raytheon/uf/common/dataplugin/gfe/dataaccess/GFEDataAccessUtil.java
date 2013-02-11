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
package com.raytheon.uf.common.dataplugin.gfe.dataaccess;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridParmInfoRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.serialization.comm.RequestRouter;

/**
 * 
 * Some utility methods for querying and retrieving GFE data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GFEDataAccessUtil {

    private static final String PARM_ID_FORMAT = "%s_%s:%s_GRID_%s_%s_%s";

    public static final String PARM_ID = "parmId";

    public static final String DB_ID = "dbId";

    public static final String PLUGIN_NAME = "pluginName";

    public static final String SITE_ID = "siteId";

    public static final String DB_TYPE = "dbType";

    public static final String MODEL_NAME = "modelName";

    public static final String MODEL_TIME = "modelTime";

    public static final String PARM_NAME = "parmName";

    public static final String PARM_LEVEL = "parmLevel";

    /**
     * Retrieve the GridParmInfo for a ParmID
     * 
     * @param parmId
     * @return
     * @throws Exception
     */
    public static GridParmInfo getGridParmInfo(ParmID parmId) throws Exception {
        GetGridParmInfoRequest request = new GetGridParmInfoRequest();
        request.setParmIds(Arrays.asList(parmId));
        request.setSiteID(parmId.getDbId().getSiteId());
        @SuppressWarnings("unchecked")
        ServerResponse<List<GridParmInfo>> response = (ServerResponse<List<GridParmInfo>>) RequestRouter
                .route(request);
        return response.getPayload().get(0);
    }

    /**
     * Send a GetGridDataRequest through the requestRouter to grab a single
     * slice of grid data.
     * 
     * @param gfeRecord
     * @return
     * @throws Exception
     */
    public static IGridSlice getSlice(GFERecord gfeRecord) throws Exception {
        GetGridRequest gridRequest = new GetGridRequest();
        gridRequest.setParmId(gfeRecord.getParmId());
        gridRequest.setRecords(Arrays.asList(gfeRecord));

        GetGridDataRequest request = new GetGridDataRequest();
        request.setSiteID(gfeRecord.getDbId().getSiteId());
        request.setRequests(Arrays.asList(gridRequest));

        @SuppressWarnings("unchecked")
        ServerResponse<List<IGridSlice>> response = (ServerResponse<List<IGridSlice>>) RequestRouter
                .route(request);
        return response.getPayload().get(0);
    }

    /**
     * Take a map of value for various fields that make up the ParmId and
     * convert it into a single like constraint for ParmId. ANd fields not in
     * the map will accept any value.
     * 
     * @param components
     * @return
     */
    public static RequestConstraint createParmIdConstraint(
            Map<String, String> components) {
        String siteId = "%";
        String modelName = "%";
        String modelTime = "%";
        String dbType = "%";
        String parmName = "%";
        String parmLevel = "%";

        if (components.containsKey(SITE_ID)) {
            siteId = components.get(SITE_ID);
        }

        if (components.containsKey(MODEL_NAME)) {
            modelName = components.get(MODEL_NAME);
        }

        if (components.containsKey(MODEL_TIME)) {
            modelTime = components.get(MODEL_TIME);
        }

        if (components.containsKey(DB_TYPE)) {
            dbType = components.get(DB_TYPE);
        }

        if (components.containsKey(PARM_NAME)) {
            parmName = components.get(PARM_NAME);
        }

        if (components.containsKey(PARM_LEVEL)) {
            parmLevel = components.get(PARM_LEVEL);
        }

        String parmId = String.format(PARM_ID_FORMAT, parmName,
                parmLevel, siteId, dbType, modelName, modelTime);
        return new RequestConstraint(parmId, ConstraintType.LIKE);
    }

}
