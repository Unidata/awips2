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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridParmInfoRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
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
 * May 02, 2013 1949      bsteffen     Update GFE data access in Product
 *                                     Browser, Volume Browser, and Data Access
 *                                     Framework.
 * Jul 01, 2014 3149      randerso     Changed to use updated GetGridRequest
 * Dec 15, 2016 6040      tgurney      Added DB_TYPE constant
 * Sep 27, 2017 6463      tgurney      Check response.isOkay before getting payload
 * May 25, 2018 6609      njensen      Added constant FCST
 *
 * </pre>
 *
 * @author bsteffen
 */
public class GFEDataAccessUtil {

    public static final String PARM_ID = "parmId";

    public static final String DB_ID = PARM_ID + ".dbId";

    public static final String SITE_ID = DB_ID + ".siteId";

    public static final String MODEL_NAME = DB_ID + ".modelName";

    public static final String MODEL_TIME = DB_ID + ".modelTime";

    public static final String PARM_NAME = PARM_ID + ".parmName";

    public static final String PARM_LEVEL = PARM_ID + ".parmLevel";

    public static final String DB_TYPE = DB_ID + ".dbType";

    public static final String FCST = "Fcst";

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
        if (!response.isOkay()) {
            throw new GfeException(response.message());
        }
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
        GetGridRequest gridRequest = new GetGridRequest(gfeRecord.getParmId(),
                Arrays.asList(gfeRecord.getDataTime().getValidPeriod()));

        GetGridDataRequest request = new GetGridDataRequest();
        request.setSiteID(gfeRecord.getDbId().getSiteId());
        request.setRequests(Arrays.asList(gridRequest));

        @SuppressWarnings("unchecked")
        ServerResponse<List<IGridSlice>> response = (ServerResponse<List<IGridSlice>>) RequestRouter
                .route(request);
        if (!response.isOkay()) {
            throw new GfeException(response.message());
        }
        return response.getPayload().get(0);
    }

}
