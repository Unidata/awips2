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
package com.raytheon.uf.viz.d2d.gfe;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridParmInfoRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;

/**
 * 
 * TODO Add Description
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
public class GFEUtil {

    public static final String PARM_ID_FORMAT = "%s_%s:%s_GRID_%s_%s_%s";

    public static final String PARM_ID = "parmId";

    public static final String PLUGIN_NAME = "pluginName";

    public static GridParmInfo getGridParmInfo(ParmID parmId)
            throws VizException {
        GetGridParmInfoRequest request = new GetGridParmInfoRequest();
        request.setParmIds(Arrays.asList(parmId));
        request.setSiteID(parmId.getDbId().getSiteId());
        request.setWorkstationID(VizApp.getWsId());
        @SuppressWarnings("unchecked")
        ServerResponse<List<GridParmInfo>> response = (ServerResponse<List<GridParmInfo>>) ThriftClient
                .sendRequest(request);
        return response.getPayload().get(0);
    }

    public static ParamLevelMatchCriteria getMatchCriteria(ParmID parmId) {
        ParamLevelMatchCriteria criteria = new ParamLevelMatchCriteria();
        criteria.setParameterName(new ArrayList<String>());
        criteria.setLevels(new ArrayList<Level>());
        criteria.setCreatingEntityNames(new ArrayList<String>());
        String parameter = "GFE:" + parmId.getParmName();
        SingleLevel level = new SingleLevel(Level.LevelType.SURFACE);
        String model = "GFE:" + parmId.getDbId().getModelName();
        if (!criteria.getParameterNames().contains(parameter)) {
            criteria.getParameterNames().add(parameter);
        }
        if (!criteria.getLevels().contains(level)) {
            criteria.getLevels().add(level);
        }
        if (!criteria.getCreatingEntityNames().contains(model)) {
            criteria.getCreatingEntityNames().add(model);
        }
        return criteria;
    }

    public static IGridSlice getSlice(GFERecord gfeRecord) throws VizException {
        GetGridRequest gridRequest = new GetGridRequest();
        gridRequest.setParmId(gfeRecord.getParmId());
        gridRequest.setRecords(Arrays.asList(gfeRecord));

        GetGridDataRequest request = new GetGridDataRequest();
        request.setSiteID(gfeRecord.getDbId().getSiteId());
        request.setWorkstationID(VizApp.getWsId());
        request.setRequests(Arrays.asList(gridRequest));

        @SuppressWarnings("unchecked")
        ServerResponse<List<IGridSlice>> response = (ServerResponse<List<IGridSlice>>) ThriftClient
                .sendRequest(request);
        return response.getPayload().get(0);
    }

}
