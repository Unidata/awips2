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
package com.raytheon.uf.edex.pointdata;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.DataTree;
import com.raytheon.uf.common.pointdata.GetPointDataTreeRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.RequestRouter;

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
 * May 10, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GetPointDataTreeHandler implements
        IRequestHandler<GetPointDataTreeRequest> {

    public static final String PLUGIN_NAME = "pluginName";

    @Override
    public DataTree handleRequest(GetPointDataTreeRequest request)
            throws Exception {
        DataTree tree = new DataTree();
        String stationId = Long.toString(LevelFactory.getInstance()
                .getLevel(LevelFactory.UNKNOWN_LEVEL, 0.0).getId());
        for (Entry<String, String> entry : request.getPluginTypeKeyMap()
                .entrySet()) {
            String[] types = getAvailableTypes(entry.getKey(), entry.getValue());
            for (String type : types) {
                String source = entry.getKey();
                if (!PLUGIN_NAME.equals(entry.getValue())) {
                    source += type;
                }
                for (String param : getBaseParams(entry.getKey(),
                        entry.getValue(), type)) {
                    tree.addBranch(source, param, null, null, stationId);
                }
            }
        }
        return tree;
    }

    private String[] getAvailableTypes(String pluginName, String typeKey) {
        if (!typeKey.equals(PLUGIN_NAME)) {
            try {
                DbQueryRequest request = new DbQueryRequest();
                request.addConstraint(PLUGIN_NAME, new RequestConstraint(
                        pluginName));
                request.addRequestField(typeKey);
                request.setDistinct(true);
                DbQueryResponse response = (DbQueryResponse) RequestRouter
                        .route(request);
                String[] result = response.getFieldObjects(typeKey,
                        String.class);
                return result;
            } catch (Exception e) {
                e.printStackTrace();
                return new String[0];
            }
        } else {
            return new String[] { pluginName };
        }
    }

    private List<String> getBaseParams(String pluginName, String typeKey,
            String type) {
        try {
            PointDataQuery query;
            query = new PointDataQuery(pluginName);
            if (!typeKey.equals(PLUGIN_NAME)) {
                query.addParameter(typeKey, type, "=");
            }
            String[] params = query.getAvailableParameters().getValues();
            return Arrays.asList(params);
        } catch (Exception e) {
            e.printStackTrace();
            return Collections.emptyList();
        }
    }

}
