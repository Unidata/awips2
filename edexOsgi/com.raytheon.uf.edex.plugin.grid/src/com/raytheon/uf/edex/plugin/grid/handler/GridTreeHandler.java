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
package com.raytheon.uf.edex.plugin.grid.handler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.grid.request.GetGridTreeRequest;
import com.raytheon.uf.common.derivparam.tree.DataTree;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Build a DataTree representing all the grid data in the db.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridTreeHandler implements IRequestHandler<GetGridTreeRequest> {

    private static final String[] GRIDFIELDS = { GridInfoConstants.DATASET_ID,
            GridInfoConstants.PARAMETER_ABBREVIATION,
            GridInfoConstants.PARAMETER_NAME, GridInfoConstants.PARAMETER_UNIT,
            GridInfoConstants.LEVEL_ID };

    @Override
    public DataTree handleRequest(GetGridTreeRequest request) throws Exception {
        DataTree gridTree = null;
        CoreDao gribDao = null;
        List<?> queryResults = null;
        gribDao = new CoreDao(DaoConfig.forClass(GridInfoRecord.class));

        // if we do not get a table back, just return an empty list
        gridTree = new DataTree();
        DatabaseQuery query = new DatabaseQuery(GridInfoRecord.class.getName());

        List<String> distinctFields = Arrays.asList(GRIDFIELDS);

        query.addOrder(distinctFields.get(0), true);
        query.addDistinctParameter(distinctFields);
        queryResults = gribDao.queryByCriteria(query);
        if (queryResults.size() > 0) {
            for (Object gridField : queryResults) {
                if (gridField.getClass().isArray()) {
                    ArrayList<Object> gridFields = new ArrayList<Object>(
                            Arrays.asList((Object[]) gridField));
                    String model = gridFields.get(0).toString();
                    gridTree.addBranch(model, getDt(model), gridFields.get(1)
                            .toString(), gridFields.get(2).toString(),
                            gridFields.get(3).toString(), gridFields.get(4)
                                    .toString());
                }
            }
        }
        return gridTree;
    }

    private int getDt(String modelName) {
        DatasetInfo info = DatasetInfoLookup.getInstance().getInfo(modelName);
        if (info != null && info.getDt() != null) {
            int dTinSeconds = info.getDt();

            // dT <= 24 is in hours, need to convert to seconds
            if (Math.abs(dTinSeconds) <= 24) {
                dTinSeconds *= 3600;
            }
            return dTinSeconds;
        }
        return -1;
    }

}
