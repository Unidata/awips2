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
package com.raytheon.uf.edex.stats.handler;

import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.AggregatedStatsRequest;
import com.raytheon.uf.common.stats.AggregatedStatsResponse;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Handles an AggregateStatsRequest and queries the metadata.aggregate table and
 * return the results in an AggregateStatsResponse
 * 
 *  * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez     Initial creation.
 * 
 * </pre>
 * 
 * @author jsanchez
 * 
 */
public class AggregatedStatsHandler implements
        IRequestHandler<AggregatedStatsRequest> {

    private CoreDao dao = new CoreDao(DaoConfig.forClass("metadata",
            AggregateRecord.class));

    @Override
    public AggregatedStatsResponse handleRequest(AggregatedStatsRequest request)
            throws Exception {
        DatabaseQuery query = new DatabaseQuery(AggregateRecord.class.getName());

        // TODO Add a time range implementation

        if (request.getEventType() != null) {
            query.addQueryParam("eventType", request.getEventType(),
                    QueryOperand.EQUALS);
        }

        if (request.getGrouping() != null) {
            StringBuffer grouping = new StringBuffer();
            for (String group : request.getGrouping()) {
                if (grouping.length() > 0) {
                    grouping.append("-");
                }
                grouping.append(group);
            }
            query.addQueryParam("grouping", grouping, QueryOperand.EQUALS);
        }

        if (request.getField() != null) {
            query.addQueryParam("field", request.getField(),
                    QueryOperand.EQUALS);
        }

        List<?> results = dao.queryByCriteria(query);
        AggregateRecord[] records = new AggregateRecord[results.size()];
        for (int i = 0; i < results.size(); i++) {
            if (results.get(i) instanceof AggregateRecord) {
                records[i] = (AggregateRecord) results.get(i);
            }
        }

        AggregatedStatsResponse response = new AggregatedStatsResponse(
                request.getTimeRange(), request.getEventType(),
                request.getGrouping(), request.getField());
        response.setRecords(records);

        return response;
    }
}
