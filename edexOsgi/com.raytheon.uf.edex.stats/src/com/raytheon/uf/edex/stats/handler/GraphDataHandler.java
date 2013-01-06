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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.GraphDataRequest;
import com.raytheon.uf.common.stats.GraphDataResponse;
import com.raytheon.uf.common.stats.data.GraphData;
import com.raytheon.uf.common.stats.xml.StatisticsAggregate;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;
import com.raytheon.uf.common.stats.xml.StatisticsEvent;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.stats.data.StatsDataAccumulator;
import com.raytheon.uf.edex.stats.util.ConfigLoader;

/**
 * Graph Data Request Handler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012   728      mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GraphDataHandler implements IRequestHandler<GraphDataRequest> {
    /** Aggregate Record DAO */
    private final CoreDao dao = new CoreDao(DaoConfig.forClass("metadata",
            AggregateRecord.class));

    private static final String START_DATE = "startDate";

    private static final String END_DATE = "endDate";

    private static final String EVENT_TYPE = "eventType";

    private static final String FIELD = "field";

    private static final String COUNT = "COUNT";

    /**
     * Constructor.
     */
    public GraphDataHandler() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GraphDataResponse handleRequest(GraphDataRequest request)
            throws Exception {
        GraphDataResponse response;

        if (request.isMetaDataRequest()) {
            response = getMetaData();
        } else {
            response = getGraphData(request);
        }

        return response;
    }

    /**
     * Get the statistical configuration objects and add them to the response.
     * 
     * @return GraphDataResponse
     * 
     * @throws Exception
     */
    private GraphDataResponse getMetaData() throws Exception {
        ConfigLoader loader = ConfigLoader.getInstance();
        loader.load();
        List<StatisticsConfig> configList = loader.getConfigurations();
        GraphDataResponse response = new GraphDataResponse();
        response.setConfigList(configList);

        return response;
    }

    /**
     * Get the Graph Data object and add it to the response.
     * 
     * @param request
     *            The request object
     * @return GraphDataResponse
     * @throws Exception
     */
    private GraphDataResponse getGraphData(GraphDataRequest request)
            throws Exception {
        GraphDataResponse response = new GraphDataResponse();
        DatabaseQuery query = new DatabaseQuery(AggregateRecord.class.getName());
        Calendar start = convertToCalendar(request.getTimeRange().getStart());
        Calendar end = convertToCalendar(request.getTimeRange().getEnd());
        query.addQueryParam(START_DATE, start, QueryOperand.GREATERTHANEQUALS);
        query.addQueryParam(END_DATE, end, QueryOperand.LESSTHANEQUALS);

        if (request.getEventType() != null) {
            query.addQueryParam(EVENT_TYPE, request.getEventType(),
                    QueryOperand.EQUALS);
        }

        if (request.getField() != null) {
            query.addQueryParam(FIELD, request.getField(), QueryOperand.EQUALS);
        }

        List<?> results = dao.queryByCriteria(query);

        if (!results.isEmpty()) {
            List<AggregateRecord> arList = new ArrayList<AggregateRecord>();
            for (int i = 0; i < results.size(); i++) {
                if (results.get(i) instanceof AggregateRecord) {
                    arList.add((AggregateRecord) results.get(i));
                }
            }

            String displayUnit = getDisplayUnit(request.getCategory(),
                    request.getEventType(), request.getDataType());

            StatsDataAccumulator accum = new StatsDataAccumulator();
            accum.setRecords(arList.toArray(new AggregateRecord[arList.size()]));
            accum.setDisplayUnits(displayUnit);
            accum.setEventType(request.getEventType());
            accum.setDataType(request.getField());
            accum.setTimeRange(request.getTimeRange());
            accum.setTimeStep(request.getTimeStep());
            accum.setupGroupings();

            GraphData graphData = accum.getGraphData(request.getGrouping());

            response.setGraphData(graphData);
        }
        return response;
    }

    /**
     * Convert a Date object to Calendar object.
     * 
     * @param date
     * @return Calendar object
     */
    private Calendar convertToCalendar(Date date) {
        Calendar cal = TimeUtil.newCalendar(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(date.getTime());

        return cal;
    }

    /**
     * Get the display unit.
     * 
     * @param dataType
     * @param type
     * @param category
     * @return The display unit, NONE if no units
     */
    private String getDisplayUnit(String category, String type, String dataType)
            throws Exception {
        ConfigLoader loader = ConfigLoader.getInstance();
        loader.load();
        List<StatisticsConfig> configList = loader.getConfigurations();

        String unit = COUNT;
        for (StatisticsConfig config : configList) {
            for (String cat : config.getCategories()) {
                if (cat.equals(category)) {
                    for (StatisticsEvent event : config.getEvents()) {
                        if (event.getType().equals(type)) {
                            for (StatisticsAggregate agg : event
                                    .getAggregateList()) {
                                if (agg.getField().equals(dataType)) {
                                    return agg.getDisplayUnit();
                                }
                            }
                        }
                    }
                }
            }
        }

        return unit;
    }

}
