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

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.GraphDataRequest;
import com.raytheon.uf.common.stats.GraphDataResponse;
import com.raytheon.uf.common.stats.data.GraphData;
import com.raytheon.uf.common.stats.xml.StatisticsAggregate;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;
import com.raytheon.uf.common.stats.xml.StatisticsEventConfig;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.stats.dao.AggregateRecordDao;
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
 * Sep 11, 2012 728        mpduff      Initial creation
 * Jan 07, 2013 1451       djohnson    Use newGmtCalendar().
 * May 22, 2013 1917       rjpeter     Renamed StatisticsEvent to StatisticsEventConfig.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Transactional
public class GraphDataHandler implements IRequestHandler<GraphDataRequest> {
    /** Aggregate Record DAO */
    private AggregateRecordDao aggregateRecordDao;

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
        List<Object> params = new ArrayList<Object>();
        StringBuffer query = new StringBuffer();
        query.append("from AggregateRecord rec where rec.startDate >= :startDate and rec.endDate <= :endDate");
        GraphDataResponse response = new GraphDataResponse();
        Calendar start = convertToCalendar(request.getTimeRange().getStart());
        Calendar end = convertToCalendar(request.getTimeRange().getEnd());
        params.add(START_DATE);
        params.add(start);
        params.add(END_DATE);
        params.add(end);

        if (request.getEventType() != null) {
            query.append(" and rec.eventType = :eventType");
            params.add(EVENT_TYPE);
            params.add(request.getEventType());
        }

        if (request.getField() != null) {
            query.append(" and rec.field = :field");
            params.add(FIELD);
            params.add(request.getField());
        }

        List<?> results = aggregateRecordDao.executeHQLQuery(query.toString(),
                params.toArray(new Object[params.size()]));

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
        Calendar cal = TimeUtil.newGmtCalendar();
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
                    for (StatisticsEventConfig event : config.getEvents()) {
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

    public void setAggregateRecordDao(AggregateRecordDao aggregateRecordDao) {
        this.aggregateRecordDao = aggregateRecordDao;
    }

}
