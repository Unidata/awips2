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
package com.raytheon.uf.edex.stats.util;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.stats.xml.Aggregate;
import com.raytheon.uf.edex.stats.xml.GroupBy;
import com.raytheon.uf.edex.stats.xml.Item;
import com.raytheon.uf.edex.stats.xml.Statistics;

/**
 * Archives the data in the aggregate_bucket table to an xml file.
 * 
 * <pre>
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
public class Archiver {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Archiver.class);

    private class StatisticsKey {
        public String eventType;

        public String grouping;

        public TimeRange timeRange;

        @Override
        public boolean equals(Object o) {
            if (o != null && o instanceof StatisticsKey) {
                StatisticsKey other = (StatisticsKey) o;

                return (this.eventType.equals(other.eventType)
                        && this.timeRange.getStart().equals(
                                other.timeRange.getStart()) && this.timeRange
                        .getEnd().equals(other.timeRange.getEnd()));
            }

            return false;
        }

        @Override
        public int hashCode() {
            return 1;
        }
    }

    /** Marshaller object */
    private Marshaller marshaller;

    /** JAXB context */
    private JAXBContext jax;

    private IPathManager pm = PathManagerFactory.getPathManager();

    private LocalizationContext context = pm.getContext(
            LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

    private SimpleDateFormat dateFormatter = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    private SimpleDateFormat fileDateFormatter = new SimpleDateFormat(
            "yyyyMMdd_HHmm");

    public Archiver() throws JAXBException {
        jax = JAXBContext.newInstance(new Class[] { Statistics.class });
        this.marshaller = jax.createMarshaller();
    }

    /**
     * Writes the statistics xml to disk.
     * 
     * @param statistics
     * @throws JAXBException
     */
    public void writeToDisk(String filename, Statistics statistics)
            throws JAXBException {
        LocalizationFile siteLocalization = pm.getLocalizationFile(context,
                filename);
        marshaller.marshal(statistics, siteLocalization.getFile());
    }

    /**
     * Writes the aggregate records to disk.
     * 
     * @param aggregateRecords
     * @throws JAXBException
     */
    public void writeToDisk(AggregateRecord[] aggregateRecords) {

        Map<StatisticsKey, List<AggregateRecord>> statisticsMap = new HashMap<StatisticsKey, List<AggregateRecord>>();

        for (AggregateRecord record : aggregateRecords) {
            StatisticsKey key = new StatisticsKey();
            key.eventType = record.getEventType();
            key.grouping = record.getGrouping();
            key.timeRange = new TimeRange(record.getStartDate(),
                    record.getEndDate());

            List<AggregateRecord> aggregateRecordList = statisticsMap.get(key);
            if (aggregateRecordList == null) {
                aggregateRecordList = new ArrayList<AggregateRecord>();
                statisticsMap.put(key, aggregateRecordList);
            }

            aggregateRecordList.add(record);
        }

        for (StatisticsKey key : statisticsMap.keySet()) {
            Statistics statistics = new Statistics();
            statistics.setEventType(key.eventType);
            statistics.setStart(dateFormatter.format(key.timeRange.getStart()));
            statistics.setEnd(dateFormatter.format(key.timeRange.getEnd()));
            statistics.setGroupBy(createGroupBy(key.grouping));
            statistics.setAggregates(createAggregates(statisticsMap.get(key)));

            String filename = createFilename(key.timeRange, statistics);
            try {
                writeToDisk(filename, statistics);
            } catch (JAXBException e) {
                statusHandler.error("Unable to write statistics file "
                        + filename, e);
            }
        }

    }

    /**
     * Creates a filename in the format
     * /stats/aggregates/groupBy{0}/groupby{1}...
     * /group{n}/eventType.start-end.dat
     * 
     * @param items
     * @return
     */
    private String createFilename(TimeRange tr, Statistics statistics) {
        StringBuffer sb = new StringBuffer("stats/aggregates");
        for (Item item : statistics.getGroupBy().getAttributes()) {
            sb.append("/" + item.getResult());
        }
        sb.append("/" + statistics.getEventType() + "."
                + fileDateFormatter.format(tr.getStart()) + "-"
                + fileDateFormatter.format(tr.getEnd()) + ".dat");

        return sb.toString();
    }

    /**
     * Transforms the grouping string from the record into a GroupBy object.
     * 
     * @param recordGroupBy
     * @return
     */
    private GroupBy createGroupBy(String recordGroupBy) {
        GroupBy groupBy = new GroupBy();
        String[] groups = recordGroupBy.split("-");
        Item[] attributes = new Item[groups.length];

        for (int i = 0; i < groups.length; i++) {
            String[] g = groups[i].split(":");

            String name = g[0];
            String result = g[1];

            Item item = new Item();
            item.setName(name);
            item.setResult(result);
            attributes[i] = item;
        }

        groupBy.setAttributes(attributes);

        return groupBy;
    }

    /**
     * Transforms the records into Aggregate objects
     * 
     * @param aggregateRecordList
     * @return
     */
    private Aggregate[] createAggregates(
            List<AggregateRecord> aggregateRecordList) {
        Aggregate[] aggregates = new Aggregate[aggregateRecordList.size()];

        for (int i = 0; i < aggregates.length; i++) {
            AggregateRecord record = aggregateRecordList.get(i);
            Aggregate aggregate = new Aggregate();
            aggregate.setField(record.getField());

            Item sumItem = new Item();
            sumItem.setName("sum");
            sumItem.setResult(String.valueOf(record.getSum()));

            Item minItem = new Item();
            minItem.setName("min");
            minItem.setResult(String.valueOf(record.getMin()));

            Item maxItem = new Item();
            sumItem.setName("max");
            sumItem.setResult(String.valueOf(record.getMax()));

            Item countItem = new Item();
            minItem.setName("count");
            minItem.setResult(String.valueOf(record.getCount()));

            aggregate.setFunctions(new Item[] { sumItem, minItem, maxItem,
                    countItem });
            aggregates[i] = aggregate;
        }

        return aggregates;
    }
}
