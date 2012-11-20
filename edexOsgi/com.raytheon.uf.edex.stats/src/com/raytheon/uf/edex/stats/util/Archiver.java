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

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Archives the data in the aggregate_bucket table to an xml file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez     Initial creation.
 * Nov 09, 2012            dhladky      Changed to CSV output
 * 
 * </pre>
 * 
 * @author jsanchez
 * 
 */
public class Archiver {

    private class StatisticsKey {
        public String eventType;

        public String grouping;

        public TimeRange timeRange;

        @Override
        public boolean equals(Object o) {
            if (o != null && o instanceof StatisticsKey) {
                StatisticsKey other = (StatisticsKey) o;

                return eventType.equals(other.eventType)
                        && timeRange.getStart().equals(
                                other.timeRange.getStart())
                        && timeRange.getEnd().equals(other.timeRange.getEnd());
            }

            return false;
        }

        @Override
        public int hashCode() {
            return 1;
        }
    }

    private static final String COMMA = ",";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Archiver.class);

    private final IPathManager pm = PathManagerFactory.getPathManager();

    private final LocalizationContext context = pm.getContext(
            LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

    private static final String DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";

    private static final String FILE_DATE_FORMAT = "yyyyMMdd_HHmm";

    private static final Pattern PERIOD_PATTERN = Pattern.compile("\\.");

    public Archiver() {

    }

    /**
     * Creates a filename in the format /stats/aggregates/group...
     * /eventType.start-end.dat
     * 
     * @param items
     * @return
     */
    private String createFilename(TimeRange tr, String eventType, String group) {

        SimpleDateFormat fileDateFormatter = new SimpleDateFormat(
                FILE_DATE_FORMAT);
        StringBuilder sb = new StringBuilder("stats/aggregates");
        String[] chunks = PERIOD_PATTERN.split(eventType);
        sb.append("/");
        sb.append(group);
        sb.append("/");
        sb.append(chunks[chunks.length - 1]);
        sb.append(".");
        sb.append(fileDateFormatter.format(tr.getStart()));
        sb.append("-");
        sb.append(fileDateFormatter.format(tr.getEnd()));
        sb.append(".csv");

        return sb.toString();
    }

    /**
     * Used for outputting the stats as CSV
     * 
     * @return
     */
    private String getCSVOutput(AggregateRecord agrec,
            SimpleDateFormat dateFormat) {

        StringBuilder sb = new StringBuilder();

        String eventType = agrec.getEventType();
        Calendar startDate = agrec.getStartDate();
        Calendar endDate = agrec.getEndDate();
        String grouping = agrec.getGrouping();
        String field = agrec.getField();
        double max = agrec.getMax();
        double min = agrec.getMin();
        double sum = agrec.getSum();
        double count = agrec.getCount();

        if (eventType != null) {
            sb.append(eventType).append(COMMA);
        }

        if (startDate != null) {
            sb.append(dateFormat.format(startDate.getTime()))
                    .append(COMMA);
        }

        if (endDate != null) {
            sb.append(dateFormat.format(endDate.getTime())).append(
                    COMMA);
        }
        if (grouping != null) {
            sb.append(grouping).append(COMMA);
        }
        if (field != null) {
            sb.append(field).append(COMMA);
        }

        sb.append(max).append(COMMA);
        sb.append(min).append(COMMA);
        sb.append(sum).append(COMMA);
        sb.append(count);

        return sb.toString();
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

            String eventType = key.eventType;
            String grouping = key.grouping;
            List<AggregateRecord> records = statisticsMap.get(key);

            String filename = createFilename(key.timeRange, eventType, grouping);
            try {
                writeToFile(filename, records);
            } catch (JAXBException e) {
                statusHandler.error("Unable to write statistics file "
                        + filename, e);
            }
        }
    }

    /**
     * Writes the statistics xml to disk.
     * 
     * @param statistics
     * @throws JAXBException
     */
    public void writeToFile(String filename, List<AggregateRecord> records)
            throws JAXBException {

        BufferedWriter bw = null;
        SimpleDateFormat dateFormatter = new SimpleDateFormat(DATE_FORMAT);
        LocalizationFile siteLocalization = pm.getLocalizationFile(context,
                filename);
        String outputFilePath = siteLocalization.getFile().getAbsolutePath();
        // pre-create directories if necessary
        siteLocalization.getFile().getParentFile().mkdirs();
        // Write this to output CSV
        try {
            bw = new BufferedWriter(new FileWriter(
                    outputFilePath));
            if (bw != null) {
                for (AggregateRecord agrec : records) {
                    bw.write(getCSVOutput(agrec, dateFormatter));
                    bw.newLine();
                }
            }

        } catch (IOException e) {

            statusHandler.handle(Priority.ERROR, "Failed to write File: "
                    + outputFilePath, e);
        } finally {
            if (bw != null) {
                try {
                    bw.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "failed to close CSV output file stream. "
                                    + filename, e);
                }
            }
        }

    }
}
