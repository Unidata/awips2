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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;

import com.raytheon.uf.common.dataplugin.shef.tables.Dailypp;
import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.IHourlyTS;
import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Temperature;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DailyPPDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DatalimitsDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.HourlyPCDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.HourlyPPDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.LocdatalimitsDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.TemperatureDao;

/**
 * Container for the hourlypp, hourlypc, dailypp, and limits data that will be
 * utilized for a single run of the DQC PreProcessor. This container ensures
 * that the state of the data is captured at a single point in time for use
 * throughout an entire single run of the DQC PreProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 9, 2018  7184       bkowal      Initial creation
 * Mar 12, 2018 7184       bkowal      Arrange hourlypp data into {@link #lidTSHourlyPPMap}
 *                                     before it is used.
 *                                     
 * Nov 21, 2019 21684      jkelmer     
 *
 * </pre>
 *
 * @author bkowal
 */

public class DQCPreProcRunData {

    private List<Dailypp> dailyPPData = Collections.emptyList();

    private Map<LidTSKey, Set<IHourlyTS>> lidTSHourlyPPMap = Collections
            .emptyMap();

    private Map<LidTSKey, Set<IHourlyTS>> lidTSHourlyPCMap = Collections
            .emptyMap();

    private List<Temperature> temperatureData = Collections.emptyList();

    private List<Datalimits> precipDatalimits = Collections.emptyList();

    private Map<String, List<Locdatalimits>> precipLidLocdatalimitMap = Collections
            .emptyMap();

    private List<Datalimits> tempDatalimits = Collections.emptyList();

    private Map<String, List<Locdatalimits>> tempLidLocdatalimitMap = Collections
            .emptyMap();

    public DQCPreProcRunData(final DQCPreProcConfig config) {
        gather(config);
    }

    /**
     * Gathers the data required for a run and arranges it within lookup tables
     * for easy and convenient access.
     */
    private void gather(final DQCPreProcConfig config) {
        /*
         * Gather the DailyPP data.
         */
        final DailyPPDao dailyPPDao = new DailyPPDao();
        dailyPPData = dailyPPDao.retrieveNonMissingDataWithinObsTimeRange(
                config.getStartDate(), config.getEndDate());

        /*
         * DR 21684
         * Set the start date back a day to get Hourly Values for the previous 12-12 run
         * Fixes the earliest date in a run not getting HourlyPP values
         */
        Calendar hourlyStartDate = (Calendar) config.getStartDate().clone();
        hourlyStartDate.add(Calendar.DATE, -1);
        
        /*
         * Gather the HourlyPP data.
         */
        final HourlyPPDao hourlyPPDao = new HourlyPPDao();

        List<Hourlypp> hourlyPPResults = hourlyPPDao
                .retrieveDataWithinObsDateRange(hourlyStartDate,
                        config.getEndDate());
        if (CollectionUtils.isNotEmpty(hourlyPPResults)) {
            lidTSHourlyPPMap = new HashMap<>(hourlyPPResults.size(), 1.0f);
            for (Hourlypp hourlypp : hourlyPPResults) {
                final LidTSKey lidTSKey = new LidTSKey(hourlypp.getLid(),
                        hourlypp.getTs());
                Set<IHourlyTS> hourlyTSSet = lidTSHourlyPPMap.get(lidTSKey);
                if (hourlyTSSet == null) {
                    hourlyTSSet = new LinkedHashSet<>();
                    lidTSHourlyPPMap.put(lidTSKey, hourlyTSSet);
                }
                hourlyTSSet.add(hourlypp);
            }
        }

        /*
         * Gather the HourlyPC data (if enabled). Limits are only needed to
         * "verify" totals calculated from the HourlyPC data.
         */
        if (Boolean.TRUE.equals(config.getLoadHourlyPC())) {
            final HourlyPCDao hourlyPCDao = new HourlyPCDao();
            final List<Hourlypc> hourlyPCResults = hourlyPCDao
                    .retrieveDataWithinObsDateRange(hourlyStartDate,
                            config.getEndDate());
            if (CollectionUtils.isNotEmpty(hourlyPCResults)) {
                /*
                 * Segregate the hourlypc records by lid and ts. This will be
                 * used later to fill in empty output slots per the Apps
                 * Defaults configuration.
                 */
                lidTSHourlyPCMap = new HashMap<>(hourlyPCResults.size(), 1.0f);
                for (Hourlypc hourlypc : hourlyPCResults) {
                    final LidTSKey lidTSKey = new LidTSKey(hourlypc.getLid(),
                            hourlypc.getTs());
                    Set<IHourlyTS> hourlyTSSet = lidTSHourlyPCMap.get(lidTSKey);
                    if (hourlyTSSet == null) {
                        hourlyTSSet = new LinkedHashSet<>();
                        lidTSHourlyPCMap.put(lidTSKey, hourlyTSSet);
                    }
                    hourlyTSSet.add(hourlypc);
                }

                /*
                 * Load the data limits when there is potential HourlyPC data
                 * available to verify.
                 */
                DatalimitsDao datalimitsDao = new DatalimitsDao();
                precipDatalimits = datalimitsDao
                        .getPPWithDur1006_2001_5004Limits();
                removeUnusableDataLimits(precipDatalimits);

                LocdatalimitsDao locdatalimitsDao = new LocdatalimitsDao();
                List<Locdatalimits> locdatalimits = locdatalimitsDao
                        .getPPWithDur1006_2001_5004Limits();
                precipLidLocdatalimitMap = buildLocDataLimitsByLidMap(
                        locdatalimits);
            }
        }

        /*
         * Load the temperature data.
         */
        final Calendar temperatureEndTime = TimeUtil
                .newCalendar(config.getEndDate());
        /*
         * Add additional look-ahead time for the temperature report.
         */
        temperatureEndTime.add(Calendar.MINUTE,
                (int) config.getTemperatureWindow());
        final Calendar temperatureStartTime = TimeUtil
                .newCalendar(config.getStartDate());
        /*
         * Add additional look-behind time for the temperature report.
         */
        temperatureStartTime.add(Calendar.MINUTE,
                (int) -config.getTemperatureWindow());

        final TemperatureDao temperatureDao = new TemperatureDao();
        temperatureData = temperatureDao
                .retrieveNonMissingTADataWithinObsTimeRange(
                        temperatureStartTime, temperatureEndTime);
        if (!temperatureData.isEmpty()) {
            /*
             * Load the temperature limits when temperature data exists.
             */
            DatalimitsDao datalimitsDao = new DatalimitsDao();
            tempDatalimits = datalimitsDao.getTALimits();
            removeUnusableDataLimits(precipDatalimits);

            LocdatalimitsDao locdatalimitsDao = new LocdatalimitsDao();
            List<Locdatalimits> locdatalimits = locdatalimitsDao.getTALimits();
            tempLidLocdatalimitMap = buildLocDataLimitsByLidMap(locdatalimits);
        }
    }

    private void removeUnusableDataLimits(
            final List<Datalimits> datalimitsList) {
        if (CollectionUtils.isEmpty(datalimitsList)) {
            return;
        }
        Iterator<Datalimits> iterator = datalimitsList.iterator();
        while (iterator.hasNext()) {
            if (iterator.next().getGrossRangeMax() == null) {
                /*
                 * Limits without a gross range max will never be applicable.
                 * So, they will be excluded immediately.
                 */
                iterator.remove();
            }
        }
    }

    private Map<String, List<Locdatalimits>> buildLocDataLimitsByLidMap(
            List<Locdatalimits> locdatalimits) {
        if (locdatalimits.isEmpty()) {
            return Collections.emptyMap();
        }
        final Map<String, List<Locdatalimits>> destinationMap = new HashMap<>(
                locdatalimits.size(), 1.0f);
        for (Locdatalimits ldl : locdatalimits) {
            if (ldl.getGrossRangeMax() == null) {
                /*
                 * Limits without a gross range max will never be applicable.
                 * So, they will be excluded immediately.
                 */
                continue;
            }
            final String lid = ldl.getId().getLid();
            List<Locdatalimits> ldlList = destinationMap.get(lid);
            if (ldlList == null) {
                ldlList = new ArrayList<>();
                destinationMap.put(lid, ldlList);
            }
            ldlList.add(ldl);
        }

        return destinationMap;
    }

    /**
     * @return the dailyPPData
     */
    public List<Dailypp> getDailyPPData() {
        return dailyPPData;
    }

    public Map<LidTSKey, Set<IHourlyTS>> getLidTSHourlyPPMap() {
        return lidTSHourlyPPMap;
    }

    /**
     * @return the lidTSHourlyPCMap
     */
    public Map<LidTSKey, Set<IHourlyTS>> getLidTSHourlyPCMap() {
        return lidTSHourlyPCMap;
    }

    /**
     * @return the temperatureData
     */
    public List<Temperature> getTemperatureData() {
        return temperatureData;
    }

    /**
     * @return the precipDatalimits
     */
    public List<Datalimits> getPrecipDatalimits() {
        return precipDatalimits;
    }

    /**
     * @return the precipLidLocdatalimitMap
     */
    public Map<String, List<Locdatalimits>> getPrecipLidLocdatalimitMap() {
        return precipLidLocdatalimitMap;
    }

    /**
     * @return the tempDatalimits
     */
    public List<Datalimits> getTempDatalimits() {
        return tempDatalimits;
    }

    /**
     * @return the tempLidLocdatalimitMap
     */
    public Map<String, List<Locdatalimits>> getTempLidLocdatalimitMap() {
        return tempLidLocdatalimitMap;
    }
}