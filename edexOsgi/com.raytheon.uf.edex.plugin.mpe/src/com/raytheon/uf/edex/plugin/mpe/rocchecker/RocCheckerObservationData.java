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
package com.raytheon.uf.edex.plugin.mpe.rocchecker;

import java.util.Calendar;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.shef.data.Observation;

import java.util.HashMap;

/**
 * POJO used to store observation data associated with a
 * {@link RocCheckerConfig} for later use.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2016 5699       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class RocCheckerObservationData {

    private final Calendar startTime;

    private final Calendar endTime;

    private final RocCheckerConfig config;

    private Map<ObservationKey, List<Observation>> observationLookupMap;

    private final int totalObservationCount;

    public RocCheckerObservationData(RocCheckerConfig config) {
        this(null, null, config,
                new HashMap<ObservationKey, List<Observation>>(1), 0);
    }

    public RocCheckerObservationData(Calendar startTime, Calendar endTime,
            RocCheckerConfig config,
            Map<ObservationKey, List<Observation>> observationLookupMap,
            final int totalObservationCount) {
        if (config == null) {
            throw new IllegalArgumentException(
                    "Required argument 'config' cannot be NULL.");
        }
        if (observationLookupMap == null) {
            throw new IllegalArgumentException(
                    "Required argument 'observationRecords' cannot be NULL.");
        }
        if (startTime == null && !observationLookupMap.isEmpty()) {
            throw new IllegalArgumentException(
                    "Required argument 'startTime' cannot be NULL when observations exist.");
        }
        if (endTime == null && !observationLookupMap.isEmpty()) {
            throw new IllegalArgumentException(
                    "Required argument 'startTime' cannot be NULL when observations exist.");
        }
        this.startTime = startTime;
        this.endTime = endTime;
        this.config = config;
        this.observationLookupMap = observationLookupMap;
        this.totalObservationCount = totalObservationCount;
    }

    public boolean observationsFound() {
        return !observationLookupMap.isEmpty();
    }

    public Calendar getStartTime() {
        return startTime;
    }

    public Calendar getEndTime() {
        return endTime;
    }

    public RocCheckerConfig getConfig() {
        return config;
    }

    public Map<ObservationKey, List<Observation>> getObservationLookupMap() {
        return observationLookupMap;
    }

    public void setObservationLookupMap(
            Map<ObservationKey, List<Observation>> observationLookupMap) {
        this.observationLookupMap = observationLookupMap;
    }

    public int getTotalObservationCount() {
        return totalObservationCount;
    }
}