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

import java.util.Map;
import java.util.List;
import java.util.Collection;

import com.raytheon.uf.common.dataplugin.shef.tables.Datalimits;
import com.raytheon.uf.common.dataplugin.shef.tables.Locdatalimits;

/**
 * POJO to store inputs gathered by {@link RocChecker}.
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

public class RocCheckerInputs {

    private Collection<RocCheckerObservationData> observationData;

    private Map<DataLimitKey, List<Datalimits>> dataLimitLookupMap;

    private Map<LocDataLimitKey, List<Locdatalimits>> locationDataLimitLookupMap;

    public RocCheckerInputs() {
    }

    public boolean generationPossible() {
        return (observationData != null && !observationData.isEmpty());
    }

    public Collection<RocCheckerObservationData> getObservationData() {
        return observationData;
    }

    public void setObservationData(
            Collection<RocCheckerObservationData> observationData) {
        this.observationData = observationData;
    }

    public Map<DataLimitKey, List<Datalimits>> getDataLimitLookupMap() {
        return dataLimitLookupMap;
    }

    public void setDataLimitLookupMap(
            Map<DataLimitKey, List<Datalimits>> dataLimitLookupMap) {
        this.dataLimitLookupMap = dataLimitLookupMap;
    }

    public Map<LocDataLimitKey, List<Locdatalimits>> getLocationDataLimitLookupMap() {
        return locationDataLimitLookupMap;
    }

    public void setLocationDataLimitLookupMap(
            Map<LocDataLimitKey, List<Locdatalimits>> locationDataLimitLookupMap) {
        this.locationDataLimitLookupMap = locationDataLimitLookupMap;
    }
}