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
package com.raytheon.viz.mpe.ui.rsc;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;

import com.raytheon.uf.common.dataplugin.mpe.PrecipRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.mpe.fieldgen.PrecipDataKey;
import com.raytheon.uf.common.mpe.fieldgen.PrecipDataRequest;
import com.raytheon.uf.common.mpe.fieldgen.PrecipDataResponse;
import com.raytheon.uf.common.mpe.fieldgen.PrecipField;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * Used to retrieve MPE precipitation data from multiple sources.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2017 6407       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public final class MPEPrecipDataLoader {

    private MPEPrecipDataLoader() {
    }

    /**
     * Retrieves the requested {@link MPEPrecipData} for the specified
     * {@link DisplayFieldData} and the specified {@link Calendar}. Handles
     * determining whether the data should be retrieved from hdf5 or xmrg.
     * 
     * @param displayField
     *            the specified {@link DisplayFieldData}.
     * @param timeToLoad
     *            the specified {@link Calendar}.
     * @return the retrieved {@link MPEPrecipData} if available
     * @throws MPEDataLoadException
     */
    public static MPEPrecipData load(final DisplayFieldData displayField,
            final Calendar timeToLoad) throws MPEDataLoadException {
        if (displayField == DisplayFieldData.goesRSatPre) {
            return loadFromHdf5(PrecipField.SATPRE, timeToLoad);
        }
        return loadFromXmrg(displayField, timeToLoad);
    }

    private static MPEPrecipData loadFromXmrg(
            final DisplayFieldData displayField, final Calendar timeToLoad)
            throws MPEDataLoadException {
        XmrgFile xmrgFile = MPEDisplayManager.getXmrgFile(displayField,
                timeToLoad.getTime());
        try {
            xmrgFile.load();
        } catch (IOException e) {
            throw new MPEDataLoadException("Failed to load xmrg file: "
                    + xmrgFile.getFile().getAbsolutePath() + ".", e);
        }

        return new MPEPrecipData(xmrgFile.getHrapExtent(), xmrgFile.getData());
    }

    private static MPEPrecipData loadFromHdf5(final PrecipField field,
            final Calendar timeToLoad) throws MPEDataLoadException {
        PrecipDataKey precipDataKey = new PrecipDataKey(field,
                TimeUtil.newGmtCalendar(timeToLoad.getTimeInMillis()));
        final MPEPrecipInventoryManager inventory = MPEPrecipInventoryManager
                .getInstance(field);
        if (!inventory.inventoryExistsAndDataAvailable(precipDataKey)) {
            /*
             * There is presently not any known inventory for this data. So,
             * retrieval would return nothing.
             */
            /*
             * TODO: improve handling of no data scenarios across all MPE precip
             * data.
             */
            throw new MPEDataLoadException(
                    "Failed to load precipitation data for: "
                            + precipDataKey.toString() + ".");
        }
        MPEPrecipData mpePrecipData = inventory
                .retrieveFromInventory(precipDataKey);
        if (mpePrecipData != null) {
            return mpePrecipData;
        }
        PrecipDataResponse response = null;
        try {
            response = (PrecipDataResponse) RequestRouter
                    .route(new PrecipDataRequest(precipDataKey));
        } catch (Exception e) {
            throw new MPEDataLoadException(
                    "Failed to load precipitation data for: "
                            + precipDataKey.toString() + ".",
                    e);
        }

        mpePrecipData = new MPEPrecipData(response.getHrapExtent(),
                response.getData());
        inventory.addToInventory(precipDataKey, mpePrecipData);
        return mpePrecipData;
    }

    /**
     * Retrieves a {@link Set} of {@link PrecipDataKey}s corresponding to the
     * readily available hdf5 data for the specified {@link PrecipField}.
     * 
     * @param field
     *            the specified {@link PrecipField}
     * @return the retrieved {@link Set} of {@link PrecipDataKey}s
     * @throws MPEDataLoadException
     */
    public static Set<PrecipDataKey> getInventory(final PrecipField field)
            throws MPEDataLoadException {
        List<DataTime> dataTimes = null;
        try {
            TimeQueryRequest request = new TimeQueryRequest();
            request.setPluginName(PrecipRecord.PLUGIN_NAME);
            Map<String, RequestConstraint> queryTerms = new HashMap<>(1, 1.0f);
            queryTerms.put(PrecipRecord.Fields.PRECIP_FIELD,
                    new RequestConstraint(field.name()));
            request.setQueryTerms(queryTerms);
            /*
             * The whole of MPE (particularly all other precip fields) is not
             * compatible with simulated time. So, ensure that it will not
             * affect the query results.
             */
            request.setSimDate(null);
            List<?> results = (List<?>) RequestRouter.route(request);
            if (CollectionUtils.isEmpty(results)) {
                dataTimes = Collections.emptyList();
            } else {
                dataTimes = new ArrayList<>(results.size());
                for (Object result : results) {
                    dataTimes.add((DataTime) result);
                }
            }
        } catch (Exception e) {
            throw new MPEDataLoadException(
                    "Failed to load inventory for precip: " + field.name()
                            + ".",
                    e);
        }

        Set<PrecipDataKey> inventory = new HashSet<>(dataTimes.size());
        for (DataTime dataTime : dataTimes) {
            inventory.add(
                    new PrecipDataKey(field, dataTime.getRefTimeAsCalendar()));
        }
        return inventory;
    }
}