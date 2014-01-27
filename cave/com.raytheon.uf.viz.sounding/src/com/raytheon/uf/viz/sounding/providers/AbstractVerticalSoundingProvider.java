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
package com.raytheon.uf.viz.sounding.providers;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Abstract implementation of {@link IVerticalSoundingProvider}, attempts to do
 * {@link DataTime} and {@link PluginDataObject} retrieval in a common way
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2013       2190 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractVerticalSoundingProvider<T> implements
        IVerticalSoundingProvider {

    protected static final String DATATIME_REQUEST_FIELD_ID = "dataTime";

    private Map<String, RequestConstraint> constraints;

    private DataTime[] dataTimes;

    private Map<DataTime, T> dataMap = new HashMap<DataTime, T>();

    void setConstraints(Map<String, RequestConstraint> constraints) {
        if (constraints == null) {
            throw new IllegalArgumentException(
                    "Sounding constraints must not be null");
        }
        this.constraints = constraints;
        populateBaseConstraints(this.constraints);
    }

    /**
     * Method classes can override to add to the base constraints
     * 
     * @param constraints
     */
    protected void populateBaseConstraints(
            Map<String, RequestConstraint> constraints) {
        // Default does nothing
    }

    @Override
    public final DataTime[] getSoundingTimes() {
        if (dataTimes == null) {
            dataTimes = queryForSoundingTimes(new HashMap<String, RequestConstraint>(
                    constraints));
        }
        return dataTimes;
    }

    protected DataTime[] queryForSoundingTimes(
            Map<String, RequestConstraint> constraints) {
        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(new HashMap<String, RequestConstraint>(
                constraints));
        request.setDistinct(true);
        request.addRequestField(DATATIME_REQUEST_FIELD_ID);
        try {
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            return response.getFieldObjects(DATATIME_REQUEST_FIELD_ID,
                    DataTime.class);
        } catch (VizException e) {
            throw new RuntimeException(
                    "Error querying for available sounding times", e);
        }
    }

    @Override
    public final VerticalSounding getSounding(DataTime time, Coordinate location) {
        T data = dataMap.get(time);
        if (data == null) {
            data = queryForData(new HashMap<String, RequestConstraint>(
                    this.constraints), time, location);
            dataMap.put(time, data);
        }

        VerticalSounding sounding = null;
        if (data != null) {
            sounding = createSounding(time, data, location);
        }
        return sounding;
    }

    protected abstract T queryForData(
            Map<String, RequestConstraint> constraints, DataTime time,
            Coordinate location);

    /**
     * Given records and a location, query for
     * 
     * @param data
     * @param location
     * @return
     */
    protected abstract VerticalSounding createSounding(DataTime time,
            T records, Coordinate location);
}