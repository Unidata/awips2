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
package com.raytheon.uf.common.stats;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Request object to retrieve data for the Stats graphs
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
@DynamicSerialize
public class GraphDataRequest implements IServerRequest {
    /** The time range */
    @DynamicSerializeElement
    private TimeRange timeRange;

    /** Statistics Category */
    @DynamicSerializeElement
    private String category;

    /** Statistics event type */
    @DynamicSerializeElement
    private String eventType;

    /** Statistics field */
    @DynamicSerializeElement
    private String field;

    /** List of groups */
    @DynamicSerializeElement
    private List<String> grouping;

    /** Event data type/Attribute */
    @DynamicSerializeElement
    private String dataType;

    /** Metadata request flag */
    @DynamicSerializeElement
    private boolean metaDataRequest = false;

    /**
     * Data timestep (frequency) in minutes.
     */
    @DynamicSerializeElement
    private int timeStep;

    /**
     * @return the timeRange
     */
    public TimeRange getTimeRange() {
        return timeRange;
    }

    /**
     * @param timeRange
     *            the timeRange to set
     */
    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    /**
     * @return the eventType
     */
    public String getEventType() {
        return eventType;
    }

    /**
     * @param eventType
     *            the eventType to set
     */
    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    /**
     * @return the field
     */
    public String getField() {
        return field;
    }

    /**
     * @param field
     *            the field to set
     */
    public void setField(String field) {
        this.field = field;
    }

    /**
     * @return the grouping
     */
    public List<String> getGrouping() {
        return grouping;
    }

    /**
     * @param grouping
     *            the grouping to set
     */
    public void setGrouping(List<String> grouping) {
        this.grouping = grouping;
    }

    /**
     * @param timeStep
     *            the timeStep to set
     */
    public void setTimeStep(int timeStep) {
        this.timeStep = timeStep;
    }

    /**
     * @return the timeStep
     */
    public int getTimeStep() {
        return timeStep;
    }

    /**
     * @return the dataType
     */
    public String getDataType() {
        return dataType;
    }

    /**
     * @param dataType
     *            the dataType to set
     */
    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    /**
     * @param metaDataRequest
     *            the metaDataRequest to set
     */
    public void setMetaDataRequest(boolean metaDataRequest) {
        this.metaDataRequest = metaDataRequest;
    }

    /**
     * @return the category
     */
    public String getCategory() {
        return category;
    }

    /**
     * @param category
     *            the category to set
     */
    public void setCategory(String category) {
        this.category = category;
    }

    /**
     * @return the metaDataRequest
     */
    public boolean isMetaDataRequest() {
        return metaDataRequest;
    }
}
