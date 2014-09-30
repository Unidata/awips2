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
package com.raytheon.uf.common.dataplugin.radar.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.time.TimeRange;

/**
 * A request for radar hdf5 data. Requires a radar id, product code, primary
 * elevation angle, and a time range in order to search.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2014  3393       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@DynamicSerialize
public class GetRadarDataRecordRequest implements IServerRequest {

    /** The ICAO id of the radar. */
    @DynamicSerializeElement
    private String radarId;

    /** The product code. */
    @DynamicSerializeElement
    private int productCode;

    /** The primary elevation angle. */
    @DynamicSerializeElement
    private double primaryElevationAngle;

    /** The datetime range. */
    @DynamicSerializeElement
    private TimeRange timeRange;

    /**
     * Constructor.
     */
    public GetRadarDataRecordRequest() {
        super();
    }

    /**
     * @return the radarId
     */
    public String getRadarId() {
        return radarId;
    }

    /**
     * @param radarId
     *            the radarId to set
     */
    public void setRadarId(String radarId) {
        this.radarId = radarId;
    }

    /**
     * @return the productCode
     */
    public int getProductCode() {
        return productCode;
    }

    /**
     * @param productCode
     *            the productCode to set
     */
    public void setProductCode(int productCode) {
        this.productCode = productCode;
    }

    /**
     * @return the primaryElevationAngle
     */
    public double getPrimaryElevationAngle() {
        return primaryElevationAngle;
    }

    /**
     * @param primaryElevationAngle
     *            the primaryElevationAngle to set
     */
    public void setPrimaryElevationAngle(double primaryElevationAngle) {
        this.primaryElevationAngle = primaryElevationAngle;
    }

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
}
