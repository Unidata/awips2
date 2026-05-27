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
package com.raytheon.uf.common.dataplugin.radar.response;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * A data object for a single radar record. The IDataRecords and a subset of the
 * radar fields are available.
 * 
 * String fields are used to match MSAS/LAPS uEngine precision for side by side
 * comparison.
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
public class RadarDataRecord {

    @DynamicSerializeElement
    private IDataRecord[] hdf5Data;

    @DynamicSerializeElement
    private DataTime dataTime;

    @DynamicSerializeElement
    private String latitude;

    @DynamicSerializeElement
    private String longitude;

    @DynamicSerializeElement
    private String elevation;

    @DynamicSerializeElement
    private String elevationNumber;

    @DynamicSerializeElement
    private String trueElevationAngle;

    @DynamicSerializeElement
    private String volumeCoveragePattern;

    /**
     * Constructor.
     */
    public RadarDataRecord() {
        super();
    }

    /**
     * @return the hdf5Data
     */
    public IDataRecord[] getHdf5Data() {
        return hdf5Data;
    }

    /**
     * @param hdf5Data
     *            the hdf5Data to set
     */
    public void setHdf5Data(IDataRecord[] hdf5Data) {
        this.hdf5Data = hdf5Data;
    }

    /**
     * @return the dataTime
     */
    public DataTime getDataTime() {
        return dataTime;
    }

    /**
     * @param dataTime
     *            the dataTime to set
     */
    public void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
    }

    /**
     * @return the latitude
     */
    public String getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(String latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public String getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(String longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the elevation
     */
    public String getElevation() {
        return elevation;
    }

    /**
     * @param elevation
     *            the elevation to set
     */
    public void setElevation(String elevation) {
        this.elevation = elevation;
    }

    /**
     * @return the elevationNumber
     */
    public String getElevationNumber() {
        return elevationNumber;
    }

    /**
     * @param elevationNumber
     *            the elevationNumber to set
     */
    public void setElevationNumber(String elevationNumber) {
        this.elevationNumber = elevationNumber;
    }

    /**
     * @return the trueElevationAngle
     */
    public String getTrueElevationAngle() {
        return trueElevationAngle;
    }

    /**
     * @param trueElevationAngle
     *            the trueElevationAngle to set
     */
    public void setTrueElevationAngle(String trueElevationAngle) {
        this.trueElevationAngle = trueElevationAngle;
    }

    /**
     * @return the volumeCoveragePattern
     */
    public String getVolumeCoveragePattern() {
        return volumeCoveragePattern;
    }

    /**
     * @param volumeCoveragePattern
     *            the volumeCoveragePattern to set
     */
    public void setVolumeCoveragePattern(String volumeCoveragePattern) {
        this.volumeCoveragePattern = volumeCoveragePattern;
    }
}
