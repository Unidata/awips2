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
package com.raytheon.viz.radar;

import java.util.Calendar;

import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RadarRecordMetadata implements IRadarRecordMetadata {

    public RadarRecord record;

    public RadarRecordMetadata(RadarRecord record) {
        this.record = record;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getInsertTime()
     */
    @Override
    public Calendar getInsertTime() {
        return record.getInsertTime();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getLatitude()
     */
    @Override
    public Float getLatitude() {
        return record.getLatitude();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getLongitude()
     */
    @Override
    public Float getLongitude() {
        return record.getLongitude();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getFormat()
     */
    @Override
    public String getFormat() {
        return record.getFormat();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getGateResolution()
     */
    @Override
    public Integer getGateResolution() {
        return record.getGateResolution();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getOperationalMode()
     */
    @Override
    public Integer getOperationalMode() {
        return record.getOperationalMode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getNumLevels()
     */
    @Override
    public Integer getNumLevels() {
        return record.getNumLevels();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getLayer()
     */
    @Override
    public Double getLayer() {
        return record.getLayer();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getElevation()
     */
    @Override
    public Float getElevation() {
        return record.getElevation();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getTrueElevationAngle()
     */
    @Override
    public Float getTrueElevationAngle() {
        return record.getTrueElevationAngle();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.IRadarRecordMetadata#getPrimaryElevationAngle()
     */
    @Override
    public Double getPrimaryElevationAngle() {
        return record.getPrimaryElevationAngle();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getCRS()
     */
    @Override
    public ProjectedCRS getCRS() {
        return record.getCRS();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#getNumBins()
     */
    @Override
    public Integer getNumBins() {
        return record.getNumBins();
    }

    @Override
    public Integer getNumRadials() {
        return record.getNumRadials();
    }

    @Override
    public Integer getProductCode() {
        return record.getProductCode();
    }

    @Override
    public String getDataURI() {
        return record.getDataURI();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((record == null) ? 0 : record.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        RadarRecordMetadata other = (RadarRecordMetadata) obj;
        if (record == null) {
            if (other.record != null)
                return false;
        } else if (!record.equals(other.record))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarRecordMetadata#populateRecord()
     */
    @Override
    public RadarRecord populateRecord() {
        RadarRecord tmp = new RadarRecord();
        tmp.setInsertTime(getInsertTime());
        tmp.setLatitude(getLatitude());
        tmp.setLongitude(getLongitude());
        tmp.setFormat(getFormat());
        tmp.setGateResolution(getGateResolution());
        tmp.setOperationalMode(getOperationalMode());
        tmp.setNumLevels(getNumLevels());
        tmp.setLayer(getLayer());
        tmp.setElevation(getElevation());
        tmp.setTrueElevationAngle(getTrueElevationAngle());
        tmp.setPrimaryElevationAngle(getPrimaryElevationAngle());
        tmp.setProductCode(getProductCode());
        tmp.setCRS(getCRS());
        tmp.setNumBins(getNumBins());
        tmp.setNumRadials(getNumRadials());
        tmp.setJstart(record.getJstart());
        tmp.setDataURI(getDataURI());
        return tmp;
    }

}