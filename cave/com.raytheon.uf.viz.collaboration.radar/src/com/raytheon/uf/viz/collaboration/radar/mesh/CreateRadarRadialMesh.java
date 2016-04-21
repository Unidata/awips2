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
package com.raytheon.uf.viz.collaboration.radar.mesh;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension.RadialMeshData;

/**
 * Event class used to specify the creation of a radar radial mesh
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 16, 2012           mschenke    Initial creation
 * Jun 24, 2014  3072     bsteffen    Remove RadarRecord dependency for Radial
 *                                    Mesh
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class CreateRadarRadialMesh extends AbstractDispatchingObjectEvent
        implements ICreationEvent {

    @DynamicSerializeElement
    private Integer numBins;

    @DynamicSerializeElement
    private Integer numRadials;

    @DynamicSerializeElement
    private float[] angleData;

    @DynamicSerializeElement
    private Integer gateResolution;

    @DynamicSerializeElement
    private Float trueElevationAngle;

    @DynamicSerializeElement
    private Integer jstart;

    @DynamicSerializeElement
    private String format;

    @DynamicSerializeElement
    private Float latitude;

    @DynamicSerializeElement
    private Float longitude;

    @DynamicSerializeElement
    private GeneralGridGeometry targetGeometry;

    public RadialMeshData getMeshData() {
        RadialMeshData radarRecord = new RadialMeshData();
        radarRecord.setAngleData(angleData);
        radarRecord.setBinWidth(gateResolution);
        radarRecord.setFirstBin(jstart);
        radarRecord.setLatitude(latitude);
        radarRecord.setLongitude(longitude);
        radarRecord.setNumBins(numBins);
        radarRecord.setNumRadials(numRadials);
        radarRecord.setTiltAngle(trueElevationAngle);
        return radarRecord;
    }

    public void setMeshData(RadialMeshData radarRecord) {
        this.angleData = radarRecord.getAngleData();
        this.format = "Radial";
        this.gateResolution = radarRecord.getBinWidth();
        this.jstart = radarRecord.getFirstBin();
        this.latitude = radarRecord.getLatitude();
        this.longitude = radarRecord.getLongitude();
        this.numBins = radarRecord.getNumBins();
        this.numRadials = radarRecord.getNumRadials();
        this.trueElevationAngle = radarRecord.getTiltAngle();
    }

    /**
     * @return the targetGeometry
     */
    public GeneralGridGeometry getTargetGeometry() {
        return targetGeometry;
    }

    /**
     * @param targetGeometry
     *            the targetGeometry to set
     */
    public void setTargetGeometry(GeneralGridGeometry targetGeometry) {
        this.targetGeometry = targetGeometry;
    }

    /**
     * @return the numBins
     */
    public Integer getNumBins() {
        return numBins;
    }

    /**
     * @param numBins
     *            the numBins to set
     */
    public void setNumBins(Integer numBins) {
        this.numBins = numBins;
    }

    /**
     * @return the numRadials
     */
    public Integer getNumRadials() {
        return numRadials;
    }

    /**
     * @param numRadials
     *            the numRadials to set
     */
    public void setNumRadials(Integer numRadials) {
        this.numRadials = numRadials;
    }

    /**
     * @return the angleData
     */
    public float[] getAngleData() {
        return angleData;
    }

    /**
     * @param angleData
     *            the angleData to set
     */
    public void setAngleData(float[] angleData) {
        this.angleData = angleData;
    }

    /**
     * @return the gateResolution
     */
    public Integer getGateResolution() {
        return gateResolution;
    }

    /**
     * @param gateResolution
     *            the gateResolution to set
     */
    public void setGateResolution(Integer gateResolution) {
        this.gateResolution = gateResolution;
    }

    /**
     * @return the trueElevationAngle
     */
    public Float getTrueElevationAngle() {
        return trueElevationAngle;
    }

    /**
     * @param trueElevationAngle
     *            the trueElevationAngle to set
     */
    public void setTrueElevationAngle(Float trueElevationAngle) {
        this.trueElevationAngle = trueElevationAngle;
    }

    /**
     * @return the jstart
     */
    public Integer getJstart() {
        return jstart;
    }

    /**
     * @param jstart
     *            the jstart to set
     */
    public void setJstart(Integer jstart) {
        this.jstart = jstart;
    }

    /**
     * @return the format
     */
    public String getFormat() {
        return format;
    }

    /**
     * @param format
     *            the format to set
     */
    public void setFormat(String format) {
        this.format = format;
    }

    /**
     * @return the latitude
     */
    public Float getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(Float latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public Float getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(Float longitude) {
        this.longitude = longitude;
    }

}
