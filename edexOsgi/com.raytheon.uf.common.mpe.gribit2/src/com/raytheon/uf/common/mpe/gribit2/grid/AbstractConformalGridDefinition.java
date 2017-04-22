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
package com.raytheon.uf.common.mpe.gribit2.grid;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Common attributes associated with conformal
 * (https://en.wikipedia.org/wiki/List_of_map_projections#Properties) map
 * projections associated with grid definitions utilized by gribit. Based on:
 * /rary.ohd.pproc.gribit/TEXT/w3fi71.f
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2016 4619       bkowal      Initial creation
 * Aug 10, 2016 4619       bkowal      Implement {@link IGridDefinition}.
 * Aug 11, 2016 4619       bkowal      Include {@link #originLon} in {@link #toString()}.
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AbstractConformalGridDefinition implements IGridDefinition {

    /**
     * NUMBER OF VERTICAL COORDINATES
     */
    @XmlElement(required = true)
    private int numVertCoords;

    /**
     * PV, PL OR 255
     */
    @XmlElement(required = true)
    private int pvPL255;

    /**
     * DATA REPRESENTATION TYPE (CODE TABLE 6)
     */
    @XmlElement(required = true)
    private int dataRepresentationType;

    /**
     * NO. OF POINTS ALONG X-AXIS
     */
    @XmlElement(required = true)
    private int numXAxisPoints;

    /**
     * NO. OF POINTS ALONG Y-AXIS
     */
    @XmlElement(required = true)
    private int numYAxisPoints;

    /**
     * LATITUDE OF ORIGIN (SOUTH - IVE)
     */
    @XmlElement(required = true)
    private int originLat;

    /**
     * LONGITUDE OF ORIGIN (WEST -IVE)
     */
    @XmlElement(required = true)
    private int originLon;

    /**
     * RESOLUTION FLAG (CODE TABLE 7)
     */
    @XmlElement(required = true)
    private int resolutionFlag;

    /**
     * LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
     */
    @XmlElement(required = true)
    private int lonMeridianParallelX;

    /**
     * X-DIRECTION GRID LENGTH (INCREMENT)
     */
    @XmlElement(required = true)
    private int gridLengthX;

    /**
     * Y-DIRECTION GRID LENGTH (INCREMENT)
     */
    @XmlElement(required = true)
    private int gridLengthY;

    /**
     * PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE, 1=SOUTH POLE ON PLANE)
     */
    @XmlElement(required = true)
    private int projectionCenterFlag;

    /**
     * SCANNING MODE FLAGS (CODE TABLE 8)
     */
    @XmlElement(required = true)
    private int scanningModeFlag;

    @Override
    public int getNumberXPoints() {
        return getNumXAxisPoints();
    }

    @Override
    public void setNumberXPoints(int xPoints) {
        setNumXAxisPoints(xPoints);
    }

    @Override
    public int getNumberYPoints() {
        return getNumYAxisPoints();
    }

    @Override
    public void setNumberYPoints(int yPoints) {
        setNumYAxisPoints(yPoints);
    }

    @Override
    public int getNumVertCoords() {
        return numVertCoords;
    }

    @Override
    public void setNumVertCoords(int numVertCoords) {
        this.numVertCoords = numVertCoords;
    }

    @Override
    public int getPvPL255() {
        return pvPL255;
    }

    @Override
    public void setPvPL255(int pvPL255) {
        this.pvPL255 = pvPL255;
    }

    @Override
    public int getDataRepresentationType() {
        return dataRepresentationType;
    }

    @Override
    public void setDataRepresentationType(int dataRepresentationType) {
        this.dataRepresentationType = dataRepresentationType;
    }

    public int getNumXAxisPoints() {
        return numXAxisPoints;
    }

    public void setNumXAxisPoints(int numXAxisPoints) {
        this.numXAxisPoints = numXAxisPoints;
    }

    public int getNumYAxisPoints() {
        return numYAxisPoints;
    }

    public void setNumYAxisPoints(int numYAxisPoints) {
        this.numYAxisPoints = numYAxisPoints;
    }

    @Override
    public int getOriginLat() {
        return originLat;
    }

    @Override
    public void setOriginLat(int originLat) {
        this.originLat = originLat;
    }

    @Override
    public int getOriginLon() {
        return originLon;
    }

    @Override
    public void setOriginLon(int originLon) {
        this.originLon = originLon;
    }

    public int getResolutionFlag() {
        return resolutionFlag;
    }

    public void setResolutionFlag(int resolutionFlag) {
        this.resolutionFlag = resolutionFlag;
    }

    public int getLonMeridianParallelX() {
        return lonMeridianParallelX;
    }

    public void setLonMeridianParallelX(int lonMeridianParallelX) {
        this.lonMeridianParallelX = lonMeridianParallelX;
    }

    public int getGridLengthX() {
        return gridLengthX;
    }

    public void setGridLengthX(int gridLengthX) {
        this.gridLengthX = gridLengthX;
    }

    public int getGridLengthY() {
        return gridLengthY;
    }

    public void setGridLengthY(int gridLengthY) {
        this.gridLengthY = gridLengthY;
    }

    public int getProjectionCenterFlag() {
        return projectionCenterFlag;
    }

    public void setProjectionCenterFlag(int projectionCenterFlag) {
        this.projectionCenterFlag = projectionCenterFlag;
    }

    public int getScanningModeFlag() {
        return scanningModeFlag;
    }

    public void setScanningModeFlag(int scanningModeFlag) {
        this.scanningModeFlag = scanningModeFlag;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("numVertCoords=").append(numVertCoords);
        sb.append(", pvPL255=").append(pvPL255);
        sb.append(", dataRepresentationType=").append(dataRepresentationType);
        sb.append(", numXAxisPoints=").append(numXAxisPoints);
        sb.append(", numYAxisPoints=").append(numYAxisPoints);
        sb.append(", originLat=").append(originLat);
        sb.append(", originLon=").append(originLon);
        sb.append(", resolutionFlag=").append(resolutionFlag);
        sb.append(", lonMeridianParallelX=").append(lonMeridianParallelX);
        sb.append(", gridLengthX=").append(gridLengthX);
        sb.append(", gridLengthY=").append(gridLengthY);
        sb.append(", projectionCenterFlag=").append(projectionCenterFlag);
        sb.append(", scanningModeFlag=").append(scanningModeFlag);
        return sb.toString();
    }
}