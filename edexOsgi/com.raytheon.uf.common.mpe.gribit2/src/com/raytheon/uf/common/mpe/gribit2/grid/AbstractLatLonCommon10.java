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
 * Ten (10) attributes associated with a Latitude/Longitude grid that are common
 * across multiple grid definitions. Based on:
 * /rary.ohd.pproc.gribit/TEXT/w3fi71.f
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2016 4619       bkowal      Initial creation
 * Aug 10, 2016 4619       bkowal      Implement {@link IGridDefinition}.
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractLatLonCommon10 implements IGridDefinition {

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
     * NO. OF POINTS ALONG A LATITUDE
     */
    @XmlElement(required = true)
    private int numberLatPoints;

    /**
     * NO. OF POINTS ALONG A LONGITUDE MERIDIAN
     */
    @XmlElement(required = true)
    private int numberLonPoints;

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
     * LATITUDE OF EXTREME POINT (SOUTH - IVE)
     */
    @XmlElement(required = true)
    private int extremePointLat;

    /**
     * LONGITUDE OF EXTREME POINT (WEST - IVE)
     */
    @XmlElement(required = true)
    private int extremePointLon;

    @Override
    public int getNumberXPoints() {
        return getNumberLatPoints();
    }

    @Override
    public void setNumberXPoints(int xPoints) {
        setNumberLatPoints(xPoints);
    }

    @Override
    public int getNumberYPoints() {
        return getNumberLonPoints();
    }

    @Override
    public void setNumberYPoints(int yPoints) {
        setNumberLonPoints(yPoints);
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

    public int getNumberLatPoints() {
        return numberLatPoints;
    }

    public void setNumberLatPoints(int numberLatPoints) {
        this.numberLatPoints = numberLatPoints;
    }

    public int getNumberLonPoints() {
        return numberLonPoints;
    }

    public void setNumberLonPoints(int numberLonPoints) {
        this.numberLonPoints = numberLonPoints;
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

    public int getExtremePointLat() {
        return extremePointLat;
    }

    public void setExtremePointLat(int extremePointLat) {
        this.extremePointLat = extremePointLat;
    }

    public int getExtremePointLon() {
        return extremePointLon;
    }

    public void setExtremePointLon(int extremePointLon) {
        this.extremePointLon = extremePointLon;
    }

    @Override
    public String toString() {
        /*
         * The class name and opening, closing brackets are excluded in this
         * toString method because this method should ideally only be used
         * directly by a subclass.
         */
        StringBuilder sb = new StringBuilder();
        sb.append("numVertCoords=").append(numVertCoords);
        sb.append(", pvPL255=").append(pvPL255);
        sb.append(", dataRepresentationType=").append(dataRepresentationType);
        sb.append(", numberLatPoints=").append(numberLatPoints);
        sb.append(", numberLonPoints=").append(numberLonPoints);
        sb.append(", numberLonPoints=").append(numberLonPoints);
        sb.append(", originLat=").append(originLat);
        sb.append(", originLon=").append(originLon);
        sb.append(", resolutionFlag=").append(resolutionFlag);
        sb.append(", extremePointLat=").append(extremePointLat);
        sb.append(", extremePointLon=").append(extremePointLon);
        return sb.toString();
    }
}