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
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.mpe.gribit2.grib.IMultiGridDefinitionMapping;

/**
 * Arakawa Staggered Grid Definition. Based on:
 * /rary.ohd.pproc.gribit/TEXT/w3fi71.f
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2016 4619       bkowal      Initial creation
 * Aug 10, 2016 4619       bkowal      Implement {@link IMultiGridDefinitionMapping}.
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "staggeredGrid")
public class StaggeredGridDefinition implements IMultiGridDefinitionMapping {

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
     * NI - TOTAL NUMBER OF ACTUAL DATA POINTS INCLUDED ON GRID
     */
    @XmlElement(required = true)
    private int ni;

    /**
     * DUMMY SECOND DIMENSION; SET=1
     */
    @XmlElement(required = true)
    private int nj;

    /**
     * LATITUDE OF FIRST GRID POINT
     */
    @XmlElement(required = true)
    private int la1;

    /**
     * LONGITUDE OF FIRST GRID POINT
     */
    @XmlElement(required = true)
    private int lo1;

    /**
     * RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
     */
    @XmlElement(required = true)
    private int resolutionComponentFlag;

    /**
     * NUMBER OF MASS POINTS ALONG SOUTHERNMOST ROW OF GRID
     */
    @XmlElement(required = true)
    private int la2;

    /**
     * NUMBER OF ROWS IN EACH COLUMN
     */
    @XmlElement(required = true)
    private int lo2;

    /**
     * LONGITUDINAL DIRECTION INCREMENT
     */
    @XmlElement(required = true)
    private int di;

    /**
     * LATITUDINAL DIRECTION INCREMENT
     */
    @XmlElement(required = true)
    private int dj;

    /**
     * SCANNING MODE FLAGS (CODE TABLE 8)
     */
    @XmlElement(required = true)
    private int scanningModeFlag;

    @Override
    public int getNumberXPoints() {
        return getNi();
    }

    @Override
    public void setNumberXPoints(int xPoints) {
        setNi(xPoints);
    }

    @Override
    public int getNumberYPoints() {
        return getNj();
    }

    @Override
    public void setNumberYPoints(int yPoints) {
        setNj(yPoints);
    }

    @Override
    public int getOriginLat() {
        return getLa1();
    }

    @Override
    public void setOriginLat(int originLat) {
        setLa1(originLat);
    }

    @Override
    public int getOriginLon() {
        return getLo1();
    }

    @Override
    public void setOriginLon(int originLon) {
        setLo1(originLon);
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

    @Override
    public int get9thValue() {
        return getLa2();
    }

    @Override
    public int get10thValue() {
        return getLo2();
    }

    @Override
    public int get11thValue() {
        return getDi();
    }

    @Override
    public int get12thValue() {
        return getDj();
    }

    public int getNi() {
        return ni;
    }

    public void setNi(int ni) {
        this.ni = ni;
    }

    public int getNj() {
        return nj;
    }

    public void setNj(int nj) {
        this.nj = nj;
    }

    public int getLa1() {
        return la1;
    }

    public void setLa1(int la1) {
        this.la1 = la1;
    }

    public int getLo1() {
        return lo1;
    }

    public void setLo1(int lo1) {
        this.lo1 = lo1;
    }

    public int getResolutionComponentFlag() {
        return resolutionComponentFlag;
    }

    public void setResolutionComponentFlag(int resolutionComponentFlag) {
        this.resolutionComponentFlag = resolutionComponentFlag;
    }

    public int getLa2() {
        return la2;
    }

    public void setLa2(int la2) {
        this.la2 = la2;
    }

    public int getLo2() {
        return lo2;
    }

    public void setLo2(int lo2) {
        this.lo2 = lo2;
    }

    public int getDi() {
        return di;
    }

    public void setDi(int di) {
        this.di = di;
    }

    public int getDj() {
        return dj;
    }

    public void setDj(int dj) {
        this.dj = dj;
    }

    @Override
    public int getScanningModeFlag() {
        return scanningModeFlag;
    }

    public void setScanningModeFlag(int scanningModeFlag) {
        this.scanningModeFlag = scanningModeFlag;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("StaggeredGridDefinition [");
        sb.append("numVertCoords=").append(numVertCoords);
        sb.append(", pvPL255=").append(pvPL255);
        sb.append(", dataRepresentationType=").append(dataRepresentationType);
        sb.append(", ni=").append(ni);
        sb.append(", nj=").append(nj);
        sb.append(", la1=").append(la1);
        sb.append(", lo1=").append(lo1);
        sb.append(", resolutionComponentFlag=").append(resolutionComponentFlag);
        sb.append(", la2=").append(la2);
        sb.append(", lo2=").append(lo2);
        sb.append(", di=").append(di);
        sb.append(", dj=").append(dj);
        sb.append(", scanningModeFlag=").append(scanningModeFlag);
        sb.append("]");
        return sb.toString();
    }
}