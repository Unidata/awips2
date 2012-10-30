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
package com.raytheon.edex.plugin.grib.spatial;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A sub grid definition
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class SubGridDef implements ISerializableObject {
    /** The model names this sub grid applies to */
    @XmlElement(required = true)
    @XmlList
    private List<String> modelNames;

    @XmlElement
    private String referenceModel;

    @XmlElement
    private String referenceGrid;

    @XmlElement(required = true)
    private double nx;

    @XmlElement(required = true)
    private double ny;
    

    // annotation on setter to enforce data constraints
    private Double centerLatitude;

    // annotation on setter to enforce data constraints
    private Double centerLongitude;

    public List<String> getModelNames() {
        return modelNames;
    }

    public void setModelNames(List<String> modelNames) {
        this.modelNames = modelNames;
    }

    public double getNx() {
        return nx;
    }

    public void setNx(double nx) {
        this.nx = nx;
    }

    public double getNy() {
        return ny;
    }

    public void setNy(double ny) {
        this.ny = ny;
    }

    /**
     * a model may have more than one grid so use reference grid instead.
     * 
     * @return
     */
    @Deprecated
    public String getReferenceModel() {
        return referenceModel;
    }

    /**
     * a model may have more than one grid so use reference grid instead.
     * 
     * @param referenceModel
     */
    @Deprecated
    public void setReferenceModel(String referenceModel) {
        this.referenceModel = referenceModel;
    }

    public String getReferenceGrid() {
        return referenceGrid;
    }

    public void setReferenceGrid(String referenceGrid) {
        this.referenceGrid = referenceGrid;
    }
    
    public Double getCenterLatitude() {
        return centerLatitude;
    }

    @XmlElement
    public void setCenterLatitude(final Double centerLatitude) {
        this.centerLatitude = centerLatitude;
        if (this.centerLatitude != null) {
            this.centerLatitude = new Double(MapUtil.correctLat(centerLatitude
                    .doubleValue()));
        }
    }

    public Double getCenterLongitude() {
        return centerLongitude;
    }

    @XmlElement
    public void setCenterLongitude(final Double centerLongitude) {
        this.centerLongitude = centerLongitude;
        if (this.centerLongitude != null) {
            this.centerLongitude = new Double(
                    MapUtil.correctLon(centerLongitude.doubleValue()));
        }
    }

}