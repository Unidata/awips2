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
package com.raytheon.edex.plugin.binlightning.filter;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.geospatial.adapter.GeometryAdapter;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Root JAXB POJO for geographic filter configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2014 3226      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlRootElement(name = "filters")
@XmlAccessorType(XmlAccessType.NONE)
public class GeoFilters {

    @XmlElement(name = "wkt")
    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    private List<Geometry> geometries;
    
    @XmlElement(name = "cwa")
    private List<String> cwas;

    @XmlElement(name = "bbox")
    private List<GeoFilterBbox> bboxes;

    /**
     * 
     */
    public GeoFilters() {
    }

    /**
     * @param geometries
     * @param cwas
     * @param bboxes
     */
    public GeoFilters(List<Geometry> geometries,
 List<String> cwas,
            List<GeoFilterBbox> bboxes) {
        this.geometries = geometries;
        this.cwas = cwas;
        this.bboxes = bboxes;
    }

    /**
     * @return the geometries
     */
    public List<Geometry> getGeometries() {
        return geometries;
    }

    /**
     * @param geometries
     *            the geometries to set
     */
    public void setGeometries(List<Geometry> geometries) {
        this.geometries = geometries;
    }

    /**
     * @return the cwas
     */
    public List<String> getCwas() {
        return cwas;
    }

    /**
     * @param cwas
     *            the cwas to set
     */
    public void setCwas(List<String> cwas) {
        this.cwas = cwas;
    }

    /**
     * @return the bboxes
     */
    public List<GeoFilterBbox> getBboxes() {
        return bboxes;
    }

    /**
     * @param bboxes
     *            the bboxes to set
     */
    public void setBboxes(List<GeoFilterBbox> bboxes) {
        this.bboxes = bboxes;
    }

}
