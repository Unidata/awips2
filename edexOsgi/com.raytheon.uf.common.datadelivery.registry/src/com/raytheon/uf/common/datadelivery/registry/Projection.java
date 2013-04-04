package com.raytheon.uf.common.datadelivery.registry;

import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.MercatorGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.util.ReflectionUtil;

/**
 * 
 * A Projection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Projection implements ISerializableObject {

    /**
     * Projection
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Oct 1, 2012            dhladky     Initial creation
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    @XmlEnum
    @XmlType(namespace = "com.raytheon.uf.common.datadelivery.registry")
    @DynamicSerialize
    public enum ProjectionType {

        LatLon(LatLonGridCoverage.PROJECTION_TYPE, LatLonGridCoverage.class), LambertConformal(
                LambertConformalGridCoverage.PROJECTION_TYPE,
                LambertConformalGridCoverage.class), Mercator(
                MercatorGridCoverage.PROJECTION_TYPE,
                MercatorGridCoverage.class), PolarStereo(
                PolarStereoGridCoverage.PROJECTION_TYPE,
                PolarStereoGridCoverage.class);

        private final String projection;

        private final Class<? extends GridCoverage> coverageClass;

        private ProjectionType(String name,
                Class<? extends GridCoverage> coverageClass) {
            projection = name;
            this.coverageClass = coverageClass;
        }

        public String getProjection() {
            return projection;
        }

        /**
         * Get the {@link GridCoverage} for the projection.
         * 
         * @return the {@link GridCoverage}
         */
        public GridCoverage getGridCoverage() {
            return ReflectionUtil.newInstanceOfAssignableType(
                    GridCoverage.class, coverageClass);
        }
    }

    @XmlAttribute
    @DynamicSerializeElement
    private ProjectionType type;

    // This would wkt name
    @XmlElement
    @DynamicSerializeElement
    private String name;

    // This is CRS wkt
    @XmlElement
    @DynamicSerializeElement
    private String description;

    // / JAXB can't serialize this
    @Transient
    private CoordinateReferenceSystem crs;

    public Projection() {

    }

    public CoordinateReferenceSystem getCrs() {
        return crs;
    }

    public String getDescription() {
        return description;
    }

    public String getName() {
        return name;
    }

    public ProjectionType getType() {
        return type;
    }

    public void setCrs(CoordinateReferenceSystem crs) {
        this.crs = crs;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setType(ProjectionType type) {
        this.type = type;
    };
}
