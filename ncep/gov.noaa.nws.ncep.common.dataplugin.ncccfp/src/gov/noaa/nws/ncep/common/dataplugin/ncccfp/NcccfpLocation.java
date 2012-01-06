package gov.noaa.nws.ncep.common.dataplugin.ncccfp;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.hibernate.annotations.Type;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.adapters.GeometryAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * NCCCFP Location
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/05/09     155         F. J. Yen   From Raytheon's CCFP; mod for NCCCFP.  Fix for LINESTRING:
 * 										Change type of geometry from Polygon to Geometry.
 * 12/14/09		155			F. J. Yen	Updated from to11d3 to to11d6 (changed import of
 *                                      IspatialObject package to ...geospatial.ISpatialObject)
 * 05/26/10		155			F. J. Yen	Refactored from plugin for migration to to11dr11
 * 
 * </pre>
 * 
 * @author fjyen
 * @version 1
 */
@Embeddable
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcccfpLocation implements ISpatialObject {

    private static final long serialVersionUID = 8890315829188793187L;

    @DataURI(position = 0)
    @Column(name = "location", columnDefinition = "geometry")
    @Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    @DynamicSerializeElement
    private Geometry geometry;

    @Column(length = 600)
    @DynamicSerializeElement
    @XmlElement
    private String locationAll;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private double boxLat;

    @Column
    @DynamicSerializeElement
    @XmlElement
    private double boxLong;

    @Override
    public CoordinateReferenceSystem getCrs() {
        return null;
    }

    @Override
    public Geometry getGeometry() {
        return geometry;
    }

    @Override
    public Integer getNx() {
        return 0;
    }

    @Override
    public Integer getNy() {
        return 0;
    }

    public double getBoxLat() {
        return boxLat;
    }

    public void setBoxLat(double boxLat) {
        this.boxLat = boxLat;
    }

    public double getBoxLong() {
        return boxLong;
    }

    public void setBoxLong(double boxLong) {
        this.boxLong = boxLong;
    }

    public void setGeometry(Geometry geometry) {
        this.geometry = geometry;
    }

    public void setLocationAll(String locationAll) {
        this.locationAll = locationAll;
    }

    public String getLocationAll() {
        return locationAll;
    }
}
