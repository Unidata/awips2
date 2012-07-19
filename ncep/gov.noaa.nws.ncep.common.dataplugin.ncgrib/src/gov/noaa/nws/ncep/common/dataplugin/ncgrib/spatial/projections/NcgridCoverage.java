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

package gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.CRS;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.subgrid.SubNcgrid;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Base class for encapsulating grib spatial information
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 5/23/12                  xguo        Merged 4 coverage tables into one
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class NcgridCoverage extends PersistableDataObject implements
        ISpatialObject {

    private static final long serialVersionUID = -1355232934065074837L;

    /** The id for this grid. This value is generated in the initialize method **/
    @Id
    @DynamicSerializeElement
    protected int id;

    /** The name of the grid */
    @Column(length = 2047)
    @XmlElement
    @DynamicSerializeElement
    protected String name;

    /** A description of the grid coverage */
    @Column(length = 3071)
    @XmlElement
    @DynamicSerializeElement
    protected String description;

    /** Geometry object holding the corner points of the grid */
    @Column(name = "the_geom", columnDefinition = "geometry")
    @Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
    @DynamicSerializeElement
    protected Polygon geometry;

    /** The CRS as a WKT String */
    @Column(name = "crs", length = 2047)
    @DynamicSerializeElement
    protected String crsWKT;

    /** The CRS object */
    @Transient
    protected CoordinateReferenceSystem crs;

    /** The Grid geometry */
    @Transient
    protected transient GridGeometry2D gridGeometry;

    /**
     * Creates an empty GridCoverage object
     */
    protected NcgridCoverage() {

    }

    @Override
    public String toString() {
        return "Coverage Information Not Specified yet";
    }

    @Override
    public int hashCode() {
        return generateHash();
    }

    /**
     * Generates a hash code based on selected fields in the grid coverage
     * object. The fields used will vary among different projections.
     * 
     * @return The hash code generated from selected fields in the object
     */
    public abstract int generateHash();

    /**
     * Initializes the grib coverage object. Initialization should entail
     * creation of the crs and geometry object as well as assigning the id field
     * 
     * @throws GribException
     *             If problems occur while creating the crs, geometry, or the id
     */
    public abstract void initialize() throws GribException;

    /**
     * Gets the name of the projection. The projection type is specified by each
     * subclass and accessed through this method.
     * 
     * @return The name/type of the projection
     */
    public abstract String getProjectionType();

    /**
     * If this grid coverage object describes a grid that is not predefined,
     * this method is used to generate and assign a descriptive name.
     */
    public abstract void generateName();

    /**
     * Trim this GridCoverage to a sub grid.
     * 
     * @param subGrid
     * @return
     */
    public abstract NcgridCoverage trim(SubNcgrid subGrid);

    public Polygon getGeometry() {
        return geometry;
    }

    public CoordinateReferenceSystem getCrs() {
        if (crs == null) {
            try {
                this.crs = CRS.parseWKT(crsWKT);
            } catch (FactoryException e) {
                this.crs = null;
            }
        }
        return crs;
    }

    /**
     * Gets the id
     * 
     * @return The id
     */
    public int getId() {
        return id;
    }

    /**
     * Sets the id
     * 
     * @param id
     *            The id
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * Gets the name
     * 
     * @return The name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name
     * 
     * @param name
     *            The name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Sets the geometry
     * 
     * @param geometry
     *            The geometry
     */
    public void setGeometry(Geometry geometry) {
        this.geometry = (Polygon) geometry;
    }

    /**
     * Sets the CRS object
     * 
     * @param crs
     *            The crs object
     */
    public void setCrs(CoordinateReferenceSystem crs) {
        this.crs = crs;
    }

    /**
     * Gets the CRS WKT object
     * 
     * @return The CRS WKT object
     */
    public String getCrsWKT() {
        return crsWKT;
    }

    /**
     * Sets the CRS WKT object
     * 
     * @param crsWKT
     */
    public void setCrsWKT(String crsWKT) {
        this.crsWKT = crsWKT;

    }

    /**
     * Gets the description
     * 
     * @return The description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the description
     * 
     * @param description
     *            The description
     */
    public void setDescription(String description) {
        this.description = description;
    }

    public GridGeometry2D getGridGeometry() {
        if (gridGeometry == null) {
            gridGeometry = MapUtil.getGridGeometry(this);
        }

        return gridGeometry;
    }

    public void setGridGeometry(GridGeometry2D gridGeometry) {
        this.gridGeometry = gridGeometry;
    }
}
