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

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.subgrid.SubNcgrid;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Defines a Mercator grid coverage. This class is generally used to describe
 * grids described by GDS Template 10 as specified in Table 3.1
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "ncgrib_mercator_coverages")
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MercatorNcgridCoverage extends NcgridCoverage {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MercatorNcgridCoverage.class);

    private static final long serialVersionUID = 3140441023975157052L;

    /** The name of the projection */
    public static final String PROJECTION_TYPE = "Mercator";

    /** The minor axis of the earth */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double minorAxis;

    /** The major axis of the earth */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double majorAxis;

    /** Number of points along a parallel */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Integer nx;

    /** Number of points along a meridian */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Integer ny;

    /** Latitude of the first grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double la1;

    /** Longitude of the first grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double lo1;

    /**
     * latitude at which the Mercator projection intersects the Earth (Latitude
     * where Di and Dj are specified)
     */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double latin;

    /** Latitude of the last grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double la2;

    /** Longitude of the last grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double lo2;

    /** Longitudinal direction grid length */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double dx;

    /** Latitudinal direction grid length */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double dy;

    /** Spacing unit of dx and dy */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private String spacingUnit;

    @Override
    public int generateHash() {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(nx);
        hashBuilder.append(ny);
        hashBuilder.append(la1);
        hashBuilder.append(lo1);
        hashBuilder.append(latin);
        hashBuilder.append(la2);
        hashBuilder.append(lo2);
        hashBuilder.append(dx);
        hashBuilder.append(dy);
        return hashBuilder.toHashCode();
    }

    @Override
    public void initialize() throws GribException {
        double meridian = (lo1 + lo2) / 2;
        if (lo2 < lo1) {
            meridian += 180;
        }
        if (meridian > 180) {
            meridian -= 360;
        }
        crs = MapUtil.constructMercator(majorAxis, minorAxis, latin, meridian);
        crsWKT = crs.toWKT();
        try {
            // geometry = MapUtil.createGeometry(la1, lo1, la2, lo2);
            Unit<?> spacingUnitObj = Unit.valueOf(spacingUnit);
            if (spacingUnitObj.isCompatible(SI.METRE)) {
                UnitConverter converter = spacingUnitObj
                        .getConverterTo(SI.METRE);
                geometry = MapUtil.createGeometry(crs, la1, lo1,
                        converter.convert(dx), converter.convert(dy), nx, ny);
            } else {
                throw new GribException("Unable to convert " + spacingUnit
                        + " to meters while creating geometry!");
            }

        } catch (Exception e) {
            throw new GribException("Error creating geometry", e);
        }
        id = generateHash();

    }

    @Override
    public void generateName() {
        String nameAndDescription = "Unknown Mercator " + nx + " X " + ny + " "
                + Math.round(dx) + " " + spacingUnit + " "
                + getProjectionType() + " grid";
        String nameModel_gridid = "" + nx + ny
                + Integer.toString((int) (la1 + la2 + lo1 + lo2));
        this.setName(nameModel_gridid);
        // this.setName(nameAndDescription);
        this.setDescription(nameAndDescription);
    }

    @Override
    public NcgridCoverage trim(SubNcgrid subGrid) {
        MercatorNcgridCoverage rval = new MercatorNcgridCoverage();
        rval.description = this.description;
        rval.dx = this.dx;
        rval.dy = this.dy;
        rval.spacingUnit = this.spacingUnit;
        rval.latin = this.latin;
        rval.majorAxis = this.majorAxis;
        rval.minorAxis = this.minorAxis;
        int startX = subGrid.getStartX();
        int startY = subGrid.getStartY();
        rval.nx = subGrid.getNX();
        rval.ny = subGrid.getNY();

        // validate bounds are ok
        if (startX + rval.nx > this.nx) {
            rval.nx = this.nx - startX;
        }
        if (startY + rval.ny > this.ny) {
            rval.ny = this.ny - startY;
        }

        rval.setName(this.name + "SubGrid");

        try {
            Unit<?> spacingUnitObj = Unit.valueOf(spacingUnit);
            if (spacingUnitObj.isCompatible(SI.METRE)) {
                UnitConverter converter = spacingUnitObj
                        .getConverterTo(SI.METRE);
                double dxMeter = converter.convert(dx);
                double dyMeter = converter.convert(dy);
                MathTransform fromLatLon = MapUtil.getTransformFromLatLon(crs);
                MathTransform toLatLon = fromLatLon.inverse();
                // get UL
                boolean thisLL = (this.la1 < this.la2);
                double upperLat = thisLL ? this.la2 : this.la1;
                double[] tmp1 = new double[] { this.lo1, upperLat, 0.0, 0.0 };
                double[] tmp2 = new double[4];
                fromLatLon.transform(tmp1, 0, tmp2, 0, 1);
                // tmp[0/1] is UL
                tmp2[0] += dxMeter * startX;
                tmp2[1] -= dyMeter * startY;
                // tmp[2/3] is LR
                tmp2[2] = tmp2[0] + dxMeter * rval.nx;
                tmp2[3] = tmp2[1] - dyMeter * rval.ny;
                toLatLon.transform(tmp2, 0, tmp1, 0, 2);
                rval.setLo1(tmp1[0]);
                rval.setLo2(tmp1[2]);

                if (thisLL) {
                    rval.setLa1(tmp1[3]);
                    rval.setLa2(tmp1[1]);
                } else {
                    rval.setLa1(tmp1[1]);
                    rval.setLa2(tmp1[3]);
                }
                rval.setId(rval.hashCode());
            } else {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Error creating sub grid definition ["
                                + subGrid.getModelName()
                                + "], units are not compatible with meter ["
                                + spacingUnit + "]");
                rval = null;
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating sub grid definition", e);
            rval = null;
        }

        return rval;
    }

    @Override
    public String getProjectionType() {
        return PROJECTION_TYPE;
    }

    public Integer getNx() {
        return nx;
    }

    public Integer getNy() {
        return ny;
    }

    /**
     * Sets nx
     * 
     * @param nx
     *            The nx value
     */
    public void setNx(Integer nx) {
        this.nx = nx;
    }

    /**
     * Set ny
     * 
     * @param ny
     *            The ny value
     */
    public void setNy(Integer ny) {
        this.ny = ny;
    }

    /**
     * Gets la1
     * 
     * @return The la1 latitude value
     */
    public double getLa1() {
        return la1;
    }

    /**
     * Set la1
     * 
     * @param la1
     *            The la1 latitude value
     */
    public void setLa1(double la1) {
        this.la1 = la1;
    }

    /**
     * Gets lo1
     * 
     * @return The lo1 value
     */
    public double getLo1() {
        return lo1;
    }

    /**
     * Sets lo1
     * 
     * @param lo1
     *            The l1 value
     */
    public void setLo1(double lo1) {
        this.lo1 = lo1;
    }

    /**
     * Gets latin
     * 
     * @return The latin latitude value
     */
    public double getLatin() {
        return latin;
    }

    /**
     * Sets latin
     * 
     * @param latin
     *            The latin latitude value
     */
    public void setLatin(double latin) {
        this.latin = latin;
    }

    /**
     * Gets la2
     * 
     * @return The la2 latitude value
     */
    public double getLa2() {
        return la2;
    }

    /**
     * Sets la2
     * 
     * @param la2
     *            The la2 latitude value
     */
    public void setLa2(double la2) {
        this.la2 = la2;
    }

    /**
     * Gets lo2
     * 
     * @return The lo2 latitude value
     */
    public double getLo2() {
        return lo2;
    }

    /**
     * Sets lo2
     * 
     * @param lo2
     *            The lo2 longitude value
     */
    public void setLo2(double lo2) {
        this.lo2 = lo2;
    }

    /**
     * Gets dx
     * 
     * @return The dx value
     */
    public double getDx() {
        return dx;
    }

    /**
     * Sets dx
     * 
     * @param dx
     *            The dx value
     */
    public void setDx(double dx) {
        this.dx = dx;
    }

    /**
     * Gets dy
     * 
     * @return The dy value
     */
    public double getDy() {
        return dy;
    }

    /**
     * Sets dy
     * 
     * @param dy
     *            The dy value
     */
    public void setDy(double dy) {
        this.dy = dy;
    }

    /**
     * Gets the dx/dy spacing unit
     * 
     * @return The dx/dy spacing unit
     */
    public String getSpacingUnit() {
        return spacingUnit;
    }

    /**
     * Sets the dx/dy spacing unit
     * 
     * @param spacingUnit
     *            The dx/dy spacing unit
     */
    public void setSpacingUnit(String spacingUnit) {
        this.spacingUnit = spacingUnit;
    }

    /**
     * Gets the minor axis
     * 
     * @return The minor axis value
     */
    public double getMinorAxis() {
        return minorAxis;
    }

    /**
     * Sets the minor axis
     * 
     * @param minorAxis
     *            The minor axis value
     */
    public void setMinorAxis(double minorAxis) {
        this.minorAxis = minorAxis;
    }

    /**
     * Gets the major axis
     * 
     * @return The major axis value
     */
    public double getMajorAxis() {
        return majorAxis;
    }

    /**
     * Sets the major axis
     * 
     * @param majorAxis
     *            The major axis value
     */
    public void setMajorAxis(double majorAxis) {
        this.majorAxis = majorAxis;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        MercatorNcgridCoverage other = (MercatorNcgridCoverage) obj;
        if (Double.doubleToLongBits(dx) != Double.doubleToLongBits(other.dx))
            return false;
        if (Double.doubleToLongBits(dy) != Double.doubleToLongBits(other.dy))
            return false;
        if (Double.doubleToLongBits(la1) != Double.doubleToLongBits(other.la1))
            return false;
        if (Double.doubleToLongBits(la2) != Double.doubleToLongBits(other.la2))
            return false;
        if (Double.doubleToLongBits(latin) != Double
                .doubleToLongBits(other.latin))
            return false;
        if (Double.doubleToLongBits(lo1) != Double.doubleToLongBits(other.lo1))
            return false;
        if (Double.doubleToLongBits(lo2) != Double.doubleToLongBits(other.lo2))
            return false;
        if (Double.doubleToLongBits(majorAxis) != Double
                .doubleToLongBits(other.majorAxis))
            return false;
        if (Double.doubleToLongBits(minorAxis) != Double
                .doubleToLongBits(other.minorAxis))
            return false;
        if (nx == null) {
            if (other.nx != null)
                return false;
        } else if (!nx.equals(other.nx))
            return false;
        if (ny == null) {
            if (other.ny != null)
                return false;
        } else if (!ny.equals(other.ny))
            return false;
        if (spacingUnit == null) {
            if (other.spacingUnit != null)
                return false;
        } else if (!spacingUnit.equals(other.spacingUnit))
            return false;
        return true;
    }
}
