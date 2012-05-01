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
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.referencing.datum.DefaultGeodeticDatum;
import org.geotools.referencing.datum.DefaultPrimeMeridian;
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
 * Defines a Lat/Lon grid coverage. This class is generally used to describe
 * grids described by GDS Template 0 as specified in Table 3.1
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 9/08/11                  X. Guo		Skip correct Lon value after add new column
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "ncgrib_latlon_coverages")
@Cache(usage = CacheConcurrencyStrategy.TRANSACTIONAL)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class LatLonNcgridCoverage extends NcgridCoverage {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LatLonNcgridCoverage.class);

    private static final long serialVersionUID = 8371251040172233074L;

    private static final DefaultGeographicCRS WGS84 = DefaultGeographicCRS.WGS84;

    /** The name of the projection */
    public static final String PROJECTION_TYPE = "LatLon";

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

    /** Latitude of first grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double la1;

    /** Longitude of the first grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double lo1;

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

    /** I direction increment */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double dx;

    /** J direction increment */
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
        if (getName() == null) {
            generateName();
        }
        hashBuilder.append(name);
        hashBuilder.append(nx);
        hashBuilder.append(ny);
        hashBuilder.append(la1);
        hashBuilder.append(lo1);
        hashBuilder.append(la2);
        hashBuilder.append(lo2);
        return hashBuilder.toHashCode();
    }

    @Override
    public void initialize() throws GribException {
        double maxLon;
        double minLat = MapUtil.correctLat(la1);
        double maxLat = MapUtil.correctLat(la2);
        double minLon = MapUtil.correctLon(lo1);
        if (lo2 >= 360.0) {
            maxLon = lo2;
        } else {
            maxLon = MapUtil.correctLon(lo2);
        }
        if (maxLon < minLon) {
            maxLon += 360.0;
        }
        if (maxLon > 180) {
            crs = new DefaultGeographicCRS(new DefaultGeodeticDatum("WGS84",
                    WGS84.getDatum().getEllipsoid(), new DefaultPrimeMeridian(
                            "DateLine", 180.0)), WGS84.getCoordinateSystem());
        } else {
            crs = WGS84;
        }
        crsWKT = crs.toWKT();
        try {
            double xOffset = dx * 0.5;
            double yOffset = dy * 0.5;
            geometry = MapUtil.createGeometry(minLat + yOffset, minLon
                    - xOffset, maxLat - yOffset, maxLon + xOffset);
        } catch (Exception e) {
            throw new GribException("Error creating geometry", e);
        }
        id = generateHash();
    }

    @Override
    public void generateName() {

        String nameModel_gridid = "" + (int) (la1 * 10.)
                + (int) ((lo1 + la2 + lo2) * 100.);
        String nameAndDescription = "Unknown LatLon " + nx + " X " + ny + " "
                + nameModel_gridid + " " + getProjectionType() + " grid";
        // System.out.println(" nameModel_gridid=" + nameModel_gridid);
        this.setName(nameModel_gridid);
        // this.setName(nameAndDescription);
        this.setDescription(nameAndDescription);

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

    @Override
    public NcgridCoverage trim(SubNcgrid subGrid) {
        LatLonNcgridCoverage rval = new LatLonNcgridCoverage();
        rval.description = this.description;
        rval.dx = this.dx;
        rval.dy = this.dy;
        rval.spacingUnit = this.spacingUnit;
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

        rval.name = this.name + "SubGrid";

        try {
            if (spacingUnit.equals("degree")) {
                // refigure corner points directly using degrees
                rval.lo1 = this.lo1 + dx * startX;
                rval.lo2 = rval.lo1 + dx * rval.nx;

                if (this.la1 < this.la2) {
                    // this is LL, subgrid is UL
                    rval.la1 = this.la2 - dy * (startY + rval.ny);
                    rval.la2 = rval.la1 + dy * rval.ny;
                } else {
                    // this is UL, subgrid is UL
                    rval.la1 = this.la1 - dy * startY;
                    rval.la2 = rval.la1 - dy * rval.ny;
                }
            } else {
                Unit<?> spacingUnitObj = Unit.valueOf(spacingUnit);
                if (spacingUnitObj.isCompatible(SI.METRE)) {
                    UnitConverter converter = spacingUnitObj
                            .getConverterTo(SI.METRE);
                    double dxMeter = converter.convert(dx);
                    double dyMeter = converter.convert(dy);
                    MathTransform fromLatLon = MapUtil
                            .getTransformFromLatLon(crs);
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
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Error creating sub grid definition ["
                                            + subGrid.getModelName()
                                            + "], units are not compatible with meter ["
                                            + spacingUnit + "]");
                    rval = null;
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating sub grid definition", e);
            rval = null;
        }

        return rval;
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
     * Sets ny
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
     * Sets la1
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
     * @return The lo1 longitude value
     */
    public double getLo1() {
        return lo1;
    }

    /**
     * Sets lo1
     * 
     * @param lo1
     *            The lo1 longitude value
     */
    public void setLo1(double lo1) {
        this.lo1 = lo1;
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
     * Sets la1
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
     * @return The lo2 longitude value
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
     */
    public void setSpacingUnit(String spacingUnit) {
        this.spacingUnit = spacingUnit;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        LatLonNcgridCoverage other = (LatLonNcgridCoverage) obj;
        if (Double.doubleToLongBits(dx) != Double.doubleToLongBits(other.dx))
            return false;
        if (Double.doubleToLongBits(dy) != Double.doubleToLongBits(other.dy))
            return false;
        if (Double.doubleToLongBits(la1) != Double.doubleToLongBits(other.la1))
            return false;
        if (Double.doubleToLongBits(la2) != Double.doubleToLongBits(other.la2))
            return false;
        if (Double.doubleToLongBits(lo1) != Double.doubleToLongBits(other.lo1))
            return false;
        if (Double.doubleToLongBits(lo2) != Double.doubleToLongBits(other.lo2))
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
