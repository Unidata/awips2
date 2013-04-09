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
package com.raytheon.uf.common.dataplugin.npp.viirs;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.npp.viirs.projection.VIIRSMapProjectionFactory;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Geometry;

/**
 * VIIRS geographic data record object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2011            mschenke     Initial creation
 * Feb 21, 2012  #30      mschenke     Removed unused envelopePercentage field
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@Entity
@Table(name = "viirs_spatial")
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "viirs_spatial",
		indexes = {
				@Index(name = "viirs_spatial_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@DynamicSerialize
public class VIIRSSpatialCoverage extends PersistableDataObject implements
        ISpatialObject {

    private static final long serialVersionUID = -2532225158997059309L;

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @DynamicSerializeElement
    private int gid;

    @DynamicSerializeElement
    @Embedded
    private DataTime dataTime;

    /** Number of points along the x-axis */
    @DynamicSerializeElement
    @Column
    private Integer nx;

    /** Number of points along the y-axis */
    @DynamicSerializeElement
    @Column
    private Integer ny;

    /** The horizontal resolution of the grid */
    @Column
    @DynamicSerializeElement
    private Float dx;

    /** The vertical resolution of the grid */
    @Column
    @DynamicSerializeElement
    private Float dy;

    @Column
    @DynamicSerializeElement
    private float[] centerLatitudes;

    @Column
    @DynamicSerializeElement
    private float[] centerLongitudes;

    @Column
    @DynamicSerializeElement
    private float[] directions;

    @Transient
    private CoordinateReferenceSystem crs;

    @Column
    @DynamicSerializeElement
    private Geometry envelope;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.geospatial.ISpatialObject#getGeometry()
     */
    @Override
    public Geometry getGeometry() {
        return envelope;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.geospatial.ISpatialObject#getCrs()
     */
    @Override
    public CoordinateReferenceSystem getCrs() {
        if (crs == null) {
            try {
                crs = VIIRSMapProjectionFactory.construct(this);
            } catch (FactoryException e) {
                crs = null;
            }
        }
        return crs;
    }

    public GridGeometry2D getGridGeometry() {
        double widthBy2 = (getNx() * getDx()) / 2.0;
        double[] lowRange = new double[] { -widthBy2, 0 };
        double[] highRange = new double[] { widthBy2, getNy() * getDy() };

        GeneralEnvelope env = new GeneralEnvelope(lowRange, highRange);
        env.setCoordinateReferenceSystem(getCrs());
        return new GridGeometry2D(new GeneralGridEnvelope(new int[] { 0, 0 },
                new int[] { getNx(), getNy() }, false), env);
    }

    /**
     * @return the centerLatitudes
     */
    public float[] getCenterLatitudes() {
        return centerLatitudes;
    }

    /**
     * @param centerLatitudes
     *            the centerLatitudes to set
     */
    public void setCenterLatitudes(float[] centerLatitudes) {
        this.centerLatitudes = centerLatitudes;
    }

    /**
     * @return the centerLongitudes
     */
    public float[] getCenterLongitudes() {
        return centerLongitudes;
    }

    /**
     * @param centerLongitudes
     *            the centerLongitudes to set
     */
    public void setCenterLongitudes(float[] centerLongitudes) {
        this.centerLongitudes = centerLongitudes;
    }

    /**
     * @return the directions
     */
    public float[] getDirections() {
        return directions;
    }

    /**
     * @param directions
     *            the directions to set
     */
    public void setDirections(float[] directions) {
        this.directions = directions;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.geospatial.ISpatialObject#getNx()
     */
    @Override
    public Integer getNx() {
        return nx;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.geospatial.ISpatialObject#getNy()
     */
    @Override
    public Integer getNy() {
        return ny;
    }

    /**
     * @return the gid
     */
    public int getGid() {
        return gid;
    }

    /**
     * @param gid
     *            the gid to set
     */
    public void setGid(int gid) {
        this.gid = gid;
    }

    /**
     * @return the dataTime
     */
    public DataTime getDataTime() {
        return dataTime;
    }

    /**
     * @param dataTime
     *            the dataTime to set
     */
    public void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
    }

    /**
     * @return the dx
     */
    public Float getDx() {
        return dx;
    }

    /**
     * @param dx
     *            the dx to set
     */
    public void setDx(Float dx) {
        this.dx = dx;
    }

    /**
     * @return the dy
     */
    public Float getDy() {
        return dy;
    }

    /**
     * @param dy
     *            the dy to set
     */
    public void setDy(Float dy) {
        this.dy = dy;
    }

    /**
     * @return the envelope
     */
    public Geometry getEnvelope() {
        return envelope;
    }

    /**
     * @param envelope
     *            the envelope to set
     */
    public void setEnvelope(Geometry envelope) {
        this.envelope = envelope;
    }

    /**
     * @param nx
     *            the nx to set
     */
    public void setNx(Integer nx) {
        this.nx = nx;
    }

    /**
     * @param ny
     *            the ny to set
     */
    public void setNy(Integer ny) {
        this.ny = ny;
    }

}
