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

package com.raytheon.uf.common.dataplugin.radar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.adapters.GeometryAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * 
 * Represents a radar station. This class maps to the radar_spatial table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial Check in
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "radar_spatial")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RadarStation extends PersistableDataObject implements
        ISpatialObject {

    private static final long serialVersionUID = 1L;

    @Column
    @Index(name = "radarNameIndex")
    @XmlAttribute
    @DynamicSerializeElement
    private String name;

    @Id
    @Column(name = "rda_id")
    @XmlAttribute
    @DynamicSerializeElement
    private String rdaId;

    @Column(name = "rpg_id_dec")
    @Index(name = "radarRpgIndex")
    @XmlAttribute
    @DynamicSerializeElement
    private String rpgIdDec;

    @Column(name = "immutablex")
    @XmlAttribute
    @DynamicSerializeElement
    private Float immutablEx;

    @Column(name = "wfo_id")
    @XmlAttribute
    @DynamicSerializeElement
    private String wfoId;

    @Column(name = "eqp_elv")
    @XmlAttribute
    @DynamicSerializeElement
    private Float eqpElv;

    @Column(name = "elevmeter")
    @XmlAttribute
    @DynamicSerializeElement
    private Float elevMeter;

    @Column(name = "lat")
    @XmlAttribute
    @DynamicSerializeElement
    private Float lat;

    @Column(name = "lon")
    @XmlAttribute
    @DynamicSerializeElement
    private Float lon;

    @Column(name = "the_geom", columnDefinition = "geometry")
    @Type(type = "com.raytheon.edex.db.objects.hibernate.GeometryType")
    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    @DynamicSerializeElement
    private Point station;

    public Float getLat() {
        return lat;
    }

    public void setLat(Float lat) {
        this.lat = lat;
    }

    public Float getLon() {
        return lon;
    }

    public void setLon(Float lon) {
        this.lon = lon;
    }

    @Override
    public Geometry getGeometry() {
        return station;
    }

    @Override
    public CoordinateReferenceSystem getCrs() {
        return null;
    }

    public String getRdaId() {
        return rdaId;
    }

    public void setRdaId(String rdaId) {
        this.rdaId = rdaId;
    }

    public String getRpgIdDec() {
        return rpgIdDec;
    }

    public void setRpgIdDec(String rpgIdDec) {
        this.rpgIdDec = rpgIdDec;
    }

    public Float getImmutablEx() {
        return immutablEx;
    }

    public void setImmutablEx(Float immutablEx) {
        this.immutablEx = immutablEx;
    }

    public String getWfoId() {
        return wfoId;
    }

    public void setWfoId(String wfoId) {
        this.wfoId = wfoId;
    }

    public Float getEqpElv() {
        return eqpElv;
    }

    public void setEqpElv(Float eqpElv) {
        this.eqpElv = eqpElv;
    }

    public Float getElevMeter() {
        return elevMeter;
    }

    public void setElevMeter(Float elevMeter) {
        this.elevMeter = elevMeter;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Point getStation() {
        return station;
    }

    public void setStation(Point station) {
        this.station = station;
    }

    @Override
    public Integer getNx() {
        return 0;
    }

    @Override
    public Integer getNy() {
        return 0;
    }
}
