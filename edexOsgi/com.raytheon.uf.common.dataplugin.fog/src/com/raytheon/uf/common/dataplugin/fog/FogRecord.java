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
package com.raytheon.uf.common.dataplugin.fog;

import java.util.Calendar;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.hibernate.annotations.Index;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.fog.analysis.FogRange;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for Fog plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/12/09                 D. Hladky   Initial release
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 04/08/13     1293        bkowal      Removed references to hdffileid.
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "fogseq")
@Table(name = "fog", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "fog",
		indexes = {
				@Index(name = "fog_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class FogRecord extends PersistablePluginDataObject
        implements IPersistable {

    private static final long serialVersionUID = 76774564365671L;

    @Column(length = 7)
    @DataURI(position = 1)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    private String cwa;

    // Time of the observation to the nearest hour.
    @Column
    @DynamicSerializeElement
    @XmlElement
    private Calendar refHour;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public int nx = 0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public int ny = 0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public float dx = 0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public float dy = 0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public double satLon = 0.0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public double satHeight = 0.0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public double lon = 0.0;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    public double lat = 0.0;

    @Transient
    private int[] vis_array = null;

    @Transient
    private int[] ir3_9_array = null;

    @Transient
    private int[] ir10_7_array = null;

    @Transient
    private FogRange[] vis_ranges = null;

    @Transient
    private FogRange[] ir_ranges = null;

    @Transient
    private IMAGE_GROUP[] groups = null;

    @Transient
    private float[] threats = null;

    @Transient
    private GridGeometry2D gridGeometry2D = null;

    /**
     * Default Constructor
     */
    public FogRecord() {
    }

    /**
     * Constructs a record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public FogRecord(String uri) {
        super(uri);
    }

    /**
     * 
     * Enumeration of the CHANNELS in Fog analysis
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum CHANNEL {

        VIS("VIS"), IR3_9("IR3_9"), IR10_7("IR10_7");

        private final String channel;

        private CHANNEL(String name) {
            channel = name;
        }

        public String getChannel() {
            return channel;
        }
    };

    /**
     * 
     * Enumeration of the Image Groups in Fog analysis
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum IMAGE_GROUP {

        TWILIGHT_GROUP("TWILIGHT_GROUP"), VIS_GROUP("VIS_GROUP"), IR_GROUP(
                "IR_GROUP");

        private final String group;

        private IMAGE_GROUP(String name) {
            group = name;
        }

        public String getImageGroup() {
            return group;
        }
    };

    /**
     * 
     * Enumeration of the possible outcomes in Fog analysis
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum FOG_THREAT {

        BLACK("BLACK"), GRAY("GRAY"), GREEN("GREEN"), YELLOW("YELLOW"), RED(
                "RED");

        private final String threat;

        private FOG_THREAT(String name) {
            threat = name;
        }

        public String getThreat() {
            return threat;
        }
    };

    /**
     * Set the cwa
     * 
     * @param cwa
     */
    public void setCwa(String cwa) {
        this.cwa = cwa;
    }

    /**
     * Gets the cwa
     * 
     * @return
     */
    public String getCwa() {
        return cwa;
    }

    /**
     * set NX
     * 
     * @param nx
     */
    public void setNx(Integer nx) {
        this.nx = nx;
    }

    /**
     * Get nx
     * 
     * @return
     */
    public Integer getNx() {
        return nx;
    }

    /**
     * Set NY
     * 
     * @param ny
     */
    public void setNy(Integer ny) {
        this.ny = ny;
    }

    /**
     * Get NY
     * 
     * @param ny
     */
    public Integer getNy() {
        return ny;
    }

    /**
     * set DX in meters
     * 
     * @param f
     */
    public void setDx(float dx) {
        this.dx = dx;
    }

    /**
     * Get DX in meters
     * 
     * @return
     */
    public float getDx() {
        return dx;
    }

    /**
     * Set the DY, in meters
     * 
     * @param dy
     */
    public void setDy(float dy) {
        this.dy = dy;
    }

    /**
     * get the DY, in meters
     * 
     * @return
     */
    public float getDy() {
        return dy;
    }

    /**
     * satLon in degrees
     * 
     * @param dy
     */
    public void setSatLon(double satLon) {
        this.satLon = satLon;
    }

    /**
     * get the satLon in degrees
     * 
     * @return
     */
    public double getSatLon() {
        return satLon;
    }

    /**
     * satHeight in km
     * 
     * @param dy
     */
    public void setSatHeight(double satHeight) {
        this.satHeight = satHeight;
    }

    /**
     * get the satHeight in km
     * 
     * @return
     */
    public double getSatHeight() {
        return satHeight;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    public double getLon() {
        return lon;
    }

    public void setLon(double lon) {
        this.lon = lon;
    }

    /**
     * Get the IDecoderGettable reference for this record.
     * 
     * @return The IDecoderGettable reference for this record. Null for this
     *         class.
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    /**
     * Set the VIS pixel array
     * 
     * @param vis_array
     */
    public void setVisArray(int[] vis_array) {
        this.vis_array = vis_array;
    }

    /**
     * Get the VIS pixel array
     * 
     * @param data_array
     */
    public int[] getVisArray() {
        return vis_array;
    }

    public GridGeometry2D getGridGeometry2D() {
		return gridGeometry2D;
	}

	public void setGridGeometry2D(GridGeometry2D gridGeometry2D) {
		this.gridGeometry2D = gridGeometry2D;
	}

	/**
     * Set the VIS pixel array
     * 
     * @param data_array
     */
    public void setIR_3_9Array(int[] ir3_9_array) {
        this.ir3_9_array = ir3_9_array;
    }

    /**
     * Get the VIS pixel array
     * 
     * @param data_array
     */
    public int[] getIR_3_9Array() {
        return ir3_9_array;
    }

    /**
     * Set the VIS data array
     * 
     * @param data_array
     */
    public void setIR_10_7Array(int[] ir10_7_array) {
        this.ir10_7_array = ir10_7_array;
    }

    /**
     * Get the VIS data array
     * 
     * @param data_array
     */
    public int[] getIR_10_7Array() {
        return ir10_7_array;
    }

    /**
     * Set the threat array
     * 
     * @param data_array
     */
    public void setThreats(float[] threats) {
        this.threats = threats;
    }

    /**
     * Get the threat array
     * 
     * @param data_array
     */
    public float[] getThreats() {
        return threats;
    }

    /**
     * Gets the projected CRS
     * 
     * @return
     */
    public ProjectedCRS getCRS() {
        return MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS, getLat(), getLon());
    }

    /**
     * Construct a 2D GridGeometry to use for display
     * 
     * @return
     */
    public GridGeometry2D getGridGeometry() {
        if (gridGeometry2D == null) {

            ProjectedCRS crs = this.getCRS();

            GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
            generalEnvelope.setCoordinateReferenceSystem(crs);

            double maxExtentX = (this.getDx() * (this.getNx() / 2));
            double maxExtentY = (this.getDy() * (this.getNy() / 2));

            generalEnvelope.setRange(0, -maxExtentX, maxExtentX);
            generalEnvelope.setRange(1, -maxExtentY, maxExtentY);

            gridGeometry2D = new GridGeometry2D(new GeneralGridEnvelope(
                    new int[] { 0, 0 },
                    new int[] { this.getNx(), this.getNy() }, false),
                    generalEnvelope);
        }

        return gridGeometry2D;
    }

    /**
     * Sets the data array from the store.
     * 
     * @param dataStore
     */
    public void retrieveFromDataStore(IDataStore dataStore) {

        try {
            for (CHANNEL channel : CHANNEL.values()) {
                IntegerDataRecord dataRec = (IntegerDataRecord) dataStore
                        .retrieve(getDataURI(), channel.getChannel(),
                                Request.ALL);
                if (dataRec.getIntData().length > 0) {
                    if (channel == CHANNEL.VIS) {
                        setVisArray(dataRec.getIntData());
                    } else if (channel == CHANNEL.IR3_9) {
                        setIR_3_9Array(dataRec.getIntData());
                    } else if (channel == CHANNEL.IR10_7) {
                        setIR_10_7Array(dataRec.getIntData());
                    }
                }
            }

        } catch (Exception se) {
            se.printStackTrace();
        }
    }

    /**
     * Used for debugging.
     */
    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("\n dataURI: " + getDataURI() + "\n");
        sb.append("WFO: " + getCwa() + "\n");
        sb.append("dataTime: "
                + getDataTime().getValidTime().getTime().toString() + "\n");
        // sb.append("PersistanceTime: " + getPersistenceTime().toString() +
        // "\n");
        sb.append("Nx: " + getNx() + "\n");
        sb.append("Ny: " + getNy() + "\n");
        sb.append("Dx: " + getDx() + "\n");
        sb.append("Dy: " + getDy() + "\n");
        sb.append("lat: " + getLat() + "\n");
        sb.append("lon: " + getLon() + "\n");

        return sb.toString();
    }

    public IMAGE_GROUP getRangeType(int j) {
        return getGroups()[j];
    }

    public void setRangeType(IMAGE_GROUP group, int j) {
        getGroups()[j] = group;
    }

    public FogRange getVisRange(int j) {
        return getVisRanges()[j];
    }

    public void setVisRange(FogRange range, int j) {
        getVisRanges()[j] = range;
    }

    public FogRange getIRRange(int j) {
        return getIRRanges()[j];
    }

    public void setIRRange(FogRange range, int j) {
        getIRRanges()[j] = range;
    }

    private IMAGE_GROUP[] getGroups() {
        if (groups == null) {
            groups = new IMAGE_GROUP[getNy()];
        }
        return groups;
    }

    private FogRange[] getIRRanges() {
        if (ir_ranges == null) {
            ir_ranges = new FogRange[getNy()];
        }
        return ir_ranges;
    }

    private FogRange[] getVisRanges() {
        if (vis_ranges == null) {
            vis_ranges = new FogRange[getNy()];
        }
        return vis_ranges;
    }

    // ---------------------------------------------------------------------------
    // Name: FindGroup
    // Type: public member function
    //
    // Description:
    // Given a point with i and j, the function finds out whether the point
    // is in daylight area, nighttime area or twilight area.
    // Input Argument:
    // i: row number
    // j: col number
    // Output Argument:
    // Return the ImageGroup the point belonging to.
    // History:
    // March 2004 ------ Qin Zeng(GDMB/MDL) created
    // Dec 2009 D Hladky ported to JAVA
    // ---------------------------------------------------------------------------
    public IMAGE_GROUP findGroup(int i, int j) {
        // Explain the _rangeType[i] here:

        // (1) If rangType[i] == TWILIGHT_GROUP
        // then means vis_range[i] stores range of vis and
        // ir_range[i] stores range of ir
        // (2) if rangType[i] == IR_GROUP
        // then means both vis_range and ir_range store range of ir
        //
        // (3) if rangType[i] == VIS_GROUP
        // then means both vis_range and ir_range store range of vis

        if (getRangeType(j) == IMAGE_GROUP.TWILIGHT_GROUP) {
            if (i <= getVisRange(j).getEnd() && i >= getVisRange(j).getStart()) {
                return IMAGE_GROUP.VIS_GROUP;
            }

            else if (i <= getIRRange(j).getEnd()
                    && i >= getIRRange(j).getStart()) {
                return IMAGE_GROUP.IR_GROUP;
            } else {
                return IMAGE_GROUP.TWILIGHT_GROUP;
            }
        } else if (getRangeType(j) == IMAGE_GROUP.VIS_GROUP) {
            if ((i <= getVisRange(j).getEnd() && i >= getVisRange(j).getStart())
                    || (i <= getIRRange(j).getEnd() && i >= getIRRange(j)
                            .getStart())) {
                return IMAGE_GROUP.VIS_GROUP;
            } else {
                return IMAGE_GROUP.TWILIGHT_GROUP;
            }
        } else {
            if ((i <= getVisRange(j).getEnd() && i >= getVisRange(j).getStart())
                    || (i <= getIRRange(j).getEnd() && i >= getIRRange(j)
                            .getStart())) {
                return IMAGE_GROUP.IR_GROUP;
            } else {
                return IMAGE_GROUP.TWILIGHT_GROUP;
            }
        }
    }

    /**
     * name for the lower right corner display
     * 
     * @return
     */
    public String getName() {
        return "Fog " + getCwa() + " Threat Level";
    }

    /**
     * @param refHour
     *            the refHour to set
     */
    public void setRefHour(Calendar refHour) {
        this.refHour = refHour;
    }

    /**
     * @return the refHour
     */
    public Calendar getRefHour() {
        return refHour;
    }
    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
