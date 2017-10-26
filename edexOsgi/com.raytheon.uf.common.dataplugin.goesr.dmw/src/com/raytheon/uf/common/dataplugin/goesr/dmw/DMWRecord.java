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
package com.raytheon.uf.common.dataplugin.goesr.dmw;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * GOES-R Derived Motion Wind Record. Updated to include "filter" DB column for
 * pressure-based filtering of GOES-R/Himawari DMWs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2015  4334       nabowle     Initial creation
 * July 14, 2016  19051   mcomerford   Added "filter" field (DCS 19051)
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "dmwseq")
@Table(name = DMWRecord.PLUGIN_NAME, uniqueConstraints = { @UniqueConstraint(columnNames = {
        "orbitalSlot", "scene", "channel", "refTime", "latitude", "longitude", "filter" }) })
@org.hibernate.annotations.Table(appliesTo = DMWRecord.PLUGIN_NAME, indexes = {
        @Index(name = "%TABLE%_filterandwspd_index", columnNames = {"filter", "windspd"}) })
@DynamicSerialize
public class DMWRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, IPersistable {

    /** Serializable id */
    private static final long serialVersionUID = 1L;

    public static final String PLUGIN_NAME = "dmw";

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    @Embedded
    @DataURI(position = 1, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @DataURI(position = 2)
    @DynamicSerializeElement
    @Column(nullable = false, length = 20)
    private String orbitalSlot;

    @DataURI(position = 3)
    @DynamicSerializeElement
    @Column(nullable = false, length = 20)
    private String scene;

    @DataURI(position = 4)
    @DynamicSerializeElement
    @Column(nullable = false)
    private int channel;

    /**
     * Required to be in the DataURI for viz updates, but not used for the
     * unique constraint.
     */
    @DataURI(position = 5)
    @DynamicSerializeElement
    @Column
    private float windSpd;

    @DynamicSerializeElement
    @Column
    private float windDir;

    /**
     * Allows for filtering based off of a defined NetCDF-4 Variable
     * (e.g. Pressure, Altitude, etc.)
     */
    @Column
    @DynamicSerializeElement
    private Float filter;

    /**
     * Constructor.
     */
    public DMWRecord() {
        super();
        pointDataView = new PointDataView();
    }

    /**
     * Constructs a record from a dataURI
     *
     * @param uri
     *            The dataURI
     */
    public DMWRecord(String uri) {
        super(uri);
        pointDataView = new PointDataView();
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.dataplugin.PluginDataObject#getPluginName()
     */
    @Override
    public String getPluginName() {
        return PLUGIN_NAME;
    }

    @Override
    public ISpatialObject getSpatialObject() {
        return location;
    }

    /**
     * @return the location
     */
    public SurfaceObsLocation getLocation() {
        return location;
    }

    /**
     * @param location
     *            the location to set
     */
    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    /**
     * @return the channel
     */
    public int getChannel() {
        return channel;
    }

    /**
     * @param channel
     *            the channel to set
     */
    public void setChannel(int channel) {
        this.channel = channel;
    }

    /**
     * @return the orbitalSlot
     */
    public String getOrbitalSlot() {
        return orbitalSlot;
    }

    /**
     * @param orbitalSlot
     *            the orbitalSlot to set
     */
    public void setOrbitalSlot(String orbitalSlot) {
        this.orbitalSlot = orbitalSlot;
    }

    /**
     * @return the scene
     */
    public String getScene() {
        return scene;
    }

    /**
     * @param scene
     *            the scene to set
     */
    public void setScene(String scene) {
        this.scene = scene;
    }

    /**
     * @return the windDir
     */
    public float getWindDir() {
        return windDir;
    }

    /**
     * @param windDir
     *            the windDir to set
     */
    public void setWindDir(float windDir) {
        this.windDir = windDir;
    }

    /**
     * @return the windSpd
     */
    public float getWindSpd() {
        return windSpd;
    }

    /**
     * @param windSpd
     *            the windSpd to set
     */
    public void setWindSpd(float windSpd) {
        this.windSpd = windSpd;
    }

    /**
     * @return the pointDataView
     */
    public PointDataView getPointDataView() {
        return pointDataView;
    }

    /**
     * @param pointDataView
     *            the pointDataView to set
     */
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    /**
     * @return the filter
     */
    public Float getFilter() {
        return filter;
    }

    /**
     * @param filter
     *             the filter to set
     */
    public void setFilter(Float filter) {
        this.filter = filter;
    }

    @Override
    public int hashCode() {

        final int prime = 31;
        int result = super.hashCode();

        result = prime * result + ((pointDataView == null) ? 0 : pointDataView.hashCode());
        result = prime * result + ((location == null) ? 0 : location.hashCode());
        result = prime * result + ((orbitalSlot == null) ? 0 : orbitalSlot.hashCode());
        result = prime * result + ((scene == null) ? 0 : scene.hashCode());
        result = prime * result + Integer.valueOf(channel).hashCode();
        result = prime * result + Float.valueOf(windSpd).hashCode();
        result = prime * result + Float.valueOf(windDir).hashCode();
        result = prime * result + ((filter == null) ? 0 : filter.hashCode());

        return result;

    }

     @Override
     public boolean equals(Object obj){
        
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;

        DMWRecord record = (DMWRecord) obj;

        if (pointDataView == null) {
            if (record.pointDataView != null)
                return false;
        } else if (!pointDataView.equals(record.pointDataView)) 
            return false;
        if (location == null) {
            if (record.location != null)
                return false;
        } else if (!location.equals(record.location)) 
            return false;
        if (orbitalSlot == null) {
            if (record.orbitalSlot != null)
                return false;
        } else if (!orbitalSlot.equals(record.orbitalSlot))
            return false;
        if (scene == null) {
            if (record.scene != null)
                return false;
        } else if (!scene.equals(record.scene))
            return false;
        if (channel != record.channel)
            return false;
        if (windSpd != record.windSpd)
            return false;
        if (windDir != record.windDir)
            return false;
        if (filter != record.filter)
            return false;

        return true;
     }
}
