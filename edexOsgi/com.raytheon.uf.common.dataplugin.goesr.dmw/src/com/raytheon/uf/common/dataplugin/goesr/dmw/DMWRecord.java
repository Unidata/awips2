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
 * GOES-R Derived Motion Wind Record.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2015  4334       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "dmwseq")
@Table(name = DMWRecord.PLUGIN_NAME, uniqueConstraints = { @UniqueConstraint(columnNames = {
        "orbitalSlot", "scene", "channel", "refTime", "latitude", "longitude" }) })
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
    @Column(nullable = false, length = 9)
    private String orbitalSlot;

    @DataURI(position = 3)
    @DynamicSerializeElement
    @Column(nullable = false, length = 10)
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
}
