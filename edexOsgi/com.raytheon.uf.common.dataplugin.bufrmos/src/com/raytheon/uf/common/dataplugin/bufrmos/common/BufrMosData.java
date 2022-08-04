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
package com.raytheon.uf.common.dataplugin.bufrmos.common;

import javax.persistence.CascadeType;
import javax.persistence.Embedded;
import javax.persistence.ManyToOne;
import javax.persistence.MappedSuperclass;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Transient;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * MOSData "mirrors" the mosdata data base table.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2008 862        jkorman     Initial Coding.
 * Feb 06, 2009 1990       bphillip    removed populateDataStore method
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 02, 2013 1970       bgonzale    Removed Table annotation, changed from
 *                                     Entity  annotation to MappedSuperClass.
 * May 14, 2013 1869       bsteffen    Remove DataURI column from bufrmos.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Nov 04, 2013 2361       njensen     Remove XML annotations
 * Jul 22, 2015 4360       rferrel     Make location not nullable.
 * Feb 09, 2016 5283       nabowle     Remove NGM MOS support.
 * Apr 24, 2019 6140       tgurney     Remove Inheritance annotation
 *                                     (Hibernate 5.4 fix)
 *
 * </pre>
 *
 * @author jkorman
 */
@MappedSuperclass
@SequenceGenerator(name = PluginDataObject.ID_GEN)
@DynamicSerialize
public abstract class BufrMosData extends PersistablePluginDataObject
        implements IPersistable, IPointData {

    public static enum MOSType {
        ETA, GFS, AVN, LAMP, HPC, MRF
    };

    private static final long serialVersionUID = 1L;

    public static final String MOS_DATA = "Data";

    // Text of the WMO header
    @Transient
    @DynamicSerializeElement
    private String wmoHeader;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView = null;

    @ManyToOne(optional = false, cascade = { CascadeType.REFRESH })
    @PrimaryKeyJoinColumn
    @DataURI(position = 1, embedded = true)
    @DynamicSerializeElement
    private BufrMosDataLocation location;

    /**
     * Create an empty MOSData object.
     */
    public BufrMosData() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     *
     * @param uri
     *            A data uri applicable to this class.
     * @param tableDef
     *            The table definitions for this class.
     */
    public BufrMosData(String uri) {
        super(uri);
    }

    /**
     * Get the geometry latitude.
     *
     * @return The geometry latitude.
     */
    public double getLatitude() {
        return location.getLatitude();
    }

    /**
     * Get the geometry longitude.
     *
     * @return The geometry longitude.
     */
    public double getLongitude() {
        return location.getLongitude();
    }

    /**
     * Get the station identifier for this observation.
     *
     * @return the stationId
     */
    public String getStationId() {
        return location.getStationId();
    }

    /**
     * @return the type
     */
    public abstract MOSType getType();

    /**
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader
     *            the wmoHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     *
     * @param dataURI
     */
    @Override
    public void setDataURI(String dataURI) {
        identifier = dataURI;
    }

    public BufrMosDataLocation getLocation() {
        return location;
    }

    public void setLocation(BufrMosDataLocation mosLocation) {
        this.location = mosLocation;
    }

    @Override
    public PointDataView getPointDataView() {
        return this.pointDataView;
    }

    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    @Override
    public String getPluginName() {
        return "bufrmos" + getType();
    }
}
