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

package com.raytheon.uf.common.dataplugin.qc;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.NullString;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record class for QC data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 07, 2009 3408       bphillip    Initial creation
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * May 16, 2013 1869       bsteffen    Remove DataURI column from qc.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Feb 27, 2014 2852       rferrel     Add getter/setter to FakePointDataView.
 * Jul 21, 2015 4360       rferrel     Named unique constraint.
 * Jan 04, 2018 6861       njensen     Removed unnecessary fields, use PointDataView
 * 
 * 
 * </pre>
 * 
 * @author bphillip
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "qcseq")
@Table(name = "qc", uniqueConstraints = { @UniqueConstraint(name = "uk_qc_datauri_fields", columnNames = {
        "stationid", "reftime", "qcType", "latitude", "longitude" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "qc", indexes = { @Index(name = "qc_refTimeIndex", columnNames = {
        "refTime", "forecastTime" }) })
@DynamicSerialize
public class QCRecord extends PersistablePluginDataObject
        implements ISpatialEnabled, IPointData {

    private static final long serialVersionUID = -8836262244188895665L;

    @Embedded
    @DataURI(position = 2, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @Column(nullable = false, length = 20)
    @DataURI(position = 1)
    @NullString
    @DynamicSerializeElement
    private String qcType;

    @Column(length = 15)
    private String ncSet;

    private PointDataView pointDataView;

    public QCRecord() {

    }

    public QCRecord(String uri) {
        super(uri);
    }

    public String getStationId() {
        return location.getStationId();
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

    public float getLatitude() {
        return location.getLatitude().floatValue();
    }

    public float getLongitude() {
        return location.getLongitude().floatValue();
    }

    public float getElevation() {
        return location.getElevation();
    }

    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * @return the ncSet
     */
    public String getNcSet() {
        return ncSet;
    }

    /**
     * @param ncSet
     *            the ncSet to set
     */
    public void setNcSet(String ncSet) {
        this.ncSet = ncSet;
    }

    @Override
    public ISpatialObject getSpatialObject() {
        return location;
    }

    /**
     * @return the qcType
     */
    public String getQcType() {
        return qcType;
    }

    /**
     * @param qcType
     *            the qcType to set
     */
    public void setQcType(String qcType) {
        this.qcType = qcType;
    }

    @Override
    public PointDataView getPointDataView() {
        return pointDataView;
    }

    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    @Override
    public String getPluginName() {
        return "qc";
    }

}
