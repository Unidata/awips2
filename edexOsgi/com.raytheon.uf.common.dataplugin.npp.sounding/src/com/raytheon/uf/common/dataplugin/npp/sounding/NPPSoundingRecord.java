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
package com.raytheon.uf.common.dataplugin.npp.sounding;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.MappedSuperclass;
import javax.persistence.SequenceGenerator;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Abstract NPP sounding record class
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 03, 2013           mschenke    Initial creation
 * Apr 12, 2013  1857     bgonzale    Changed to MappedSuperclass.
 * Mar 02, 2013  1970     bgonzale    Added SequenceGenerator and Inheritance Strategy
 *                                    annotations.
 * Jun 11, 2014  2061     bsteffen    Remove IDecoderGettable
 * Jul 27, 2016  4360     rferrel     Made latitude and longitude non-nullable.
 * Jul 11, 2016  5754     bkowal      Made {@link #pointDataView} a {@link DynamicSerializeElement}.
 * Apr 24, 2019  6140     tgurney     Remove Inheritance annotation
 *                                    (Hibernate 5.4 fix)
 *
 * </pre>
 *
 * @author mschenke
 */
@MappedSuperclass
@SequenceGenerator(name = PluginDataObject.ID_GEN)
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public abstract class NPPSoundingRecord extends PersistablePluginDataObject
        implements IPointData {

    private static final long serialVersionUID = 1L;

    public static final String LATITUDE = "latitude";

    public static final String LONGITUDE = "longitude";

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    @DataURI(position = 1)
    @Column(nullable = false)
    @XmlAttribute
    @DynamicSerializeElement
    private Double latitude;

    @DataURI(position = 2)
    @Column(nullable = false)
    @XmlAttribute
    @DynamicSerializeElement
    private Double longitude;

    /**
     * @return the latitude
     */
    public Double getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public Double getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }

    @Override
    public PointDataView getPointDataView() {
        return pointDataView;
    }

    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

}
