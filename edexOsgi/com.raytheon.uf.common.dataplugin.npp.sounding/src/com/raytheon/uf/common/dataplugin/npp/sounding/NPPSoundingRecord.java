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
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.MappedSuperclass;
import javax.persistence.SequenceGenerator;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 3, 2013            mschenke     Initial creation
 * Apr 12, 2013 1857       bgonzale    Changed to MappedSuperclass.
 * Mar 02, 2013 1970       bgonzale    Added SequenceGenerator and Inheritance Strategy
 *                                     annotations.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@MappedSuperclass
@SequenceGenerator(name = PluginDataObject.ID_GEN)
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public abstract class NPPSoundingRecord extends PersistablePluginDataObject
        implements IPointData {

    private static final long serialVersionUID = 1L;

    public static final String LATITUDE = "latitude";

    public static final String LONGITUDE = "longitude";

    @Embedded
    private PointDataView pointDataView;

    @DataURI(position = 1)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Double latitude;

    @DataURI(position = 2)
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Double longitude;

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.pointdata.IPointData#getPointDataView()
     */
    @Override
    public PointDataView getPointDataView() {
        return pointDataView;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointdata.IPointData#setPointDataView(com.raytheon
     * .uf.common.pointdata.PointDataView)
     */
    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

}
