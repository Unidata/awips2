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
package com.raytheon.uf.common.dataplugin.npp.crimss;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@Entity
@Table(name = "crimss", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@DynamicSerialize
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class CrimssRecord extends PersistablePluginDataObject implements
        IPointData {

    public static final String PDV_SURFACE_PRESSURE = "SurfacePressure";

    public static final String PDV_ALTITUDE = "AltitudeLevels_Pressure";

    public static final String PDV_P_ALTITUDE = "Pressure";

    public static final String PDV_TEMPERATURE = "Temperature";

    public static final String PDV_P_TEMPERATURE = "PressureLevels_Temperature";

    public static final String PDV_H2O = "H2O";

    public static final String PDV_P_H2O = "PressureLevels_H2O";

    public static final String LATITUDE = "latitude";

    public static final String LONGITUDE = "longitude";

    @Embedded
    private PointDataView pdv;

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

    public CrimssRecord() {
        super();
    }

    public CrimssRecord(String uri) {
        super(uri);
    }

    @Override
    public Date getPersistenceTime() {
        Calendar c = getInsertTime();
        if (c == null)
            return null;

        return c.getTime();
    }

    public void setPersistenceTime(Date persistTime) {
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTime(persistTime);
        setInsertTime(c);
    }

    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }

    public Double getLatitude() {
        return latitude;
    }

    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }

    public Double getLongitude() {
        return longitude;
    }

    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }

    @Override
    public PointDataView getPointDataView() {
        return pdv;
    }

    @Override
    public void setPointDataView(PointDataView pdv) {
        this.pdv = pdv;
    }

}
