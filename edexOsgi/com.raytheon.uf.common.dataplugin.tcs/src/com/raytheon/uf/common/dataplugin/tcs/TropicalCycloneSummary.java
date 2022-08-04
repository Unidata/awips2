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
package com.raytheon.uf.common.dataplugin.tcs;

import java.util.ArrayList;
import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.NullString;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import org.locationtech.jts.geom.Geometry;

/**
 * Record implementation for tcs plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2009            jsanchez    Initial creation
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Oct 22, 2013 2361       njensen     Remove XML annotations
 * Jul 23, 2014 3410       bclement    location changed to floats
 * Jul 28, 2015 4360       rferrel     Named unique constraint. Made productType non-nullable.
 * Jan 27, 2016 5285       tgurney     Remove dataURI column and update unique constraint.
 * Aug 04, 2016 5783       tgurney     Add forecasttime to unique constraint
 * 
 * </pre>
 * 
 * @author jsanchez
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "tcsseq")
@Table(name = "tcs", uniqueConstraints = { @UniqueConstraint(name = "uk_tcs_datauri_fields", columnNames = {
        "refTime", "forecastTime", "productType", "latitude", "longitude",
        "stationId" }) })
@DynamicSerialize
public class TropicalCycloneSummary extends PersistablePluginDataObject
        implements ISpatialEnabled, IPointData {

    private static final long serialVersionUID = 1L;

    @Embedded
    @DynamicSerializeElement
    private PointDataView pointDataView;

    // Text of the WMO header
    @Transient
    @DynamicSerializeElement
    private String wmoHeader = "";

    @DynamicSerializeElement
    @Column(nullable = false)
    @DataURI(position = 1)
    @NullString
    protected String productType = "";

    @Embedded
    @DataURI(position = 2, embedded = true)
    @DynamicSerializeElement
    private SurfaceObsLocation location;

    @DynamicSerializeElement
    @Transient
    protected String name;

    @DynamicSerializeElement
    @Transient
    private boolean tropical;

    @DynamicSerializeElement
    @Transient
    private String displayTime;

    @DynamicSerializeElement
    @Transient
    private int pressure;

    @DynamicSerializeElement
    @Transient
    protected int windSpeed;

    @DynamicSerializeElement
    @Transient
    protected ArrayList<Radius> radiusList;

    // @XmlElement
    // @DynamicSerializeElement
    // @Transient
    // protected List<WindRadius> windRadii;

    /**
     * Empty default constructor
     */
    public TropicalCycloneSummary() {
        tropical = true;
        windSpeed = 0;
        displayTime = "";
    }

    public TropicalCycloneSummary(String dataUri) {
        super(dataUri);
        tropical = true;
        windSpeed = 0;
        displayTime = "";
    }

    public TropicalCycloneSummary(String name, int pressure, float longitude,
            float latitude, String displayTime, int wind, boolean tropical) {
        this.name = name;
        this.pressure = pressure;
        location = new SurfaceObsLocation(name);
        location.setLatitude(latitude);
        location.setLongitude(longitude);
        this.displayTime = displayTime;
        this.windSpeed = wind;
        this.tropical = tropical;
    }

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

    @Override
    public SurfaceObsLocation getSpatialObject() {
        return location;
    }

    @Override
    public PointDataView getPointDataView() {
        return pointDataView;
    }

    @Override
    public void setPointDataView(PointDataView pointDataView) {
        this.pointDataView = pointDataView;
    }

    public String getProductType() {
        return productType;
    }

    public void setProductType(String productType) {
        this.productType = productType;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getPressure() {
        return pressure;
    }

    public void setPressure(int pressure) {
        this.pressure = pressure;
    }

    public SurfaceObsLocation getLocation() {
        return location;
    }

    public void setLocation(SurfaceObsLocation location) {
        this.location = location;
    }

    /**
     * Get this observation's geometry.
     * 
     * @return The geometry for this observation.
     */
    public Geometry getGeometry() {
        return location.getGeometry();
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

    public boolean isTropical() {
        return tropical;
    }

    public void setTropical(boolean tropical) {
        this.tropical = tropical;
    }

    public String getDisplayTime() {
        return displayTime;
    }

    public void setDisplayTime(String displayTime) {
        this.displayTime = displayTime;
    }

    public int getWindSpeed() {
        return windSpeed;
    }

    public void setWindSpeed(int windSpeed) {
        this.windSpeed = windSpeed;
    }

    public ArrayList<Radius> getRadiusList() {
        return radiusList;
    }

    public void setRadiusList(ArrayList<Radius> radiusList) {
        this.radiusList = radiusList;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        Calendar c = getDataTime().getRefTimeAsCalendar();
        if (c != null) {
            sb.append(String.format("TCS:%1$tY%1$tm%1$td%1$tH%1$tM",
                    getDataTime().getRefTimeAsCalendar()));
        } else {
            sb.append("TCS:YYYYMMDDHHmm");
        }
        sb.append(String.format("%6.2f %7.2f:", getLatitude(), getLongitude()));
        return sb.toString();
    }

    public String print() {
        String s = "";
        s += "Display Time = " + displayTime + "\n";
        s += location.getLatitude() + ", " + location.getLongitude() + "\n";
        s += "Wind Speed = " + windSpeed + "\n";
        if (radiusList != null) {
            for (Radius r : radiusList) {
                s += r + "\n";
            }
        }
        return s;
    }

    @Override
    public String getPluginName() {
        return "tcs";
    }
}
