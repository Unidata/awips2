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
package com.raytheon.viz.radar.rsc;

import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfo;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;

/**
 * Provides the metadata and constructor for Radar
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class RadarResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarResourceData.class);

    @XmlAttribute
    protected String pointID = "";

    // This might be better as an enumeration, currently "CZ-Pg" triggers
    // Composite Reflectivity to display as graphics rather than image and
    // "SRM" causes Velocity to do SRM stuff
    @XmlAttribute
    protected String mode = "";

    // Will only display the most recently ingested tilt for any time
    @XmlAttribute
    protected boolean latest = false;

    @XmlAttribute
    protected boolean rangeRings = true;

    public RadarResourceData() {
        nameGenerator = new RadarNameGenerator();
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        AbstractVizResource<?, ?> rrd = null;
        String format = null;
        int productCode = -1;
        if (objects.length > 0) {
            format = (objects.length > 0) ? ((RadarRecord) objects[0])
                    .getFormat() : "";
            productCode = ((RadarRecord) objects[0]).getProductCode();
        } else if (loadProperties.isLoadWithoutData()) {
            // I must be trying to load without data, Ill try.
            RequestConstraint productCodeConstraint = metadataMap
                    .get("productCode");
            if (productCodeConstraint != null
                    && productCodeConstraint.getConstraintType() == ConstraintType.EQUALS) {
                try {
                    productCode = Integer.parseInt(productCodeConstraint
                            .getConstraintValue());
                } catch (NumberFormatException e) {
                    // It was a good effort but it just wasn't meant to be.
                }
            }
            if (AbstractRadarResource.infoDict != null) {
                RadarInfo info = AbstractRadarResource.infoDict
                        .getInfo(productCode);
                if (info != null) {
                    format = info.getFormat();
                }
            }
        }
        if (format != null && productCode != -1) {
            IRadarInterrogator interrogator = RadarProductFactory
                    .buildInterrogator(productCode, format);

            if (!"".equals(format)) {
                rrd = RadarProductFactory.buildResource(this, loadProperties,
                        interrogator, productCode, format);
                for (PluginDataObject p : objects) {
                    ((AbstractRadarResource<?>) rrd).addRecord(p);
                }
            }
        } else if ("".equals(format)) {
            statusHandler.handle(Priority.ERROR,
                    "There is not format defined in radarInfo.txt");
        } else {
            throw new NoDataAvailableException();
        }
        return rrd;
    }

    /**
     * @return the pointID
     */
    public String getPointID() {
        return pointID;
    }

    /**
     * @param pointID
     *            the pointID to set
     */
    public void setPointID(String pointID) {
        this.pointID = pointID;
    }

    /**
     * @return the latest
     */
    public boolean isLatest() {
        return latest;
    }

    /**
     * @param latest
     *            the latest to set
     */
    public void setLatest(boolean latest) {
        this.latest = latest;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (latest ? 1231 : 1237);
        result = prime * result + ((mode == null) ? 0 : mode.hashCode());
        result = prime * result + ((pointID == null) ? 0 : pointID.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RadarResourceData other = (RadarResourceData) obj;
        if (latest != other.latest)
            return false;
        if (mode == null) {
            if (other.mode != null)
                return false;
        } else if (!mode.equals(other.mode))
            return false;
        if (pointID == null) {
            if (other.pointID != null)
                return false;
        } else if (!pointID.equals(other.pointID))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * getAvailableTimes()
     */
    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] all = super.getAvailableTimes();
        if (latest) {
            Set<DataTime> times = new HashSet<DataTime>();
            for (DataTime time : all) {
                time.setLevelValue(null);
                times.add(time);
            }
            all = times.toArray(new DataTime[0]);
        }
        return all;
    }

    @Override
    public void update(AlertMessage... messages) {
        for (AlertMessage message : messages) {
            // since radar dataTimes are expected to set the level value,
            // need to do that here.
            Object timeObj = message.decodedAlert.get("dataTime");
            if (timeObj instanceof DataTime) {
                DataTime time = (DataTime) timeObj;
                time.setLevelValue(((Number) message.decodedAlert
                        .get("primaryElevationAngle")).doubleValue());
            }
        }
        super.update(messages);
    }

}
