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
package com.raytheon.uf.viz.d2d.core;

import java.util.Arrays;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.comm.PerspectiveSpecificLoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.uf.viz.d2d.core.time.TimeMatchingConfiguration;

/**
 * Extension to load properties for D2D enabled Resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 01, 2009           bgonzale    Initial creation
 * Oct 23, 2013  2491     bsteffen    Remove ISerializableObject
 * Mar 05, 2018  6900     bsteffen    Add a few convenience methods.
 * Jul 30, 2018  7259     bsteffen    Serialize clockDelta.
 * 
 * </pre>
 * 
 * @author bgonzale
 */
@XmlType(name = "d2dLoadProperties")
@XmlAccessorType(XmlAccessType.NONE)
public class D2DLoadProperties extends PerspectiveSpecificLoadProperties {

    private transient TimeMatchingConfiguration timeConfig;

    public D2DLoadProperties() {
        this.timeConfig = new TimeMatchingConfiguration();
    }

    public D2DLoadProperties(TimeMatchingConfiguration timeConfig) {
        this.timeConfig = timeConfig;
    }

    public TimeMatchingConfiguration getTimeConfig() {
        return timeConfig;
    }

    public void setTimeConfig(TimeMatchingConfiguration timeConfig) {
        this.timeConfig = timeConfig;
    }

    @XmlAttribute(name = "overlayMatchTolerance")
    public Float getOverlayMatchTolerance() {
        return timeConfig.getTolerance();
    }

    public void setOverlayMatchTolerance(Float overlayMatchTolerance) {
        timeConfig.setTolerance(overlayMatchTolerance);
    }

    @XmlAttribute(name = "overlayMatchDelta")
    public Long getOverlayMatchDelta() {
        return timeConfig.getDelta();
    }

    public void setOverlayMatchDelta(Long overlayMatchDelta) {
        timeConfig.setDelta(overlayMatchDelta);
    }

    @XmlAttribute(name = "timeMatchBasis")
    public boolean isTimeMatchBasis() {
        return timeConfig.isTimeMatchBasis();
    }

    public void setTimeMatchBasis(boolean timeMatchBasis) {
        timeConfig.setTimeMatchBasis(timeMatchBasis);
    }

    @XmlAttribute(name = "loadMode")
    public LoadMode getLoadMode() {
        return timeConfig.getLoadMode();
    }

    public void setLoadMode(LoadMode loadMode) {
        timeConfig.setLoadMode(loadMode);
    }

    @XmlElement(name = "dataTime")
    public DataTime[] getDataTimes() {
        return timeConfig.getDataTimes();
    }

    public void setDataTimes(DataTime[] dataTimes) {
        timeConfig.setDataTimes(dataTimes);
    }

    @XmlElement(name = "forecast")
    public Long getForecast() {
        return timeConfig.getForecast();
    }

    public void setForecast(Long forecast) {
        timeConfig.setForecast(forecast);
    }

    @XmlElement(name = "clockDelta")
    public Long getClockDelta() {
        return timeConfig.getClockDelta();
    }

    public void setClockDelta(Long clockDelta) {
        timeConfig.setClockDelta(clockDelta);
    }

    /**
     * Determine if the time match settings for this resource indicate it should
     * be completely hidden. Mechanisms such as the {@link ResourceProperties}
     * are not used for this because the resource may change visibility
     * depending on which frame is currently being viewed.
     * 
     * @param resource
     *            the resource to check
     * @return true if the resource should not be shown, false for normal
     *         displays.
     */
    public boolean isTimeMatchHidden(AbstractVizResource<?, ?> resource) {
        if (getLoadMode() != LoadMode.INVENTORY) {
            return false;
        }
        IDescriptor descriptor = resource.getDescriptor();
        if (descriptor == null) {
            return false;
        }
        FramesInfo framesInfo = descriptor.getFramesInfo();
        if (framesInfo == null) {
            return false;
        }
        return framesInfo.getTimeForResource(resource) == null;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((timeConfig.getLoadMode() == null) ? 0
                : timeConfig.getLoadMode().hashCode());
        result = prime * result + ((timeConfig.getDataTimes() == null) ? 0
                : timeConfig.getDataTimes().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        D2DLoadProperties other = (D2DLoadProperties) obj;
        if (timeConfig.getLoadMode() == null) {
            if (other.timeConfig.getLoadMode() != null) {
                return false;
            }
        } else if (!timeConfig.getLoadMode()
                .equals(other.timeConfig.getLoadMode())) {
            return false;
        }
        if (!Arrays.equals(other.timeConfig.getDataTimes(),
                timeConfig.getDataTimes())) {
            return false;
        }
        return true;
    }

    /**
     * Convenience method for getting/casting the D2DLoadProperties from a
     * resource if it has any.
     * 
     * @param resource
     *            The resource to get the properties from.
     * @return The D2DLoadProperties for the resource, or null if it does not
     *         have any D2DLoadProperties.
     */
    public static D2DLoadProperties get(AbstractVizResource<?, ?> resource) {
        if (resource == null) {
            return null;
        }
        LoadProperties loadProps = resource.getLoadProperties();
        if (loadProps == null) {
            return null;
        }
        PerspectiveSpecificLoadProperties perspProps = loadProps
                .getPerspectiveProperty();
        if (perspProps instanceof D2DLoadProperties) {
            return (D2DLoadProperties) perspProps;
        } else {
            return null;
        }
    }

}
