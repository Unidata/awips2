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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.comm.PerspectiveSpecificLoadProperties;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.uf.viz.d2d.core.time.TimeMatchingConfiguration;

/**
 * Extension to load properties for D2D enabled Resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@XmlType(name = "d2dLoadProperties")
@XmlAccessorType(XmlAccessType.NONE)
public class D2DLoadProperties extends PerspectiveSpecificLoadProperties
        implements ISerializableObject {

    private transient TimeMatchingConfiguration timeConfig;

    public D2DLoadProperties() {
        this.timeConfig = new TimeMatchingConfiguration();
    }

    public D2DLoadProperties(TimeMatchingConfiguration timeConfig) {
        this.timeConfig = timeConfig;
    }

    /**
     * @return the timeConfig
     */
    public TimeMatchingConfiguration getTimeConfig() {
        return timeConfig;
    }

    /**
     * @param timeConfig
     *            the timeConfig to set
     */
    public void setTimeConfig(TimeMatchingConfiguration timeConfig) {
        this.timeConfig = timeConfig;
    }

    /**
     * @return the overlayMatchTolerance
     */
    @XmlAttribute(name = "overlayMatchTolerance")
    public Float getOverlayMatchTolerance() {
        return timeConfig.getTolerance();
    }

    /**
     * @param overlayMatchTolerance
     *            the overlayMatchTolerance to set
     */
    public void setOverlayMatchTolerance(Float overlayMatchTolerance) {
        timeConfig.setTolerance(overlayMatchTolerance);
    }

    /**
     * @return the overlayMatchDelta
     */
    @XmlAttribute(name = "overlayMatchDelta")
    public Long getOverlayMatchDelta() {
        return timeConfig.getDelta();
    }

    /**
     * @param overlayMatchDelta
     *            the overlayMatchDelta to set
     */
    public void setOverlayMatchDelta(Long overlayMatchDelta) {
        timeConfig.setDelta(overlayMatchDelta);
    }

    /**
     * @return the timeMatchBasis
     */
    @XmlAttribute(name = "timeMatchBasis")
    public boolean isTimeMatchBasis() {
        return timeConfig.isTimeMatchBasis();
    }

    /**
     * @param timeMatchBasis
     *            the timeMatchBasis to set
     */
    public void setTimeMatchBasis(boolean timeMatchBasis) {
        timeConfig.setTimeMatchBasis(timeMatchBasis);
    }

    /**
     * @return the loadMode
     */
    @XmlAttribute(name = "loadMode")
    public LoadMode getLoadMode() {
        return timeConfig.getLoadMode();
    }

    /**
     * @param loadMode
     *            the loadMode to set
     */
    public void setLoadMode(LoadMode loadMode) {
        timeConfig.setLoadMode(loadMode);
    }

    /**
     * @return the dataTimes
     */
    @XmlElement(name = "dataTime")
    public DataTime[] getDataTimes() {
        return timeConfig.getDataTimes();
    }

    /**
     * @param dataTimes
     *            the dataTimes to set
     */
    public void setDataTimes(DataTime[] dataTimes) {
        timeConfig.setDataTimes(dataTimes);
    }

    /**
     * @return the forecast
     */
    @XmlElement(name = "forecast")
    public Long getForecast() {
        return timeConfig.getForecast();
    }

    /**
     * @param forecast
     *            the forecast to set
     */
    public void setForecast(Long forecast) {
        timeConfig.setForecast(forecast);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime
                * result
                + ((timeConfig.getLoadMode() == null) ? 0 : timeConfig
                        .getLoadMode().hashCode());
        result = prime
                * result
                + ((timeConfig.getDataTimes() == null) ? 0 : timeConfig
                        .getDataTimes().hashCode());
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
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        D2DLoadProperties other = (D2DLoadProperties) obj;
        if (timeConfig.getLoadMode() == null) {
            if (other.timeConfig.getLoadMode() != null)
                return false;
        } else if (!timeConfig.getLoadMode().equals(
                other.timeConfig.getLoadMode()))
            return false;
        if (!Arrays.equals(other.timeConfig.getDataTimes(),
                timeConfig.getDataTimes())) {
            return false;
        }
        return true;
    }

}
