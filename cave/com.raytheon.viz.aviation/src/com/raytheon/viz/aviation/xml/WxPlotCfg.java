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
package com.raytheon.viz.aviation.xml;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Contains information for the initializing the plot viewer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2009            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@XmlRootElement(name = "WxPlotConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class WxPlotCfg implements ISerializableObject {
    /**
     * Number of hours back from current time.
     */
    @XmlElement(name = "HoursBack")
    private int hoursBack;

    /**
     * Number of hours forward from current time.
     */
    @XmlElement(name = "HoursForward")
    private int hoursForeward;

    /**
     * Visibility bottom value.
     */
    @XmlElement(name = "VisBottom")
    private double visBottom;

    /**
     * Visibility top value.
     */
    @XmlElement(name = "VisTop")
    private double visTop;

    /**
     * Ceiling bottom value.
     */
    @XmlElement(name = "CeilingBottom")
    private int ceilingBottom;

    /**
     * Ceiling top value.
     */
    @XmlElement(name = "CeilingTop")
    private int ceilingTop;

    /**
     * Array of plot viewer information.
     */
    @XmlElements({ @XmlElement(name = "PlotViewer", type = PlotViewerCfg.class) })
    private List<PlotViewerCfg> plotViewers;

    /**
     * Constructor.
     */
    public WxPlotCfg() {
    }

    /**
     * Get the number of hours back from current time.
     * 
     * @return The number of hours.
     */
    public int getHoursBack() {
        return hoursBack;
    }

    /**
     * Set the number of hours back from current time.
     * 
     * @param hoursBack
     *            The number of hours.
     */
    public void setHoursBack(int hoursBack) {
        this.hoursBack = hoursBack;
    }

    /**
     * Get the number of hours forward from current time.
     * 
     * @return The number of hours.
     */
    public int getHoursForeward() {
        return hoursForeward;
    }

    /**
     * Set the number of hours forward from current time.
     * 
     * @param hoursForeward
     *            The number of hours.
     */
    public void setHoursForeward(int hoursForeward) {
        this.hoursForeward = hoursForeward;
    }

    /**
     * Get the visibility bottom value.
     * 
     * @return The visibility bottom value.
     */
    public double getVisBottom() {
        return visBottom;
    }

    /**
     * Set the visibility bottom value.
     * 
     * @param visBottom
     *            The visibility bottom value.
     */
    public void setVisBottom(double visBottom) {
        this.visBottom = visBottom;
    }

    /**
     * Get the visibility top value.
     * 
     * @return The visibility top value.
     */
    public double getVisTop() {
        return visTop;
    }

    /**
     * Set the visibility top value.
     * 
     * @param visTop
     *            The visibility top value.
     */
    public void setVisTop(double visTop) {
        this.visTop = visTop;
    }

    /**
     * Get the ceiling bottom value.
     * 
     * @return The ceiling bottom value.
     */
    public int getCeilingBottom() {
        return ceilingBottom;
    }

    /**
     * Set the ceiling bottom value.
     * 
     * @param ceilingBottom
     *            The ceiling bottom value.
     */
    public void setCeilingBottom(int ceilingBottom) {
        this.ceilingBottom = ceilingBottom;
    }

    /**
     * Get the ceiling top value.
     * 
     * @return The ceiling top value.
     */
    public int getCeilingTop() {
        return ceilingTop;
    }

    /**
     * Set the ceiling top value.
     * 
     * @param ceilingTop
     *            The ceiling top value.
     */
    public void setCeilingTop(int ceilingTop) {
        this.ceilingTop = ceilingTop;
    }

    /**
     * Get an array of plot viewer data.
     * 
     * @return Array of plot viewer data.
     */
    public List<PlotViewerCfg> getPlotViewers() {
        return plotViewers;
    }

    /**
     * Set an array of plot viewer data.
     * 
     * @param plotViewers
     *            Array of plot viewer data.
     */
    public void setPlotViewers(List<PlotViewerCfg> plotViewers) {
        this.plotViewers = plotViewers;
    }
}
