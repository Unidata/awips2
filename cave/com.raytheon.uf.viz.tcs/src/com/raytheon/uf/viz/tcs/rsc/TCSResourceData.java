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
package com.raytheon.uf.viz.tcs.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.tcs.TropicalCycloneSummary;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 03, 2009            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "tcsResourceData")
public class TCSResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TCSResourceData.class);

    @XmlAttribute
    protected String plotSource = null;

    @XmlAttribute
    protected boolean isHourlyForecast = false;

    protected int maxHrs = 12;

    private static final int hourInMillis = 3600;

    public TCSResourceData() {
        // TODO Auto-generated constructor stub
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof TCSResourceData == false) {
            return false;
        }

        return true;
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        TCSResource rsc = null;
        rsc = new TCSResource(this, loadProperties);

        for (PluginDataObject o : objects) {
            if (o instanceof TropicalCycloneSummary) {
                TropicalCycloneSummary rec = (TropicalCycloneSummary) o;
                rsc.addRecord(rec);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Received wrong type of data.  Got: " + o.getClass()
                                + " Expected: " + TropicalCycloneSummary.class);
            }
        }
        return rsc;
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {

        DataTime[] available = getAvailableTimes(getMetadataMap(),
                getBinOffset());
        if (isHourlyForecast) {
            int counter = 0;
            DataTime[] hourlyForecasts = new DataTime[available.length * maxHrs];
            for (DataTime dt : available) {
                hourlyForecasts[counter++] = dt;
                for (int i = 1; i < maxHrs; i++) {
                    hourlyForecasts[counter++] = new DataTime(
                            dt.getRefTimeAsCalendar(), i * hourInMillis);
                }
            }
            available = hourlyForecasts;
        }
        return available;
    }

    /**
     * @return the plotSource
     */
    public String getPlotSource() {
        return plotSource;
    }

    /**
     * @param plotSource
     *            the plotSource to set
     */
    public void setPlotSource(String plotSource) {
        this.plotSource = plotSource;
    }

    public boolean isHourlyForecast() {
        return isHourlyForecast;
    }

    public void setHourlyForecast(boolean isHourlyForecast) {
        this.isHourlyForecast = isHourlyForecast;
    }
}
