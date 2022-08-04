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
package com.raytheon.uf.viz.d2d.gfe.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * Resource Data for GFE grid data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 09, 2011           bsteffen  Initial creation
 * May 29, 2019  6613     bhurley   Load and display GFE data in D2D for the
 *                                  appropriate time period.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GFEGridResourceData extends AbstractRequestableResourceData {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEGridResourceData.class);

    @XmlAttribute
    private String legendString;

    @Override
    protected GFEGridResource constructResource(LoadProperties loadProperties,
            PluginDataObject[] objects) throws VizException {
        GFEGridResource resource = new GFEGridResource(this, loadProperties);
        for (PluginDataObject pdo : objects) {
            resource.addDataObject(pdo);
        }
        return resource;
    }

    public String getLegendString() {
        return legendString;
    }

    public void setLegendString(String legendString) {
        this.legendString = legendString;
    }

    @Override
    public PluginDataObject[] getLatestPluginDataObjects(DataTime[] desired,
            DataTime[] current) throws VizException {

        if (desired == null) {
            // Let the parent's implementation handle the null case
            return super.getLatestPluginDataObjects(desired, current);
        }

        // Create a list of modified desired times in order to successfully
        // request the plugin data objects. For each desired time, create a
        // modified desired time and set its reference time equal to the desired
        // time's reference time plus forecast time, set its forecast time to
        // zero, and remove the FCST_USED utility flag.
        List<DataTime> newDesired = new ArrayList<>(desired.length);
        for (DataTime desiredTime : desired) {
            if (desiredTime != null) {
                DataTime newDesiredTime = desiredTime.clone();
                Calendar refTime = newDesiredTime.getRefTimeAsCalendar();
                refTime.add(Calendar.SECOND, desiredTime.getFcstTime());
                newDesiredTime.setRefTime(refTime.getTime());
                newDesiredTime.setFcstTime(0);
                newDesiredTime.getUtilityFlags().remove(FLAG.FCST_USED);
                newDesired.add(newDesiredTime);
            }
        }

        PluginDataObject[] retVal = super.getLatestPluginDataObjects(
                newDesired.toArray(new DataTime[newDesired.size()]), current);

        // Update the data times in the returned PDOs to have proper reference
        // times and forecast times. For each PDO, search the desired list for a
        // time that has the same valid time as the PDO, then reset PDO's
        // reference time and forecast time to match those of the desired time.
        List<DataTime> desiredList = Arrays.asList(desired);
        for (PluginDataObject pdo : retVal) {
            Optional<DataTime> matchedTime = desiredList.stream().filter(
                    i -> i.getMatchValid() == pdo.getDataTime().getMatchValid())
                    .findFirst();
            if (!matchedTime.isPresent()) {
                statusHandler
                        .warn("Unable to find a desired time with a valid time that matches the PDO's valid time of "
                                + TimeUtil.formatDate(pdo.getDataTime()
                                        .getValidTimeAsDate()));
                continue;
            }
            DataTime time = matchedTime.get();
            int fcstTime = time.getFcstTime();
            Date refTime = time.getRefTime();
            pdo.getDataTime().setFcstTime(fcstTime);
            pdo.getDataTime().setRefTime(refTime);
        }

        return retVal;
    }
}
