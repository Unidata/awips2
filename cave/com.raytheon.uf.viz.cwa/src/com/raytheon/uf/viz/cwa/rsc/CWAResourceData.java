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
package com.raytheon.uf.viz.cwa.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.cwa.CWARecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * ResourceData for Center Weather Advisory data
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2010             jsanchez     Initial creation
 * Jun 16, 2015 4379       nabowle      Add the current time to {@link #getAvailableTimes()}.
 *
 * </pre>
 *
 * @author jsanchez
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "cwaResourceData")
public class CWAResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CWAResourceData.class);

    public CWAResourceData() {
        super();
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof CWAResourceData == false) {
            return false;
        }

        return true;
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        CWAResource rsc = new CWAResource(this, loadProperties);

        for (PluginDataObject o : objects) {
            if (o instanceof CWARecord) {
                CWARecord rec = (CWARecord) o;
                rsc.addRecord(rec);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Received wrong type of data.  Got: " + o.getClass()
                                + " Expected: " + CWARecord.class);
            }
        }
        return rsc;
    }

    /**
     * Return a set of available times for the given resource data. The current
     * time is appended to the list so CWAResource can display a live view of
     * active CWAs.
     */
    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        List<DataTime> times = new ArrayList<>();
        times.addAll(Arrays.asList(super.getAvailableTimes()));
        times.add(CWAResource.now());
        return times.toArray(new DataTime[0]);
    }
}
