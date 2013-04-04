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
package com.raytheon.viz.aviation.guidance;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.point.GFEPointDataContainers;
import com.raytheon.uf.common.dataplugin.gfe.request.GetPointDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Static utility methods for use with python code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2009            njensen     Initial creation
 * Mar 11, 2013 1735       rferrel     Get a list of GFE Point Data Containers
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GuidanceUtil {

    /**
     * Temporary workaround for metars, should be removed ASAP
     * 
     * @param siteID
     * @return
     */
    @Deprecated
    // TODO Remove this method no longer used in java, python of xml
    // configuration.
    public static MetarRecord getLatestMetar(String siteID) {
        try {
            Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
            map.put("pluginName", new RequestConstraint("obs"));
            map.put("location.stationId", new RequestConstraint(siteID));
            LayerProperty lp = new LayerProperty();
            lp.setEntryQueryParameters(map);
            DataTime[] dt = lp.getEntryTimes();
            if (dt.length == 0) {
                return null;
            }
            lp.setSelectedEntryTimes(new DataTime[] { dt[dt.length - 1] });
            List<Object> objs = Loader.loadData(lp, "select", 10000);
            MetarRecord metar = (MetarRecord) objs.get(0);
            return metar;
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Get a list of GFE Point Data information for the task request.
     * 
     * @param task
     * @return gfePointDataContainers
     * @throws VizException
     */
    @SuppressWarnings("unchecked")
    public static GFEPointDataContainers getGFEPointsData(
            GetPointDataRequest task) throws VizException {
        task.setWorkstationID(VizApp.getWsId());
        ServerResponse<GFEPointDataContainers> sr = (ServerResponse<GFEPointDataContainers>) ThriftClient
                .sendRequest(task);
        return sr.getPayload();
    }

}
