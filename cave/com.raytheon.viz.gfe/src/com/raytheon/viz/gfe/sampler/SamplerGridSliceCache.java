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
package com.raytheon.viz.gfe.sampler;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.gfe.ifpclient.IFPClient;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Caches the results of a call to IFPClient.getGridData(ParmID,
 * List<TimeRange>). This class exists entirely to boost the performance of the
 * FWF formatter, which inefficiently has a separate HistoSampler instance for
 * every zone, as opposed to a more efficient formatter like the ZFP which has
 * one HistoSampler and passes all the zones into that. This prevents it from
 * re-requesting the same data on the same thread over and over again for each
 * zone.
 * 
 * This class could be removed in favor of pulling the data through the
 * DataManager/ParmManager or altering the FWF formatter to not create a new
 * HistoSampler for every zone.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 23, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SamplerGridSliceCache {

    private static Map<Long, Map<ParmID, List<IGridSlice>>> cache = new HashMap<Long, Map<ParmID, List<IGridSlice>>>();

    public static ServerResponse<List<IGridSlice>> getData(IFPClient client,
            ParmID parmId, List<TimeRange> gridTimes) {
        List<IGridSlice> result = new ArrayList<IGridSlice>();
        List<TimeRange> needToRequest = new ArrayList<TimeRange>();
        long thread = Thread.currentThread().getId();
        Map<ParmID, List<IGridSlice>> parmMap = cache.get(thread);
        if (parmMap == null) {
            parmMap = new HashMap<ParmID, List<IGridSlice>>();
            cache.put(thread, parmMap);
        }
        List<IGridSlice> alreadyRetrieved = parmMap.get(parmId);
        if (alreadyRetrieved == null) {
            alreadyRetrieved = new ArrayList<IGridSlice>();
            parmMap.put(parmId, alreadyRetrieved);
        }

        for (TimeRange tr : gridTimes) {
            boolean found = false;
            for (IGridSlice slice : alreadyRetrieved) {
                if (tr.equals(slice.getValidTime())) {
                    result.add(slice);
                    found = true;
                    break;
                }
            }
            if (!found) {
                needToRequest.add(tr);
            }
        }

        ServerResponse<List<IGridSlice>> ssr = new ServerResponse<>();
        if (!needToRequest.isEmpty()) {
            ServerResponse<List<IGridSlice>> sr = client
                    .getGridData(new GetGridRequest(parmId, needToRequest));
            ssr.addMessages(sr);
            List<IGridSlice> retrieved = sr.getPayload();
            // for (IGridSlice slice : retrieved) {
            // slice.setUseCache(true);
            // }
            alreadyRetrieved.addAll(retrieved);
            result.addAll(retrieved);
        }

        Collections.sort(result);
        ssr.setPayload(result);
        return ssr;
    }

    public static void remove(long thread) {
        Map<ParmID, List<IGridSlice>> map = cache.remove(thread);
        if (map != null) {
            map.clear();
        }
    }

}
