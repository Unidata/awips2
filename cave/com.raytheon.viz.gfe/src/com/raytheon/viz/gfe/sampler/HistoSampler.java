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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.gfe.ifpclient.IFPClient;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Samples grid data and creates histogram
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 05, 2008 1167        mnash        Initial creation
 * Jul 11, 2008 1167        mnash        Fixed responses from IFPClient
 * Sep 03, 2008 1283        njensen      Fixed issues
 * Aug 19, 2009 2899        njensen      Maps for performance boost on getParmHisto()
 * Dec 09, 2015 5129        dgilling     Support new IFPClient.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class HistoSampler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HistoSampler.class);

    private List<ParmHisto> _histo = new ArrayList<ParmHisto>();

    private List<ParmHisto> _topoHisto = new ArrayList<ParmHisto>();

    private Map<String, Map<TimeRange, Map<ParmID, ParmHisto>>> histoMap = new HashMap<String, Map<TimeRange, Map<ParmID, ParmHisto>>>();

    private boolean _valid;

    /**
     * Default constructor
     */
    public HistoSampler() {
        _valid = false;
    }

    /**
     * Data constructor
     * 
     * @param histo
     * @param topoHisto
     * @param valid
     */
    public HistoSampler(ArrayList<ParmHisto> histo,
            ArrayList<ParmHisto> topoHisto, boolean valid) {
        _histo = histo;
        _topoHisto = topoHisto;
        _valid = valid;
    }

    /**
     * Constructor for HistoSampler taking a client on the network and a List of
     * SamplerRequests. This constructor limits the TimeRange of interest to
     * just those grids overlapping the given time ranges. This list of ids
     * identify the reference identifiers for which to do sampling. The list of
     * parms are those elements to have sampling performed.
     * 
     * @param client
     * @param requests
     */
    public HistoSampler(IFPClient client, ArrayList<SamplerRequest> requests) {
        _valid = true;
        statusHandler.handle(Priority.DEBUG, "Requests " + requests);
        // convert ids to reference data objects
        List<SamplerRequest> req = null;

        // if ((requests.size() > 0) && requests.get(0).isRefID()) {
        if (requests.size() > 0) {
            req = getReferenceData(client, requests);
        } else {
            req = new ArrayList<SamplerRequest>(0);
        }

        // Array requests by parm
        Map<ParmID, List<SamplerRequest>> requestsPerParm = new HashMap<ParmID, List<SamplerRequest>>();
        for (SamplerRequest sReq : req) {
            ParmID pId = sReq.parmID();
            List<SamplerRequest> tmp = requestsPerParm.get(pId);
            if (tmp == null) {
                tmp = new ArrayList<SamplerRequest>();
                requestsPerParm.put(pId, tmp);
            }
            tmp.add(sReq);
        }

        // process requests and determine the list of parms and time ranges
        Set<ParmID> parmIDs = requestsPerParm.keySet();

        statusHandler.handle(Priority.DEBUG, "RequestedParms " + parmIDs);

        // if ISC sampling, need the ISC edit areas
        List<ReferenceData> iscEditAreas = null;

        for (ParmID pId : parmIDs) {
            List<ReferenceData> myIscEditAreas = null;
            if (pId.getDbId().getModelName().equals("ISC")) {
                if (iscEditAreas == null) {
                    iscEditAreas = getISCReferenceData(client);
                }
                myIscEditAreas = iscEditAreas;
            }

            // filter requests by parm
            List<SamplerRequest> pRequests = requestsPerParm.get(pId);
            List<TimeRange> reqSamplerTR = new ArrayList<TimeRange>(
                    pRequests.size());
            // dups?
            for (SamplerRequest sReq : pRequests) {
                reqSamplerTR.add(sReq.timeRange());
            }

            // statusHandler.handle(Priority.DEBUG, "Requested Sampler TR "
            // + reqSamplerTR);

            // get the list of requested grids for the parm
            List<TimeRange> gridTimes = requestedGrids(client, pId,
                    reqSamplerTR);
            // statusHandler.handle(Priority.DEBUG, "Requested GridTimes "
            // + gridTimes);

            // get the grid data for sampling
            List<IGridSlice> gridData = getGrids(client, pId, gridTimes);
            // statusHandler.handle(Priority.DEBUG, "Data " + gridData.size()
            // + " grids.");

            // do the sampling
            // create the histogram for each area and time
            for (SamplerRequest sReq : pRequests) {
                final TimeRange tr = sReq.timeRange();
                List<IGridSlice> data = limitInventory(tr, gridData);
                ParmHisto ph = new ParmHisto(pId, data, sReq.area(), tr,
                        myIscEditAreas);
                addParmHisto(ph);
            }
        }

        IGridSlice topoGrid = getTopoGrid(client);
        TimeRange tr = TimeRange.allTimes();
        List<ReferenceData> refData = getUniqueRefDatas(req);
        List<IGridSlice> topoList = new ArrayList<IGridSlice>(1);
        topoList.add(topoGrid);
        for (int a = 0; a < refData.size(); a++) {
            _topoHisto.add(new ParmHisto(topoGrid.getGridInfo().getParmID(),
                    topoList, refData.get(a), tr, iscEditAreas));
        }
    }

    private void addParmHisto(ParmHisto ph) {
        Map<TimeRange, Map<ParmID, ParmHisto>> eaMap = histoMap.get(ph.area()
                .getId().getName());
        if (eaMap == null) {
            eaMap = new HashMap<TimeRange, Map<ParmID, ParmHisto>>();
        }
        Map<ParmID, ParmHisto> trMap = eaMap.get(ph.timeRange());
        if (trMap == null) {
            trMap = new HashMap<ParmID, ParmHisto>();
        }
        trMap.put(ph.parmID(), ph);
        eaMap.put(ph.timeRange(), trMap);
        histoMap.put(ph.area().getId().getName(), eaMap);
        _histo.add(ph);
    }

    /**
     * Returns the parm histo for the given parameter, time range, and reference
     * id. The given parameter, time range an reference id must exactly match
     * one of the item in the constructor. It also searches sequentially through
     * _histo for a match with parmid, reference id, and time range.
     * 
     * @param parm
     * @param area
     * @param timeRange
     * @return the parm histo
     */
    public final ParmHisto getParmHisto(final ParmID parm,
            final ReferenceID area, final TimeRange timeRange) {
        ParmHisto parmHisto = null;

        Map<TimeRange, Map<ParmID, ParmHisto>> eaMap = histoMap.get(area
                .getName());
        if (eaMap != null) {
            Map<ParmID, ParmHisto> trMap = eaMap.get(timeRange);
            if (trMap != null) {
                parmHisto = trMap.get(parm);
            }
        }

        if (parmHisto == null) {
            System.out.println("STOP    " + parm.toString() + "   "
                    + area.toString() + "   " + timeRange.toString());
        }

        return parmHisto;
    }

    /**
     * Returns the topography histo for the given reference id. The reference id
     * must exactly match one of the items that is given in the constructor.
     * Searches sequentially through _topoHist for a match with the reference
     * id.
     * 
     * @param area
     * @return the topo histo
     */
    public final ParmHisto getTopoHisto(final ReferenceID area) {
        final ParmHisto defaultHisto = null;

        for (int i = 0; i < _topoHisto.size(); i++) {
            if (_topoHisto.get(i).area().getId().getName()
                    .equals(area.getName())) {
                return _topoHisto.get(i);
            }
        }
        return defaultHisto;

    }

    /**
     * Gets the reference data from the server and replace the ReferenceIDs in
     * the request into ReferenceData objects. Returns the updated list of
     * requests. The IFPClient is the network connection to the server.
     * 
     * @param client
     * @param requests
     * @return
     */
    private List<SamplerRequest> getReferenceData(IFPClient client,
            final List<SamplerRequest> requests) {
        // determine list of edit areas to retrieve
        List<ReferenceID> need = new ArrayList<ReferenceID>(requests.size());
        for (SamplerRequest request : requests) {
            if (request.isRefID() && (!need.contains(request.areaID()))) {
                need.add(request.areaID());
            }
        }

        // retrieve them
        ServerResponse<List<ReferenceData>> sr = client.getReferenceData(need);
        if (!sr.isOkay()) {
            statusHandler
                    .error(String.format(
                            "Some edit areas not available in server: %s",
                            sr.message()));
            _valid = false;
            return Collections.emptyList();
        }
        List<ReferenceData> refAreas = sr.getPayload();
        for (ReferenceData area : refAreas) {
            area.getGrid();
        }

        // put in the reference data objects in place of the reference ids in
        // the request
        List<SamplerRequest> req = new ArrayList<SamplerRequest>(
                requests.size());
        for (int i = 0; i < requests.size(); i++) {
            if (requests.get(i).isRefID()) {
                for (int j = 0; j < need.size(); j++) {
                    if (need.get(j).getName()
                            .equals(requests.get(i).areaID().getName())) {
                        req.add(new SamplerRequest(requests.get(i).parmID(),
                                refAreas.get(j), requests.get(i).timeRange()));
                        break;
                    }
                }
            } else {
                req.add(requests.get(i));
            }
        }

        // sort the requests
        Collections.sort(req);
        return req;
    }

    /**
     * Given the list of SamplerRequests, returns the list of requested parmIDs
     * 
     * @param requests
     * @return
     */
    private Set<ParmID> requestedParms(List<SamplerRequest> requests) {
        Set<ParmID> ids = new HashSet<ParmID>((int) (requests.size() * 1.3) + 1);
        for (SamplerRequest req : requests) {
            ParmID pId = req.parmID();
            if (!ids.contains(pId)) {
                ids.add(pId);
            }
        }
        return ids;
    }

    /**
     * Given a list of requests and a parmID, returns just those requests that
     * apply to the given parmID
     * 
     * @param pid
     * @param requests
     * @return
     */
    private List<SamplerRequest> filterRequestByParm(ParmID pid,
            List<SamplerRequest> requests) {
        List<SamplerRequest> req = new ArrayList<SamplerRequest>();
        for (int i = 0; i < requests.size(); i++) {
            if (requests.get(i).parmID().equals(pid)) {
                req.add(requests.get(i));
            }
        }
        return req;
    }

    private List<TimeRange> requestedGrids(IFPClient client, ParmID parmID,
            List<TimeRange> sampleTR) {
        // get the inventory for this parm
        ServerResponse<List<TimeRange>> sr = client.getGridInventory(parmID);
        if (!sr.isOkay()) {
            _valid = false;
            statusHandler.error(String.format(
                    "Unable to get grid inventory for: %s %s", parmID,
                    sr.message()));
            return Collections.emptyList();
        }
        List<TimeRange> gridTimes = sr.getPayload();

        // create the needed grid list and return it
        List<TimeRange> ret = new ArrayList<TimeRange>(gridTimes.size());
        for (TimeRange gTime : gridTimes) {
            for (TimeRange sTime : sampleTR) {
                if (gTime.overlaps(sTime)) {
                    ret.add(gTime);
                    break;
                }
            }
        }

        return ret;
    }

    /**
     * Get a list of the unique reference datas from the request list. This
     * assumes that all of the requests are now in the form of ReferenceDatas
     * and there are no ReferenceIDs.
     * 
     * @param requests
     * @return
     */
    private List<ReferenceData> getUniqueRefDatas(List<SamplerRequest> requests) {
        List<ReferenceData> data = new ArrayList<ReferenceData>();
        Set<String> names = new HashSet<String>();
        for (SamplerRequest sReq : requests) {
            ReferenceData rData = sReq.area();
            String name = rData.getId().getName();
            if (!names.contains(name)) {
                data.add(rData);
                names.add(name);
            }
        }
        return data;
    }

    private List<IGridSlice> getGrids(IFPClient client, ParmID parmID,
            List<TimeRange> gridTimes) {
        ServerResponse<List<IGridSlice>> sr = SamplerGridSliceCache.getData(
                client, parmID, gridTimes);
        if (!sr.isOkay()) {
            _valid = false;
            statusHandler.error(String.format(
                    "Getting Grid Data problem for: %s %s", parmID,
                    sr.message()));
            return Collections.emptyList();
        }

        return sr.getPayload();
    }

    /**
     * Given a time range and a sequence of grids. returns a sequence back that
     * contains ONLY those grids that overlap the given time range
     * 
     * @param limitTR
     * @param grids
     * @return
     */
    private List<IGridSlice> limitInventory(final TimeRange limitTR,
            final List<IGridSlice> grids) {
        List<IGridSlice> retValue = new ArrayList<IGridSlice>();

        // limit inventory to limitTR
        for (IGridSlice grid : grids) {
            if (grid.getValidTime().overlaps(limitTR)) {
                retValue.add(grid);
            }
        }
        return retValue;
    }

    /**
     * get topography grid from server
     * 
     * @param client
     * @return
     */
    private IGridSlice getTopoGrid(IFPClient client) {
        // get the grid location
        ServerResponse<GridLocation> sr = client.getDBGridLocation();
        if (!sr.isOkay()) {
            _valid = false;
            statusHandler.error(String.format(
                    "Problem obtaining dbGridLocation: %s", sr.message()));
            return null;
        }
        GridLocation gloc = sr.getPayload();

        // get the topography grid
        ServerResponse<ScalarGridSlice> sr2 = client.getTopoData(gloc);
        if (!sr.isOkay()) {
            _valid = false;
            statusHandler.error(String.format(
                    "Problem obtaining topography grid: %s", sr2.message()));
            return null;
        }

        return sr2.getPayload();
    }

    /**
     * Retrieves the ISC reference data from the ReferenceSetManager
     * 
     * @param client
     * @return the ISC ref data
     */
    public List<ReferenceData> getISCReferenceData(IFPClient client) {
        ServerResponse<List<ReferenceID>> sr = client.getReferenceInventory();
        if (!sr.isOkay()) {
            _valid = false;
            statusHandler.error(String.format(
                    "Error getting ISC Edit Areas: %s", sr.message()));
        }
        List<ReferenceID> idsAll = sr.getPayload();

        // Get the ISC ids
        List<ReferenceID> idsISC = new ArrayList<ReferenceID>();
        for (ReferenceID id : idsAll) {
            String name = id.getName();
            if ((name.length() > 4) && (name.startsWith("ISC_"))) {
                idsISC.add(id);
            }
        }

        // statusHandler.handle(Priority.DEBUG,
        // "ISC edit areas: " + idsISC.toString());

        // get the ReferenceData objects
        ServerResponse<List<ReferenceData>> sr2 = client
                .getReferenceData(idsISC);
        if (!sr2.isOkay()) {
            statusHandler.error(String.format(
                    "Some ISC edit areas not available in server: %s",
                    sr.message()));
            _valid = false;
            return Collections.emptyList();
        }

        List<ReferenceData> refAreas = sr2.getPayload();
        for (ReferenceData refArea : refAreas) {
            refArea.getGrid();
        }
        return refAreas;
    }

    /**
     * 
     * 
     * @return the parm ID
     */
    public ParmID getParmID() {
        final List<ParmHisto> l = getParmHisto();
        if (l.size() == 0) {
            return new ParmID();
        }
        return l.get(0).parmID();
    }

    /**
     * Returns all parm histograms
     * 
     * @return the parm histograms
     */
    public final List<ParmHisto> getParmHisto() {
        return _histo;
    }

    /**
     * Returns all topo histograms
     * 
     * @return the topo histograms
     */
    public final List<ParmHisto> getTopoHisto() {
        return _topoHisto;
    }

    /**
     * Returns true if valid (all grids and edit areas retrieved successfully
     * 
     * @return true if valid
     */
    public boolean isValid() {
        return _valid;
    }

}
