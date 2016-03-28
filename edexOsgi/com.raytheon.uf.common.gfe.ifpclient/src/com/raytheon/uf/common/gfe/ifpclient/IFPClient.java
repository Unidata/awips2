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
package com.raytheon.uf.common.gfe.ifpclient;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.request.ClearPracticeVTECTableRequest;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.request.AbstractGfeRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.CommitGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.CreateNewDbRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetActiveTableRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetClientsRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetDbInventoryRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetDiscreteDefinitionRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridHistoryRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridInventoryRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGridParmInfoRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetIscSendStatusRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetIscSendStatusRequest.IscSendStatus;
import com.raytheon.uf.common.dataplugin.gfe.request.GetKnownOfficeTypesRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetKnownSitesRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetLockTablesRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetOfficialDbNameRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetParmListRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetSingletonDbIdsRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetSiteTimeZoneInfoRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetTopoDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetWXDefinitionRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GridLocRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.IscMakeRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.IscRequestQueryRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.IscRequestQueryRequest.IscQueryResponse;
import com.raytheon.uf.common.dataplugin.gfe.request.LockChangeRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.SaveGfeGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.SendIscGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleData;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.CommitGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockTableRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SendISCRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WxDefinition;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Client class for the IFP database server.
 * <p>
 * For release 16.2.2, this code was re-ported from A1 so that all methods
 * return {@code ServerResponse} objects instead of arbitrarily returning a
 * {@code ServerResponse} or the inner payload.
 * <p>
 * {@code PyFPClient} provides an IFPClient implementation that more closely
 * matches the behavior of IFPClient prior to 16.2.2. Call
 * {@code getPythonClient} to retrieve a PyFPClient instance or construct one
 * manually.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2015  #5129     dgilling     Initial creation
 * Feb 05, 2016  #5242     dgilling     Replace calls to deprecated Localization APIs.
 * Feb 24, 2016  #5129     dgilling     Change how PyFPClient is constructed.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IFPClient {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String EDIT_AREAS_DIR = "gfe" + IPathManager.SEPARATOR
            + "editAreas";

    private static final String SAMPLE_SETS_DIR = "gfe"
            + IPathManager.SEPARATOR + "sampleSets";

    private final WsId myWsId;

    private final String siteId;

    private final GridLocation gridLocation;

    private final PyFPClient pythonClient;

    /**
     * Constructor.
     * 
     * @param wsId
     *            {@code WsId} instance to associate with this client.
     * @param siteId
     *            Site ID of this client.
     */
    public IFPClient(WsId wsId, String siteId) {
        this.myWsId = wsId;
        this.siteId = siteId;

        // get and set the WxDefinition
        ServerResponse<WxDefinition> sr = getWxDefinition();
        WxDefinition wxDef = sr.getPayload();
        if (!sr.isOkay()) {
            statusHandler.error(sr.message());
        }
        WeatherSubKey.setWxDefinition(siteId, wxDef);

        // get and set the DiscreteDefinition
        ServerResponse<DiscreteDefinition> sr2 = getDiscreteDefinition();
        DiscreteDefinition dDef = sr2.getPayload();
        if (!sr2.isOkay()) {
            statusHandler.error(sr2.message());
        }
        DiscreteKey.setDiscreteDefinition(siteId, dDef);

        ServerResponse<GridLocation> sr3 = getDBGridLocation();
        GridLocation gLoc = sr3.getPayload();
        if (!sr3.isOkay()) {
            statusHandler.error(sr3.message());
        }
        this.gridLocation = gLoc;

        this.pythonClient = new PyFPClient(this);
    }

    /**
     * Command to commit grids to the official database. This function accepts
     * only a single {@code CommitGridRequest}.
     * 
     * @param request
     *            The commit request.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> commitGrid(CommitGridRequest request) {
        return commitGrid(Arrays.asList(request));
    }

    /**
     * Command to commit grids to the official database. This function accepts
     * multiple {@code CommitGridRequest}s.
     * 
     * @param requests
     *            The commit requests.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> commitGrid(List<CommitGridRequest> requests) {
        CommitGridsRequest request = new CommitGridsRequest();
        request.setCommits(requests);

        return makeRequest(request);
    }

    /**
     * Command to send ISC grids.
     * 
     * @param requests
     *            The ISC send requests.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> sendISC(List<SendISCRequest> requests) {
        SendIscGridRequest request = new SendIscGridRequest();
        request.setRequests(requests);

        return makeRequest(request);
    }

    /**
     * Gets the database inventory.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the inventory.
     */
    public ServerResponse<List<DatabaseID>> getDbInventory() {
        GetDbInventoryRequest request = new GetDbInventoryRequest();

        return (ServerResponse<List<DatabaseID>>) makeRequest(request);
    }

    /**
     * Requests the creation of a new database.
     * 
     * @param id
     *            The {@code DatabaseID} of the database to create.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> createNewDb(DatabaseID id) {
        CreateNewDbRequest request = new CreateNewDbRequest(id);

        return makeRequest(request);
    }

    /**
     * Returns the parameter list for the given database.
     * 
     * @param id
     *            {@code DatabaseID} for the database to get parm list for.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the database's list of parms.
     */
    public ServerResponse<List<ParmID>> getParmList(DatabaseID id) {
        return getParmList(Arrays.asList(id));
    }

    /**
     * Returns the parameter list for the given databases.
     * 
     * @param ids
     *            Databases to retrieve parm lists for, identified by
     *            {@code DatabaseID}.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the inventory for all requested databases in one list.
     */
    public ServerResponse<List<ParmID>> getParmList(List<DatabaseID> ids) {
        GetParmListRequest request = new GetParmListRequest();
        request.setDbIds(ids);

        return (ServerResponse<List<ParmID>>) makeRequest(request);
    }

    /**
     * Returns the grid inventory for the given parameter.
     * 
     * @param parmID
     *            Parm to request inventory for, identified by {@code ParmID}.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the inventory as a list of grid times as
     *         {@code TimeRange}s.
     */
    public ServerResponse<List<TimeRange>> getGridInventory(ParmID parmID) {
        ServerResponse<Map<ParmID, List<TimeRange>>> sr = getGridInventory(Arrays
                .asList(parmID));
        List<TimeRange> trs = Collections.emptyList();
        Map<ParmID, List<TimeRange>> inv = sr.getPayload();
        if ((sr.isOkay()) && (inv.containsKey(parmID))) {
            trs = inv.get(parmID);
        }

        ServerResponse<List<TimeRange>> ssr = new ServerResponse<>();
        ssr.setPayload(trs);
        ssr.addMessages(sr);
        return ssr;
    }

    /**
     * Returns the grid inventory for the given parameters.
     * 
     * @param parmIDs
     *            Parms to request inventory for, identified by {@code ParmID}.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the inventory as a Map of {@code ParmID} to list of grid
     *         times as {@code TimeRange}s.
     */
    public ServerResponse<Map<ParmID, List<TimeRange>>> getGridInventory(
            List<ParmID> parmIDs) {
        GetGridInventoryRequest request = new GetGridInventoryRequest();
        request.setParmIds(parmIDs);

        return (ServerResponse<Map<ParmID, List<TimeRange>>>) makeRequest(request);
    }

    /**
     * Returns the grid parameter information for the specified parm.
     * 
     * @param parmID
     *            Parm to retrieve info for identified by {@code ParmID}.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains {@code GridParmInfo} for the requested parm.
     */
    public ServerResponse<GridParmInfo> getGridParmInfo(ParmID parmID) {
        ServerResponse<List<GridParmInfo>> sr = getGridParmInfo(Arrays
                .asList(parmID));
        List<GridParmInfo> gpi = sr.getPayload();
        GridParmInfo info = null;
        if ((sr.isOkay()) && (!gpi.isEmpty())) {
            info = gpi.get(0);
        }

        ServerResponse<GridParmInfo> ssr = new ServerResponse<>();
        ssr.setPayload(info);
        ssr.addMessages(sr);
        return ssr;
    }

    /**
     * Returns the grid parameter information for the specified list of parms.
     * 
     * @param parmIDs
     *            List of parms to retrieve info for identified by
     *            {@code ParmID}.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains {@code GridParmInfo} for each requested parm.
     */
    public ServerResponse<List<GridParmInfo>> getGridParmInfo(
            List<ParmID> parmIDs) {
        GetGridParmInfoRequest request = new GetGridParmInfoRequest();
        request.setParmIds(parmIDs);

        return (ServerResponse<List<GridParmInfo>>) makeRequest(request);
    }

    /**
     * Returns the grid history information for the specified parm and grid
     * time.
     * 
     * @param parmID
     *            {@code ParmID} of the parm to retrieve grid history for.
     * @param trs
     *            Grid time to retrieve history for.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains history for the grid.
     */
    public ServerResponse<List<GridDataHistory>> getGridHistory(ParmID parmID,
            TimeRange tr) {
        ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = getGridHistory(
                parmID, Arrays.asList(tr));
        Map<TimeRange, List<GridDataHistory>> his = sr.getPayload();
        List<GridDataHistory> info = Collections.emptyList();
        if ((sr.isOkay()) && (his.containsKey(tr))) {
            info = his.get(tr);
        }

        ServerResponse<List<GridDataHistory>> ssr = new ServerResponse<>();
        ssr.setPayload(info);
        ssr.addMessages(sr);
        return ssr;
    }

    /**
     * Returns the grid history information for the parm at the specified time
     * ranges.
     * 
     * @param parmID
     *            {@code ParmID} of the parm to retrieve grid history for.
     * @param trs
     *            Grid times to retrieve histories for.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains histories keyed by grid time.
     */
    public ServerResponse<Map<TimeRange, List<GridDataHistory>>> getGridHistory(
            ParmID parmID, List<TimeRange> trs) {
        GetGridHistoryRequest request = new GetGridHistoryRequest();
        request.setParmID(parmID);
        request.setTimeRanges(trs);

        return (ServerResponse<Map<TimeRange, List<GridDataHistory>>>) makeRequest(request);
    }

    /**
     * Saves grid data identified by the save grid request. This function is for
     * a single grid request.
     * 
     * @param saveGridRequest
     *            The save request.
     * @param iscSendStatus
     *            Whether or not ISC send is enabled for this client session.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> saveGridData(SaveGridRequest saveGridRequest,
            boolean iscSendStatus) {
        return saveGridData(Arrays.asList(saveGridRequest), iscSendStatus);
    }

    /**
     * Saves grid data identified by the sequence of save grid requests.
     * 
     * @param saveGridRequest
     *            The save requests.
     * @param iscSendStatus
     *            Whether or not ISC send is enabled for this client session.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> saveGridData(
            List<SaveGridRequest> saveGridRequest, boolean iscSendStatus) {
        SaveGfeGridRequest request = new SaveGfeGridRequest(iscSendStatus,
                saveGridRequest);

        return makeRequest(request);
    }

    /**
     * Retrieves grid data identified by the get grid request.
     * 
     * @param request
     *            The get grid request.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the data as {@code IGridSlice}s.
     */
    public ServerResponse<List<IGridSlice>> getGridData(GetGridRequest request) {
        return getGridData(Arrays.asList(request));
    }

    /**
     * Retrieves grid data identified by the sequence of get grid requests.
     * 
     * @param getRequest
     *            The get grid requests.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the data as {@code IGridSlice}s.
     */
    public ServerResponse<List<IGridSlice>> getGridData(
            List<GetGridRequest> getRequest) {
        GetGridDataRequest request = new GetGridDataRequest();
        request.setRequests(getRequest);

        return (ServerResponse<List<IGridSlice>>) makeRequest(request);
    }

    /**
     * Retrieves the lock table based on the lock table request. Note that more
     * than one lock table may be returned.
     * 
     * @param request
     *            the lock table request
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested lock table (more than one lock table may
     *         be returned).
     */
    public ServerResponse<List<LockTable>> getLockTable(LockTableRequest request) {
        return getLockTable(Arrays.asList(request));
    }

    /**
     * Retrieves the lock table based on the lock table requests.
     * 
     * @param requests
     *            the lock table requests
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested lock tables.
     */
    public ServerResponse<List<LockTable>> getLockTable(
            List<LockTableRequest> requests) {
        GetLockTablesRequest request = new GetLockTablesRequest();
        request.setRequests(requests);

        return (ServerResponse<List<LockTable>>) makeRequest(request);
    }

    /**
     * Requests a lock change.
     * 
     * @param lockRequest
     *            The lock request.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the affected lock tables.
     */
    public ServerResponse<List<LockTable>> requestLockChange(
            LockRequest lockRequest) {
        return requestLockChange(Arrays.asList(lockRequest));
    }

    /**
     * Requests lock changes.
     * 
     * @param lockRequests
     *            The lock requests.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the affected lock tables.
     */
    public ServerResponse<List<LockTable>> requestLockChange(
            List<LockRequest> lockRequests) {
        LockChangeRequest request = new LockChangeRequest();
        request.setRequests(lockRequests);

        return (ServerResponse<List<LockTable>>) makeRequest(request);
    }

    /**
     * Returns the database id for the official database.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested database id.
     */
    public ServerResponse<List<DatabaseID>> getOfficialDBName() {
        GetOfficialDbNameRequest request = new GetOfficialDbNameRequest();

        return (ServerResponse<List<DatabaseID>>) makeRequest(request);
    }

    /**
     * Returns the database ids that correspond to the singleton-type of
     * database, i.e., the ones without model time stamps.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested database ids.
     */
    public ServerResponse<List<DatabaseID>> getSingletonDBName() {
        GetSingletonDbIdsRequest request = new GetSingletonDbIdsRequest();

        return (ServerResponse<List<DatabaseID>>) makeRequest(request);
    }

    /**
     * Returns the topography data set for the given {@code GridLocation}.
     * 
     * @param loc
     *            {@code GridLocation} to request topography data for.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested data as a {@code ScalarGridSlice}.
     */
    public ServerResponse<ScalarGridSlice> getTopoData(GridLocation loc) {
        GetTopoDataRequest request = new GetTopoDataRequest(loc);

        return (ServerResponse<ScalarGridSlice>) makeRequest(request);
    }

    /**
     * Gets the sample set inventory.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the inventory in {@code SampleId} form.
     */
    public ServerResponse<List<SampleId>> getSampleInventory() {
        ServerResponse<List<SampleId>> sr = new ServerResponse<>();
        List<SampleId> inv = new ArrayList<>();

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationFile[] files = pathMgr.listStaticFiles(
                LocalizationType.COMMON_STATIC, SAMPLE_SETS_DIR,
                new String[] { ".xml" }, false, true);
        for (LocalizationFile lf : files) {
            String name = LocalizationUtil.extractName(lf.getPath()).replace(
                    ".xml", "");
            inv.add(new SampleId(name, false, lf.getContext()
                    .getLocalizationLevel()));
        }

        sr.setPayload(inv);
        return sr;
    }

    /**
     * Gets the reference set inventory. The inventory is returned through
     * {@code ReferenceID}.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the inventory.
     */
    public ServerResponse<List<ReferenceID>> getReferenceInventory() {
        ServerResponse<List<ReferenceID>> sr = new ServerResponse<>();

        List<ReferenceID> refIDs = new ArrayList<>();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile[] contents = pm.listStaticFiles(
                LocalizationType.COMMON_STATIC, EDIT_AREAS_DIR,
                new String[] { ".xml" }, false, true);
        if (contents != null) {
            for (LocalizationFile lf : contents) {
                String area = LocalizationUtil.extractName(lf.getPath())
                        .replace(".xml", "");
                refIDs.add(new ReferenceID(area, false, lf.getContext()
                        .getLocalizationLevel()));
            }
        }

        sr.setPayload(refIDs);
        return sr;
    }

    /**
     * Saves sample set data identified by the sequence of {@code SampleData}.
     * 
     * @param sampleData
     *            Sample set data to save. Files will be written to USER
     *            localization level.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> saveSampleData(List<SampleData> sampleData) {
        ServerResponse<Object> sr = new ServerResponse<>();

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);
        for (SampleData sample : sampleData) {
            ILocalizationFile lf = pm.getLocalizationFile(ctx, SAMPLE_SETS_DIR
                    + IPathManager.SEPARATOR + sample.getSampleId().getName()
                    + ".xml");
            try (SaveableOutputStream out = lf.openOutputStream()) {
                SampleData.getJAXBManager().marshalToStream(sample, out);
                out.save();
            } catch (Exception e) {
                String message = String
                        .format("Unable to write output file [%s] for sample data [%s]: %s",
                                lf.toString(), sample.getSampleId().getName(),
                                e.getLocalizedMessage());
                sr.addMessage(message);
                break;
            }
        }

        return sr;
    }

    /**
     * Saves reference data identified by the sequence of {@code ReferenceData}
     * s.
     * 
     * @param referenceData
     *            Reference data to save. Files will be written to USER
     *            localization level.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> saveReferenceData(List<ReferenceData> referenceData) {
        ServerResponse<?> sr = new ServerResponse<>();

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);
        for (ReferenceData refData : referenceData) {
            ILocalizationFile lf = pm.getLocalizationFile(ctx, EDIT_AREAS_DIR
                    + IPathManager.SEPARATOR + refData.getId().getName()
                    + ".xml");
            try (SaveableOutputStream out = lf.openOutputStream()) {
                ReferenceData.getJAXBManager().marshalToStream(refData, out);
                out.save();
            } catch (Exception e) {
                String message = String
                        .format("Unable to write output file [%s] for reference data [%s]: %s",
                                lf.toString(), refData.getId().getName(),
                                e.getLocalizedMessage());
                sr.addMessage(message);
                break;
            }
        }

        return sr;
    }

    /**
     * Deletes sample data identified by the sequence of {@code SampleID}.
     * 
     * @param ids
     *            The sample sets to delete, identified by {@code SampleID}.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> deleteSampleData(List<SampleId> ids) {
        ServerResponse<?> sr = new ServerResponse<>();

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);
        for (SampleId id : ids) {
            try {
                ILocalizationFile lf = pm.getLocalizationFile(ctx,
                        SAMPLE_SETS_DIR + IPathManager.SEPARATOR + id.getName()
                                + ".xml");
                lf.delete();
            } catch (LocalizationException e) {
                String message = String.format("Deletion for [%s] failed: %s",
                        id.getName(), e.getLocalizedMessage());
                sr.addMessage(message);
            }
        }
        return sr;
    }

    /**
     * Deletes reference data identified by the sequence of {@code ReferenceID}.
     * 
     * @param ids
     *            The sample sets to delete, identified by {@code ReferenceID}.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> deleteReferenceData(List<ReferenceID> ids) {
        ServerResponse<?> sr = new ServerResponse<>();

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);
        for (ReferenceID id : ids) {
            try {
                ILocalizationFile lf = pm.getLocalizationFile(ctx,
                        EDIT_AREAS_DIR + IPathManager.SEPARATOR + id.getName()
                                + ".xml");
                lf.delete();
            } catch (LocalizationException e) {
                String message = String.format("Deletion for [%s] failed: %s",
                        id.getName(), e.getLocalizedMessage());
                sr.addMessage(message);
            }
        }
        return sr;
    }

    /**
     * Retrieves sample data identified by a sample id.
     * 
     * @param id
     *            The {@code SampleId} for the sample set to retrieve.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested {@code SampleData} object.
     */
    public ServerResponse<SampleData> getSampleData(SampleId id) {
        SampleData data = null;

        ServerResponse<List<SampleData>> sr = getSampleData(Arrays.asList(id));
        List<SampleData> seq = sr.getPayload();
        if ((sr.isOkay()) && (!seq.isEmpty())) {
            data = seq.get(0);
        }

        ServerResponse<SampleData> ssr = new ServerResponse<>();
        ssr.addMessages(sr);
        ssr.setPayload(data);
        return ssr;
    }

    /**
     * Retrieves sample data identified by the sequence of sample ids.
     * 
     * @param ids
     *            {@code SampleId}s to retrieve sample set data for.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested {@code SampleData} objects.
     */
    public ServerResponse<List<SampleData>> getSampleData(List<SampleId> ids) {
        ServerResponse<List<SampleData>> sr = new ServerResponse<>();
        List<SampleData> data = new ArrayList<>();

        for (SampleId id : ids) {
            String filePath = SAMPLE_SETS_DIR + IPathManager.SEPARATOR
                    + id.getName() + ".xml";
            ILocalizationFile lf = PathManagerFactory.getPathManager()
                    .getStaticLocalizationFile(LocalizationType.COMMON_STATIC,
                            filePath);

            if (lf != null) {
                try (InputStream in = lf.openInputStream()) {
                    SampleData sampleData = SampleData.getJAXBManager()
                            .unmarshalFromInputStream(in);
                    sampleData.setSampleId(id);
                    data.add(sampleData);
                } catch (Exception e) {
                    String message = String.format(
                            "Unable to read sample data [%s] from file [%s].",
                            id, lf.toString());
                    sr.addMessage(message);
                    data = Collections.emptyList();
                    break;
                }
            } else {
                String message = String.format(
                        "Unable to find sample data [%s].", id);
                sr.addMessage(message);
                data = Collections.emptyList();
                break;
            }
        }

        sr.setPayload(data);
        return sr;
    }

    /**
     * Retrieves reference data identified by a reference id.
     * 
     * @param id
     *            The {@code ReferenceID} for the reference set to retrieve.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested reference data.
     */
    public ServerResponse<ReferenceData> getReferenceData(ReferenceID id) {
        ReferenceData data = null;

        ServerResponse<List<ReferenceData>> sr = getReferenceData(Arrays
                .asList(id));
        List<ReferenceData> seq = sr.getPayload();
        if ((sr.isOkay()) && (seq.size() == 1)) {
            data = seq.get(0);
        }

        ServerResponse<ReferenceData> ssr = new ServerResponse<>();
        ssr.addMessages(sr);
        ssr.setPayload(data);
        return ssr;
    }

    /**
     * Retrieves reference data identified by the sequence of reference ids.
     * 
     * @param id
     *            {@code ReferenceID}s to retrieve reference data for.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested {@code ReferenceData} objects.
     */
    public ServerResponse<List<ReferenceData>> getReferenceData(
            List<ReferenceID> ids) {
        ServerResponse<List<ReferenceData>> sr = new ServerResponse<>();
        List<ReferenceData> data = new ArrayList<>();

        for (ReferenceID id : ids) {
            String filePath = EDIT_AREAS_DIR + IPathManager.SEPARATOR
                    + id.getName() + ".xml";
            ILocalizationFile lf = PathManagerFactory.getPathManager()
                    .getStaticLocalizationFile(LocalizationType.COMMON_STATIC,
                            filePath);
            if (lf != null) {
                try (InputStream in = lf.openInputStream()) {
                    ReferenceData refData = ReferenceData.getJAXBManager()
                            .unmarshalFromInputStream(in);
                    refData.setId(new ReferenceID(id.getName(), false, lf
                            .getContext().getLocalizationLevel()));
                    refData.setGloc(gridLocation);
                    data.add(refData);
                } catch (Exception e) {
                    String message = String
                            .format("Unable to read reference data [%s] from file [%s].",
                                    id, lf.toString());
                    sr.addMessage(message);
                    data = Collections.emptyList();
                    break;
                }
            } else {
                String message = String.format(
                        "Unable to find reference data [%s].", id);
                sr.addMessage(message);
                data = Collections.emptyList();
                break;
            }
        }

        sr.setPayload(data);
        return sr;
    }

    /**
     * Gets the WxDefinition.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the {@code WxDefinition}.
     */
    public ServerResponse<WxDefinition> getWxDefinition() {
        GetWXDefinitionRequest request = new GetWXDefinitionRequest();

        return (ServerResponse<WxDefinition>) makeRequest(request);
    }

    /**
     * Gets the DiscreteDefinition.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the {@code DiscreteDefinition}.
     */
    public ServerResponse<DiscreteDefinition> getDiscreteDefinition() {
        GetDiscreteDefinitionRequest request = new GetDiscreteDefinitionRequest();

        return (ServerResponse<DiscreteDefinition>) makeRequest(request);
    }

    /**
     * Gets the site identifier associated with the server.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the site identifier.
     */
    public ServerResponse<String> getSiteID() {
        ServerResponse<String> sr = new ServerResponse<>();
        sr.setPayload(siteId);
        return sr;
    }

    /**
     * Retrieves site time zone associated with the given site.
     * 
     * @param site
     *            Site to get time zone for, identified by 3-character site
     *            identifier.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the site's time zone.
     */
    public ServerResponse<String> getSiteTimeZone(String site) {
        ServerResponse<Map<String, String>> sr = getSiteTimeZone(Arrays
                .asList(site));
        Map<String, String> tzMap = sr.getPayload();

        String timeZone;
        if ((sr.isOkay()) && (tzMap.containsKey(site))) {
            timeZone = tzMap.get(site);
        } else {
            timeZone = "";
        }

        ServerResponse<String> ssr = new ServerResponse<>();
        ssr.addMessages(sr);
        ssr.setPayload(timeZone);
        return ssr;
    }

    /**
     * Retrieves site time zone associated with the list of sites.
     * 
     * @param ids
     *            Sites to get time zone for, identified by 3-character site
     *            identifier.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested sites and time zones.
     */
    public ServerResponse<Map<String, String>> getSiteTimeZone(List<String> ids) {
        GetSiteTimeZoneInfoRequest request = new GetSiteTimeZoneInfoRequest(ids);

        return (ServerResponse<Map<String, String>>) makeRequest(request);
    }

    /**
     * Retrieves the list of known sites that the server is configured for by
     * site ID.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the list of known site identifiers.
     */
    public ServerResponse<List<String>> getKnownSites() {
        GetKnownSitesRequest request = new GetKnownSitesRequest();

        return (ServerResponse<List<String>>) makeRequest(request);
    }

    /**
     * Retrieves the list of known sites that the server is configured for.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the a {@code Map} of site IDs to office types.
     */
    public ServerResponse<Map<String, String>> getKnownSitesWithOfficeType() {
        GetKnownOfficeTypesRequest request = new GetKnownOfficeTypesRequest();

        return (ServerResponse<Map<String, String>>) makeRequest(request);
    }

    /**
     * Retrieves the database grid location that is common to all weather
     * elements.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the site's {@code GridLocation}.
     */
    public ServerResponse<GridLocation> getDBGridLocation() {
        GridLocRequest request = new GridLocRequest();

        return (ServerResponse<GridLocation>) makeRequest(request);
    }

    /**
     * Returns the current list of clients that the server is connected to.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the list of connected clients.
     */
    public ServerResponse<List<String>> getClientList() {
        GetClientsRequest request = new GetClientsRequest();

        return (ServerResponse<List<String>>) makeRequest(request);
    }

    /**
     * Gets the VTEC Active Table based on the table name.
     * 
     * @param tableName
     *            The active table to retrieve.
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested active table records.
     */
    public ServerResponse<List<ActiveTableRecord>> getVTECActiveTable(
            ActiveTableMode tableName) {
        GetActiveTableRequest request = new GetActiveTableRequest();
        request.setRequestedSiteId(SiteMap.getInstance().getSite4LetterId(
                siteId));
        request.setMode(tableName);

        return (ServerResponse<List<ActiveTableRecord>>) makeRequest(request);
    }

    /**
     * Clears the specified VTEC table. Note that only the PRACTICE active table
     * can be successfully cleared.
     * 
     * @param tableName
     *            The active table to clear.
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> clearVTECTable(ActiveTableMode tableName) {
        ServerResponse<?> sr = new ServerResponse<>();

        if (ActiveTableMode.PRACTICE == tableName) {
            try {
                makeRequestInternal(new ClearPracticeVTECTableRequest(SiteMap
                        .getInstance().getSite4LetterId(siteId), myWsId));
            } catch (Exception e) {
                statusHandler.error(
                        "Server error processing clear VTEC table request.", e);
                sr.addMessage(String.format(
                        "Server error processing clear VTEC table request: %s",
                        e.getLocalizedMessage()));
            }
        } else {
            sr.addMessage("Can only clear PRACTICE active table.");
        }

        return sr;
    }

    /**
     * Queries the ISC routing table for the known ISC sites. Returns data about
     * the servers (mhsid, server, port, protocol, welist, and other
     * information). Returns a list of weather elements that normally are
     * requested.
     * 
     * @return Status of the request as a {@code ServerResponse}. Payload
     *         contains the requested ISC information.
     */
    public ServerResponse<IscQueryResponse> iscRequestQuery() {
        IscRequestQueryRequest request = new IscRequestQueryRequest();

        return (ServerResponse<IscQueryResponse>) makeRequest(request);
    }

    /**
     * Makes an ISC data request based on the information in the XML. The XML
     * contains the servers (mhsid, server, port, protocol, welist, and other
     * information) from which to request the information.
     * 
     * @param xmlRequest
     *            XML for the request
     * @return Status of the request as a {@code ServerResponse}.
     */
    public ServerResponse<?> iscRequestMake(String xmlRequest) {
        IscMakeRequest request = new IscMakeRequest();
        request.setXml(xmlRequest);

        return makeRequest(request);
    }

    /**
     * Returns the current ISC send status for the sendISCOnSave and the
     * sendISCOnPublish and request ISC states for the IFP Server.
     * 
     * @return The configuration values for requestISC, sendISCOnSave and
     *         sendISCOnPublish on the IFP server.
     */
    public ServerResponse<IscSendStatus> iscSendStatus() {
        GetIscSendStatusRequest request = new GetIscSendStatusRequest();

        return (ServerResponse<IscSendStatus>) makeRequest(request);
    }

    /**
     * To get a "legacy-style" (prior to A2 release 16.2.2) IFPClient
     * implementation. Instead of returning ServerResponse objects, the
     * legacy-style IFPClient will return the inner payload and throw Exceptions
     * if there are error messages in the ServerResponse returned from the
     * request server.
     * 
     * @return a {@code PyFPClient} instance.
     */
    public PyFPClient getPythonClient() {
        return pythonClient;
    }

    public ServerResponse<?> makeRequest(AbstractGfeRequest request) {
        request.setWorkstationID(myWsId);
        request.setSiteID(siteId);

        ServerResponse<Object> ssr = new ServerResponse<>();
        try {
            ServerResponse<?> sr = (ServerResponse<?>) makeRequestInternal(request);
            ssr.addMessages(sr);
            ssr.setPayload(sr.getPayload());
        } catch (Exception e) {
            statusHandler.error(String.format(
                    "Server error processing %s request.", request.getClass()
                            .getName()), e);
            ssr.addMessage(String.format(
                    "Server error processing %s request: %s", request
                            .getClass().getName(), e.getLocalizedMessage()));
        }

        return ssr;
    }

    private Object makeRequestInternal(IServerRequest request) throws Exception {
        return RequestRouter.route(request);
    }
}
