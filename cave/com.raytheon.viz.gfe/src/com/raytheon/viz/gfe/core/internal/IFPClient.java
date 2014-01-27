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

package com.raytheon.viz.gfe.core.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.request.AbstractGfeRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ClearPracticeVTECTableRequest;
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
import com.raytheon.uf.common.dataplugin.gfe.request.GetTopoDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GetWXDefinitionRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.GridLocRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.IscCreateDomainDictRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.IscGetRequestXmlRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.IscMakeRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.IscRequestQueryRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.LockChangeRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.SaveGfeGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.SendIscGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
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
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * This class provides the interface to the IFPServer.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 4/10/08      875        bphillip    Initial Creation
 * 4/17/08      1075       randerso    Created temporary static grid location data
 * 4/18/08      875        bphillip    Refined save functionality
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 01/29/09     #1271      njensen     Moved communication from ProductSrv to GfeSrv
 * 06/11/09     #1947      rjpeter     Fixed null pointers.
 * 07/09/09     #2590      njensen     Site ID from preferences and sent on all requests.
 * 09/22/09     #3058      rjpeter     Removed GFE Edex dependency.
 * 05/02/13     #1969      randerso    Added createNewDb method
 * 06/06/13     #2073      dgilling    Make getGridInventory() better match A1,
 *                                     fix warnings.
 * 11/20/2013   #2331      randerso    Added getTopoData method
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class IFPClient {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IFPClient.class);

    // we pull the site ID from CAVE configuration because the server
    // can run as multiple sites but the client can only be one
    private static final String siteID = LocalizationManager.getInstance()
            .getCurrentSite();

    /** The workstationID of the owner of this IFPClient instance */
    private final WsId workstationID;

    private DataManager dataManager;

    /**
     * Creates a new IFPClient
     * 
     * @param wsId
     *            The workstationID of the creator
     */
    public IFPClient(WsId wsId, DataManager dataManager) {
        this.dataManager = dataManager;
        workstationID = wsId;
        try {
            // get and set the WxDefinition
            WxDefinition wxDef = getWXDefinition();
            WeatherSubKey.setWxDefinition(siteID, wxDef);

            // get and set the DiscreteDefinition
            DiscreteDefinition dDef = getDiscreteDefinition();
            DiscreteKey.setDiscreteDefinition(siteID, dDef);

        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        }
    }

    /**
     * Gets a list of ParmIDs for the provided databaseID from the IFPServer
     * 
     * @param id
     *            The database ID for which to get the ParmIDs
     * @return The list of parms for the provided databaseID
     * @throws GFEServerException
     * 
     */
    public List<ParmID> getParmList(DatabaseID id) throws GFEServerException {
        return getParmList(Arrays.asList(new DatabaseID[] { id }));
    }

    /**
     * Gets a list of ParmIDs for the provided list of databaseIDs from the
     * IFPServer
     * 
     * @param ids
     *            The list of databaseIDS for which to get the parmIDs
     * @return The list of parms for the provided list of databaseIDs
     * @throws GFEServerException
     * 
     */
    @SuppressWarnings("unchecked")
    public List<ParmID> getParmList(List<DatabaseID> ids)
            throws GFEServerException {
        GetParmListRequest request = new GetParmListRequest();
        request.setDbIds(ids);
        ServerResponse<?> sr = makeRequest(request);
        return (List<ParmID>) sr.getPayload();
    }

    /**
     * Gets the grid parm info for the specified parmIDs from the IFPServer
     * 
     * @param parmIds
     *            The parm IDs to get parm info for
     * @return The parm information
     * @throws GFEServerException
     * 
     */
    @SuppressWarnings("unchecked")
    public List<GridParmInfo> getGridParmInfo(List<ParmID> parmIds)
            throws GFEServerException {
        GetGridParmInfoRequest request = new GetGridParmInfoRequest();
        request.setParmIds(parmIds);
        ServerResponse<?> sr = makeRequest(request);
        return (List<GridParmInfo>) sr.getPayload();
    }

    public GridParmInfo getGridParmInfo(ParmID parmId)
            throws GFEServerException {
        List<GridParmInfo> gpi = getGridParmInfo(Arrays
                .asList(new ParmID[] { parmId }));
        if ((gpi == null) || gpi.isEmpty()) {
            return null;
        } else {
            return gpi.get(0);
        }
    }

    /**
     * Gets the localized site ID for this GFE instance
     * 
     * @return The site ID for this GFE instance
     * @throws GFEServerException
     * 
     */
    public List<String> getSiteID() throws GFEServerException {
        List<String> siteIds = new ArrayList<String>();
        siteIds.add(siteID);
        return siteIds;
    }

    /**
     * Returns the time zone of the localized site
     * 
     * @return the time zone
     * @throws GFEServerException
     */
    public String getSiteTimeZone() throws GFEServerException {
        return getDBGridLocation().getTimeZone();
    }

    /**
     * Returns the grid inventory for a given parmId
     * 
     * @param parmId
     *            The parmId to get the inventory for
     * @return A map of TimeRange(as a string) to the inventory as a list of
     *         GFERecords
     * @throws GFEServerException
     */
    public List<TimeRange> getGridInventory(ParmID parmId)
            throws GFEServerException {
        Map<ParmID, List<TimeRange>> inventory = getGridInventory(Arrays
                .asList(new ParmID[] { parmId }));
        List<TimeRange> times = inventory.get(parmId);
        if (times == null) {
            times = Collections.emptyList();
        }
        return times;
    }

    /**
     * Gets the inventory of GFERecords from the IFPServer for the specified
     * list of parmIds
     * 
     * @param parmIds
     *            The parmIDs to get the inventory
     * @return The inventory of GFERecords
     * @throws GFEServerException
     * 
     */
    @SuppressWarnings("unchecked")
    public Map<ParmID, List<TimeRange>> getGridInventory(List<ParmID> parmIds)
            throws GFEServerException {
        GetGridInventoryRequest request = new GetGridInventoryRequest();
        request.setParmIds(parmIds);
        ServerResponse<Map<ParmID, List<TimeRange>>> response = (ServerResponse<Map<ParmID, List<TimeRange>>>) makeRequest(
                request, false);
        return response.getPayload();
    }

    /**
     * Requests a lock change identified by a single lock request.
     * 
     * @param lockRequest
     *            The lock request
     * @return The server response containing the changed lock tables
     * @throws GFEServerException
     */
    public ServerResponse<List<LockTable>> requestLockChange(
            LockRequest lockRequest) throws GFEServerException {
        ArrayList<LockRequest> lreq = new ArrayList<LockRequest>();
        lreq.add(lockRequest);
        return requestLockChange(lreq);
    }

    /**
     * Requests a lock change identified by a list of lock requests
     * 
     * @param lreq
     *            The lock requests
     * @return The server response containing the changed lock tables
     * @throws GFEServerException
     */
    @SuppressWarnings("unchecked")
    public ServerResponse<List<LockTable>> requestLockChange(
            List<LockRequest> lreq) throws GFEServerException {
        LockChangeRequest request = new LockChangeRequest();
        request.setRequests(lreq);
        ServerResponse<?> sr = makeRequest(request);
        return (ServerResponse<List<LockTable>>) sr;
    }

    /**
     * Gets the list of available databases from the IFPServer
     * 
     * @return The list of available databases
     * @throws GFEServerException
     * 
     */
    @SuppressWarnings("unchecked")
    public List<DatabaseID> getAvailableDbs() throws GFEServerException {
        GetDbInventoryRequest request = new GetDbInventoryRequest();
        ServerResponse<?> sr = makeRequest(request);
        return (List<DatabaseID>) sr.getPayload();
    }

    @SuppressWarnings("unchecked")
    public List<String> getClients() throws GFEServerException {
        GetClientsRequest request = new GetClientsRequest();
        ServerResponse<?> sr = makeRequest(request);
        return (List<String>) sr.getPayload();
    }

    /**
     * Retrieves the lock table from the server for this parm object.<br>
     * This is a temporary method and will be replaced by a lock notification
     * system to prevent large amounts of server requests when many parms are
     * being saved
     * 
     * @param parmId
     *            The parm to get the lock table for
     * @return The lock table for the specified parm
     * @throws GFEServerException
     * 
     */
    @SuppressWarnings("unchecked")
    public LockTable getLockTable(ParmID parmId) throws GFEServerException {
        List<LockTableRequest> ltReq = new ArrayList<LockTableRequest>();
        ltReq.add(new LockTableRequest(parmId));
        GetLockTablesRequest request = new GetLockTablesRequest();
        request.setRequests(ltReq);
        ServerResponse<?> sr = makeRequest(request);
        return ((List<LockTable>) sr.getPayload()).get(0);
    }

    /**
     * Gets a LockTable identified by a single database id
     * 
     * @param dbId
     *            The database id
     * @return The requested table
     * @throws GFEServerException
     */
    public List<LockTable> getLockTable(DatabaseID dbId)
            throws GFEServerException {
        List<LockTableRequest> request = Arrays
                .asList(new LockTableRequest[] { new LockTableRequest(dbId) });
        return getLockTable(request);
    }

    /**
     * Gets a list of LockTables identified by a list of lock table requests
     * 
     * @param ltrList
     *            The list of lock table requests
     * @return The list of LockTables
     * @throws GFEServerException
     */
    @SuppressWarnings("unchecked")
    public List<LockTable> getLockTable(List<LockTableRequest> ltrList)
            throws GFEServerException {
        GetLockTablesRequest request = new GetLockTablesRequest();
        request.setRequests(ltrList);
        ServerResponse<?> sr = makeRequest(request);
        return (List<LockTable>) sr.getPayload();
    }

    /**
     * Gets the grid location of the localized GFE instance
     * 
     * @return The grid location of the localized GFE instance
     * @throws GFEServerException
     */
    @SuppressWarnings("unchecked")
    public GridLocation getDBGridLocation() throws GFEServerException {
        GridLocRequest request = new GridLocRequest();
        ServerResponse<List<Object>> sr = (ServerResponse<List<Object>>) makeRequest(request);
        if (sr.isOkay()) {
            return (GridLocation) sr.getPayload().get(0);
        } else {
            return null;
        }
    }

    /**
     * Saves the parm information to the server
     * 
     * @param requests
     *            the save grid requests
     * @throws GFEServerException
     */
    public ServerResponse<?> saveGrids(List<SaveGridRequest> requests)
            throws GFEServerException {
        ServerResponse<?> response = null;
        if (!requests.isEmpty()) {
            SaveGfeGridRequest request = new SaveGfeGridRequest();
            request.setSaveRequest(requests);
            response = makeRequest(request);
        }
        logResponse(response);
        return response;
    }

    /**
     * Gets the list of known sites from the server
     * 
     * @return The list of known sites from the server
     * @throws GFEServerException
     */
    @SuppressWarnings("unchecked")
    public List<String> getKnownSites() throws GFEServerException {
        return (List<String>) makeRequest(new GetKnownSitesRequest())
                .getPayload();
    }

    /**
     * Gets the list of known office types from the server
     * 
     * @return The list of known office types from the server
     * @throws GFEServerException
     */
    @SuppressWarnings("unchecked")
    public List<String> getKnownOfficeTypes() throws GFEServerException {
        return (List<String>) makeRequest(new GetKnownOfficeTypesRequest())
                .getPayload();
    }

    public IscSendStatus getIscSendStatus() throws GFEServerException {
        GetIscSendStatusRequest request = new GetIscSendStatusRequest();
        request.setSiteID(siteID);
        return (IscSendStatus) makeRequest(request).getPayload();
    }

    /**
     * Logs a message to the message display
     * 
     * @param message
     *            The message to display
     */
    private void logMessage(String message) {
        Status statusMessage = new Status(IStatus.INFO, Activator.PLUGIN_ID,
                message + "\n");
        Activator.getDefault().getLog().log(statusMessage);
    }

    /**
     * Logs error messages contained in a ServerResponse object
     * 
     * @param sr
     *            The ServerResponse object containing the messages
     */
    private void logResponse(ServerResponse<?> sr) {
        for (ServerMsg msg : sr.getMessages()) {
            logMessage(msg.getMessage());
        }
    }

    /**
     * Gets the discrete definition
     * 
     * @return The discrete definition
     * @throws GFEServerException
     */
    @SuppressWarnings("unchecked")
    public DiscreteDefinition getDiscreteDefinition() throws GFEServerException {
        GetDiscreteDefinitionRequest request = new GetDiscreteDefinitionRequest();
        ServerResponse<DiscreteDefinition> sr = (ServerResponse<DiscreteDefinition>) makeRequest(request);
        if (!sr.isOkay()) {
            StringBuilder msg = new StringBuilder(
                    "Unable to retrieve DiscreteDefinition: ");
            for (ServerMsg s : sr.getMessages()) {
                msg.append("\n   ").append(s.getMessage());
            }

            statusHandler.handle(Priority.PROBLEM, msg.toString());
        }
        return sr.getPayload();
    }

    /**
     * Gets the Wx Definition from the server
     * 
     * @return The Wx Definition
     * @throws GFEServerException
     */
    @SuppressWarnings("unchecked")
    public WxDefinition getWXDefinition() throws GFEServerException {
        GetWXDefinitionRequest request = new GetWXDefinitionRequest();
        ServerResponse<WxDefinition> sr = (ServerResponse<WxDefinition>) makeRequest(request);
        if (!sr.isOkay()) {
            StringBuilder msg = new StringBuilder(
                    "Unable to retrieve WxDefinition: ");
            for (ServerMsg s : sr.getMessages()) {
                msg.append("\n   ").append(s.getMessage());
            }

            statusHandler.handle(Priority.PROBLEM, msg.toString());
        }
        return sr.getPayload();
    }

    /**
     * Makes a call to ReferenceSetManager to retrieve an ArrayList of
     * ReferenceData
     * 
     * @param need
     * @return an ArrayList of ReferenceData
     */
    public List<ReferenceData> getReferenceData(ArrayList<ReferenceID> need) {
        IReferenceSetManager ref = dataManager.getRefManager();

        return ref.getReferenceData(need);
    }

    /**
     * Makes a call to ReferenceSetManager to retrieve an ArrayList of
     * ReferenceID objects
     * 
     * @return an ArrayList of ReferenceID
     */
    public List<ReferenceID> getReferenceInventory() {
        IReferenceSetManager ref = dataManager.getRefManager();
        List<ReferenceID> refList = ref.getAvailableSets();

        return refList;
    }

    /**
     * Gets the grid data based on a parmID and a list of time ranges
     * 
     * @param parmId
     * @param gridTimes
     * @return List of grid slices
     * @throws GFEServerException
     */
    public List<IGridSlice> getGridData(ParmID parmId, List<TimeRange> gridTimes)
            throws GFEServerException {
        return getGridData(parmId, gridTimes, false);
    }

    @SuppressWarnings("unchecked")
    public List<IGridSlice> getGridData(ParmID parmId,
            List<TimeRange> gridTimes, boolean convertUnit)
            throws GFEServerException {
        GetGridRequest req = new GetGridRequest(parmId, gridTimes);
        req.setConvertUnit(convertUnit);
        GetGridDataRequest request = new GetGridDataRequest();
        request.addRequest(req);
        ServerResponse<?> resp = makeRequest(request);
        List<IGridSlice> slices = (List<IGridSlice>) resp.getPayload();
        return slices;
    }

    /**
     * Gets a parm, if there is no parm in the ParmManager it adds it based on
     * parmId
     * 
     * @param parmId
     * @param parmMan
     * @return a parm
     */
    public Parm getParm(ParmID parmId, IParmManager parmMan) {
        Parm parm = parmMan.getParm(parmId);
        if (parm == null) {
            parm = parmMan.addParm(parmId, true, false);
            parm = parmMan.getParm(parmId);
        }
        return parm;
    }

    /**
     * Commits a list of grids to the official database via a list of commit
     * grid requests
     * 
     * @param requests
     *            The commit grid requests
     * @return The ServerResponse containing any server side messages
     * @throws GFEServerException
     */
    public ServerResponse<?> commitGrid(List<CommitGridRequest> requests)
            throws GFEServerException {

        CommitGridsRequest commitRequest = new CommitGridsRequest();
        commitRequest.setCommits(requests);
        ServerResponse<?> sr = makeRequest(commitRequest);
        return sr;
    }

    /**
     * Commits a single grid to the official database
     * 
     * @param req
     *            The commitGridRequest
     * @throws GFEServerException
     */
    public ServerResponse<?> commitGrid(CommitGridRequest req)
            throws GFEServerException {
        List<CommitGridRequest> requests = new ArrayList<CommitGridRequest>();
        requests.add(req);
        return commitGrid(requests);
    }

    /**
     * Gets the official database name for this localized instance of GFE
     * 
     * @return The official database name
     * @throws GFEServerException
     */
    @SuppressWarnings("unchecked")
    public ServerResponse<List<DatabaseID>> getOfficialDBName()
            throws GFEServerException {
        GetOfficialDbNameRequest request = new GetOfficialDbNameRequest();
        return (ServerResponse<List<DatabaseID>>) makeRequest(request);
    }

    public List<ActiveTableRecord> getVTECActiveTable(String siteId)
            throws VizException {
        CAVEMode mode = dataManager.getOpMode();
        ActiveTableMode atMode = ActiveTableMode.OPERATIONAL;
        if (mode.equals(CAVEMode.PRACTICE)) {
            atMode = ActiveTableMode.PRACTICE;
        }
        return getVTECActiveTable(siteId, atMode);
    }

    @SuppressWarnings("unchecked")
    public List<ActiveTableRecord> getVTECActiveTable(String siteId,
            ActiveTableMode mode) throws VizException {
        GetActiveTableRequest request = new GetActiveTableRequest();
        request.setRequestedSiteId(SiteMap.getInstance().getSite4LetterId(
                siteId));
        request.setMode(mode);
        ServerResponse<?> sr = makeRequest(request);
        return (List<ActiveTableRecord>) sr.getPayload();
    }

    public ServerResponse<?> sendISC(List<SendISCRequest> req)
            throws GFEServerException {
        SendIscGridRequest request = new SendIscGridRequest();
        request.setRequests(req);
        return makeRequest(request);
    }

    @SuppressWarnings("unchecked")
    public ServerResponse<List<Object>> iscRequestQuery()
            throws GFEServerException {
        IscRequestQueryRequest request = new IscRequestQueryRequest();
        return (ServerResponse<List<Object>>) makeRequest(request);
    }

    @SuppressWarnings("unchecked")
    public ServerResponse<Object> iscGetDomainDict(String xml)
            throws GFEServerException {
        IscCreateDomainDictRequest request = new IscCreateDomainDictRequest();
        request.setXml(xml);
        return (ServerResponse<Object>) makeRequest(request);
    }

    public String iscGetRequestXML(String xml, List<String> selectedServers,
            List<String> selectedWE) throws GFEServerException {
        IscGetRequestXmlRequest request = new IscGetRequestXmlRequest();
        request.setXml(xml);
        request.setSelectedServers(selectedServers);
        request.setSelectedWEList(selectedWE);
        return (String) makeRequest(request).getPayload();
    }

    public void iscMakeISCRequest(String xmlRequest) throws GFEServerException {
        IscMakeRequest task = new IscMakeRequest();
        task.setXml(xmlRequest);
        makeRequest(task);
    }

    public ServerResponse<?> makeRequest(AbstractGfeRequest request)
            throws GFEServerException {
        return makeRequest(request, true);
    }

    private ServerResponse<?> makeRequest(AbstractGfeRequest request,
            boolean throwExceptionsBasedOnResponse) throws GFEServerException {
        ServerResponse<?> rval = null;

        try {
            // System.out.println("IFPClient.makeRequest: "
            // + request.getClass().getSimpleName());
            request.setWorkstationID(workstationID);
            request.setSiteID(siteID);
            Object obj = ThriftClient.sendRequest(request);

            if (obj instanceof ServerResponse) {
                rval = (ServerResponse<?>) obj;
            } else {
                throw new GFEServerException(
                        "Received invalid response object from GFE Server.  Received ["
                                + obj.getClass().getName() + "] excepted ["
                                + ServerResponse.class.getName());
            }
        } catch (VizException e) {
            throw new GFEServerException(e);
        }

        if ((throwExceptionsBasedOnResponse) && (rval != null)
                && (!rval.isOkay())) {
            StringBuilder msg = new StringBuilder();
            if (rval.getMessages().size() > 1) {
                msg.append("Errors ");
            } else {
                msg.append("Error ");
            }
            msg.append("occurred on GFE server -");
            Iterator<ServerMsg> iter = rval.getMessages().iterator();
            while (iter.hasNext()) {
                msg.append(iter.next().getMessage());
                if (iter.hasNext()) {
                    msg.append(", ");
                }
            }
            throw new GFEServerException(msg.toString());

        }

        return rval;
    }

    public void clearPracticeTable(String siteId) throws VizException {
        ClearPracticeVTECTableRequest request = new ClearPracticeVTECTableRequest();
        request.setRequestedSiteId(SiteMap.getInstance().getSite4LetterId(
                siteId));
        makeRequest(request);
    }

    public ServerResponse<?> getGridHistory(ParmID parmID,
            List<TimeRange> timeRanges) throws GFEServerException {
        GetGridHistoryRequest request = new GetGridHistoryRequest();
        request.setParmID(parmID);
        request.setTimeRanges(timeRanges);
        return makeRequest(request);
    }

    public ServerResponse<?> createNewDb(DatabaseID dbId)
            throws GFEServerException {
        CreateNewDbRequest request = new CreateNewDbRequest(dbId);
        return makeRequest(request);
    }

    public ServerResponse<ScalarGridSlice> getTopoData(GridLocation gloc)
            throws GFEServerException {
        GetTopoDataRequest request = new GetTopoDataRequest(gloc);
        return (ServerResponse<ScalarGridSlice>) makeRequest(request, false);
    }
}
