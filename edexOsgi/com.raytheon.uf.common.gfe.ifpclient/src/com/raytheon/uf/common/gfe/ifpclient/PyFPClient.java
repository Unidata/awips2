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

import java.util.Arrays;
import java.util.List;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockTableRequest;
import com.raytheon.uf.common.gfe.ifpclient.exception.GfeServerRequestException;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.TimeRange;

/**
 * An IFPClient that matches the "legacy" A2 IFPClient implementation where the
 * ServerResponse's payload were returned and exceptions were thrown if the
 * ServerResponse contained an error message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 04, 2015  #5129     dgilling     Initial creation
 * Feb 24, 2016  #5129     dgilling     Add getGridInventory, alternative 
 *                                      constructor.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class PyFPClient {

    private final IFPClient client;

    /**
     * @param wsId
     * @param siteId
     */
    public PyFPClient(WsId wsId, String siteId) {
        this(new IFPClient(wsId, siteId));
    }

    public PyFPClient(IFPClient client) {
        this.client = client;
    }

    private static void checkSR(final String operation,
            final ServerResponse<?> sr) throws GfeServerRequestException {
        if (!sr.isOkay()) {
            throw new GfeServerRequestException(String.format("%s failed: %s",
                    operation, sr.message()));
        }
    }

    public List<ParmID> getParmList(final DatabaseID id)
            throws GfeServerRequestException {
        ServerResponse<List<ParmID>> sr = client.getParmList(id);
        checkSR("IFPClient::getParmList()", sr);
        return sr.getPayload();
    }

    public List<String> getSiteID() throws GfeServerRequestException {
        ServerResponse<String> sr = client.getSiteID();
        checkSR("IFPClient::getSiteID()", sr);
        return Arrays.asList(sr.getPayload());
    }

    public List<ActiveTableRecord> getVTECActiveTable(
            final ActiveTableMode tableName) throws GfeServerRequestException {
        ServerResponse<List<ActiveTableRecord>> sr = client
                .getVTECActiveTable(tableName);
        checkSR("IFPClient::getVTECActiveTable()", sr);
        return sr.getPayload();
    }

    public void clearVTECTable(final String tableName)
            throws GfeServerRequestException {
        ServerResponse<?> sr = client.clearVTECTable(ActiveTableMode
                .valueOf(tableName));
        checkSR("IFPClient::clearVTECTable()", sr);
    }

    public String getSiteTimeZone() throws GfeServerRequestException {
        return getSiteTimeZone(getSiteID().get(0));
    }

    public String getSiteTimeZone(final String id)
            throws GfeServerRequestException {
        ServerResponse<String> sr = client.getSiteTimeZone(id);
        checkSR("IFPClient::getSiteTimeZone()", sr);
        return sr.getPayload();
    }

    public List<ReferenceID> getReferenceInventory()
            throws GfeServerRequestException {
        ServerResponse<List<ReferenceID>> sr = client.getReferenceInventory();
        checkSR("IFPClient::getReferenceInventory()", sr);
        return sr.getPayload();
    }

    public List<ReferenceData> getReferenceData(final List<ReferenceID> ids)
            throws GfeServerRequestException {
        ServerResponse<List<ReferenceData>> sr = client.getReferenceData(ids);
        checkSR("IFPClient::getReferenceData()", sr);
        return sr.getPayload();
    }

    public void saveReferenceData(final List<ReferenceData> data)
            throws GfeServerRequestException {
        ServerResponse<?> sr = client.saveReferenceData(data);
        checkSR("IFPClient::saveReferenceData()", sr);
    }

    public void deleteReferenceData(final List<ReferenceID> ids)
            throws GfeServerRequestException {
        ServerResponse<?> sr = client.deleteReferenceData(ids);
        checkSR("IFPClient::deleteReferenceData()", sr);
    }

    public GridParmInfo getGridParmInfo(final ParmID id)
            throws GfeServerRequestException {
        ServerResponse<GridParmInfo> sr = client.getGridParmInfo(id);
        checkSR("IFPClient::getGridParmInfo()", sr);
        return sr.getPayload();
    }

    public GridLocation getDBGridLocation() throws GfeServerRequestException {
        ServerResponse<GridLocation> sr = client.getDBGridLocation();
        checkSR("IFPClient::getDBGridLocation()", sr);
        return sr.getPayload();
    }

    public List<DatabaseID> getOfficialDBName()
            throws GfeServerRequestException {
        ServerResponse<List<DatabaseID>> sr = client.getOfficialDBName();
        checkSR("IFPClient::getOfficialDBName()", sr);
        return sr.getPayload();
    }

    public List<String> getClientList() throws GfeServerRequestException {
        ServerResponse<List<String>> sr = client.getClientList();
        checkSR("IFPClient::getClientList()", sr);
        return sr.getPayload();
    }

    public List<String> getKnownSites() throws GfeServerRequestException {
        ServerResponse<List<String>> sr = client.getKnownSites();
        checkSR("IFPClient::getKnownSites()", sr);
        return sr.getPayload();
    }

    /*
     * PyObject *getKnownSitesWithOfficeType(void) { SeqOf<TextString> sites,
     * officeTypes; checkSR("IFPClient::getKnownSitesWithOfficeType()",
     * self->getKnownSitesWithOfficeType(sites, officeTypes)); Dictionary d; for
     * (int i = 0; i < sites.length(); i++) d.add(Object(sites[i].stringPtr()),
     * Object(officeTypes[i].stringPtr())); return d.asRef(); }
     */

    public List<LockTable> getLockTables() throws GfeServerRequestException {
        ServerResponse<List<LockTable>> sr = client
                .getLockTable(new LockTableRequest());
        checkSR("IFPClient::getLockTable()", sr);
        return sr.getPayload();
    }

    public List<LockTable> requestLockChange(final List<LockRequest> lockRequest)
            throws GfeServerRequestException {
        ServerResponse<List<LockTable>> sr = client
                .requestLockChange(lockRequest);
        checkSR("IFPClient::requestLockChange()", sr);
        return sr.getPayload();
    }

    public IFPClient getJavaClient() {
        return client;
    }

    public List<TimeRange> getGridInventory(final ParmID parmID)
            throws GfeServerRequestException {
        ServerResponse<List<TimeRange>> sr = client.getGridInventory(parmID);
        checkSR("IFPClient::getGridInventory()", sr);
        return sr.getPayload();
    }
}
