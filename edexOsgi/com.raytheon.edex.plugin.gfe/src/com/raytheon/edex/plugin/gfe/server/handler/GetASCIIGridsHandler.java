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
package com.raytheon.edex.plugin.gfe.server.handler;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.plugin.gfe.ifpAG.ASCIIGrid;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetASCIIGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.TimeRange;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2011  #8983     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GetASCIIGridsHandler implements
        IRequestHandler<GetASCIIGridsRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<String> handleRequest(GetASCIIGridsRequest request)
            throws Exception {
        ServerResponse<String> sr = new ServerResponse<String>();

        // get the grid slices
        List<IGridSlice> gridSlices = getGridSlices(request.getDatabaseIds(),
                request.getParmIds(), request.getTimeRange());
        ASCIIGrid aGrid = new ASCIIGrid(gridSlices,
                request.getCoordConversionString(), request.getSiteID());

        LocalizationFile tempFile = getTempFile(request.getWorkstationID());
        aGrid.outputAsciiGridData(tempFile.getFile());
        tempFile.save();

        sr.setPayload(tempFile.getName());

        return sr;
    }

    private List<IGridSlice> getGridSlices(List<DatabaseID> databaseIds,
            List<ParmID> parmIds, TimeRange tr) {
        List<IGridSlice> gridSlices = new ArrayList<IGridSlice>();

        // if parms are specified, get their grid slice
        if (parmIds.size() > 0) {
            for (ParmID parmId : parmIds) {
                // get the time ranges from the inventory
                ServerResponse<List<TimeRange>> sr = GridParmManager
                        .getGridInventory(parmId);
                if (sr.isOkay()) {
                    List<TimeRange> timeRanges = sr.getPayload();

                    // filter out the time ranges that are not within the
                    // command line specified time range
                    List<TimeRange> acceptedTrs = new ArrayList<TimeRange>();
                    for (TimeRange timeRange : timeRanges) {
                        if (checkRange(tr, timeRange)) {
                            acceptedTrs.add(timeRange);
                        }
                    }

                    // make the GetGridRequest
                    GetGridRequest request = new GetGridRequest(parmId,
                            acceptedTrs);
                    List<GetGridRequest> requests = new ArrayList<GetGridRequest>();
                    requests.add(request);

                    // get the grid slices for the parm
                    ServerResponse<List<IGridSlice>> sr2 = GridParmManager
                            .getGridData(requests);
                    if (sr2.isOkay()) {
                        gridSlices.addAll(sr2.getPayload());
                    }
                }

            }
        } else {
            // if no parms are specified, get gridSlices for all parms in
            // specified database(s)
            for (DatabaseID dbId : databaseIds) {
                // get the parm list for the database
                ServerResponse<List<ParmID>> sr = GridParmManager
                        .getParmList(dbId);
                if (sr.isOkay()) {
                    List<ParmID> parmList = sr.getPayload();

                    // get the data for each parm
                    for (ParmID parm : parmList) {
                        // get the time ranges from the inventory
                        ServerResponse<List<TimeRange>> sr2 = GridParmManager
                                .getGridInventory(parm);
                        if (sr2.isOkay()) {
                            List<TimeRange> timeRanges = sr2.getPayload();

                            // filter out the time ranges that are not within
                            // the
                            // command line specified time range
                            List<TimeRange> acceptedTrs = new ArrayList<TimeRange>();
                            for (TimeRange timeRange : timeRanges) {
                                if (checkRange(tr, timeRange)) {
                                    acceptedTrs.add(timeRange);
                                }
                            }

                            // make the GetGridRequest
                            GetGridRequest request = new GetGridRequest(parm,
                                    acceptedTrs);
                            List<GetGridRequest> requests = new ArrayList<GetGridRequest>();
                            requests.add(request);

                            // get the grid slices for the parm
                            ServerResponse<List<IGridSlice>> sr3 = GridParmManager
                                    .getGridData(requests);
                            if (sr3.isOkay()) {
                                gridSlices.addAll(sr3.getPayload());
                            }
                        }
                    }
                }
            }
        }

        return gridSlices;
    }

    /**
     * @param limitTime
     * @param t
     * @return
     */
    private boolean checkRange(TimeRange limitTime, TimeRange t) {
        return limitTime.contains(t);
    }

    private LocalizationFile getTempFile(WsId requestor) throws IOException {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
        ctx.setContextName(requestor.getUserName());

        File parentDir = pathManager.getFile(ctx, "/gfe/ifpAG");
        if (!parentDir.exists()) {
            parentDir.mkdirs();
        }

        File tmpFile = File.createTempFile("ifpAG", ".txt", parentDir);
        LocalizationFile locTmpFile = pathManager.getLocalizationFile(ctx,
                "/gfe/ifpAG/" + tmpFile.getName());

        return locTmpFile;

    }

}
