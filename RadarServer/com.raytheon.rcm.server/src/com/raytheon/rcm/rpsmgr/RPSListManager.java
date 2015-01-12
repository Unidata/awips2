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
package com.raytheon.rcm.rpsmgr;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.ListIterator;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.EndpointConfig;
import com.raytheon.rcm.config.LinkResource;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.Util;
import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.ConfigEvent.Category;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.message.MD;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.ProductRequest;
import com.raytheon.rcm.products.ElevationInfo;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.ProductInfo.Selector;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;
import com.raytheon.rcm.request.Filter;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.request.RpsListFormatter;
import com.raytheon.rcm.request.Sequence;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.server.RadarServer;
import com.raytheon.rcm.server.StatusManager.RadarStatus;

/* TODO: Log PRR messages for requests the mgr sent? (RPSHandler::handlePRR)
 */

/**
 * Manages current RPS lists and requests for changes to RPS lists. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2009-04-22   #1693      D. Friedman Initial checkin
 * ...
 * 2013-01-31   DR 15458   D. Friedman Explicitly handle UNSPECIFIED_VCP.
 * 2014-02-03   DR 14762   D. Friedman Handle updated national RPS lists.
 * </pre>
 * 
 */
public class RPSListManager extends RadarEventAdapter {

    // All RPS list requests will use this for the sequence number.
    private static final short RPS_LIST_REQUEST_SEQUENCE_NUMBER = 1;

    RadarServer radarServer;

    // on sendRpsList (not store), set. on (true) vcp change, remove. should
    // persist...
    ConcurrentHashMap<String, RpsList> currentRpsLists = new ConcurrentHashMap<String, RpsList>();

    ConcurrentHashMap<String, Integer> currentVcps = new ConcurrentHashMap<String, Integer>();

    /**
     * Indicates RadarServer should generate AWIPS 1 style RPS lists. There does
     * not seem to be a need to make this an actual configuration option.
     */
    private static boolean generateCompatibilityRpsLists = true;

    public RPSListManager(RadarServer radarServer) {
        this.radarServer = radarServer;

        /*
         * Kludge to prevent lists from being purged. Should be removed when no
         * longer needed.
         */
        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                while (true) {
                    try {
                        Thread.sleep(1 * 60 * 60 * 1000); // one hour
                        regenerateCompatibilityRpsLists();
                    } catch (InterruptedException e) {
                        // nothing
                    }
                }
            }
        });
        t.setDaemon(true);
        t.start();
    }

    public RpsList getCurrentRpsList(String radarID) {
        return currentRpsLists.get(radarID);
    }

    public String sendRpsList(String radarID, RpsList list, boolean store) {
        String error = sendRpsList2(radarID, list, store);
        if (error == null)
            return null;
        else {
            Log.errorf("Cannot send RPS list to radar %s: %s", radarID, error);
            return error;
        }
    }

    private String storeRpsList(RadarConfig rc, RpsList list) {
        try {
            radarServer.getConfiguration().setLocalRpsList(rc.getRadarID(),
                    list);
            return null;
        } catch (IOException e) {
            return String.format("Cannot store RPS list for radar '%s': %s",
                    rc.getRadarID(), e.toString());
        }
    }

    private String sendRpsList2(String radarID, RpsList list, boolean store) {
        // TODO: these are errors too...
        RadarConfig rc = radarServer.getConfiguration().getConfigForRadar(
                radarID);

        if (rc == null)
            return "Unknown radar";
        if (!rc.isDedicated())
            return "Not a dedicated radar";

        if (store) {
            String error = storeRpsList(rc, list);
            // TODO: should continue to try to send. Need to keep track of
            // multiple errors...
            if (error != null)
                return error;
        }

        RadarStatus status = radarServer.getStatusManager().getRadarStatus(
                radarID);
        int opMode;
        int currentVCP;
        int[] gsmCuts;
        try {
            byte[] msg = status.getCurrentGSM();
            if (msg == null)
                return "Not connected or no status information received yet";
            GSM gsm = GSM.decode(msg);
            opMode = gsm.opMode;
            currentVCP = gsm.vcp;
            gsmCuts = gsm.cuts;
        } catch (RuntimeException e) {
            Log.errorf("Error reading GSM: %s", e);
            // TODO: losing information
            return "Error getting radar status";
        }
        
        int[] cuts = ElevationInfo.getInstance().getScanElevations(radarID, currentVCP);
        if (cuts == null && Util.getRadarType(rc) == RadarType.WSR)
            cuts = gsmCuts;

        if (list.getVcp() != RpsList.UNSPECIFIED_VCP && list.getVcp() != currentVCP) {
            if (store)
                return null; // TODO: Should warn instead.
            else
                return String.format("RPS list for VCP %d.  Current VCP is %d",
                        list.getVcp(), currentVCP);
        }

        list = (RpsList) list.clone();
        list.setVcp(currentVCP);

        list = getMergedRpsListForRadar(rc, opMode, currentVCP, cuts, list);
        if (list != null) {
            // TODO: Should persist this
            currentRpsLists.put(rc.getRadarID(), (RpsList) list.clone());

            sendList(rc, list);

            return null;
        } else {
            return "Could not create RPS list.";
        }
    }

    public void handleRadarEvent(RadarEvent event) {
        if (event.getType() == RadarEvent.MESSAGE_RECEIVED
                && Message.messageCodeOf(event.getMessageData()) == Message.GSM) {
            RadarConfig rc = radarServer.getConfiguration().getConfigForRadar(
                    event.getRadarID());
            if (rc != null && rc.isDedicated())
                handleGSM(rc, event.getMessageData());
        } else if (event.getType() == RadarEvent.CONNECTION_UP) {
            /*
             * Forget the current VCP so that when the GSM is received, a list
             * will always be sent.
             */
            currentVcps.remove(event.getRadarID());
        }
    }

    private void handleGSM(RadarConfig rc, byte[] gsmData) {
        GSM gsm = (GSM) MD.decode(gsmData);

        Integer lastVcp = currentVcps.get(rc.getRadarID());
        if (lastVcp != null && gsm.vcp == lastVcp.intValue())
            return;
        currentVcps.put(rc.getRadarID(), gsm.vcp);

        // TODO: if configuration changes, the persisted list may not be valid
        RpsList rpsList = currentRpsLists.get(rc.getRadarID());

        if (rpsList != null && rpsList.getVcp() != gsm.vcp)
            rpsList = null;

        if (rpsList == null) {
            int[] cuts = ElevationInfo.getInstance().
                getScanElevations(rc.getRadarID(), gsm.vcp);
            if (cuts == null && Util.getRadarType(rc) == RadarType.WSR)
                cuts = gsm.cuts;

            rpsList = getMergedRpsListForRadar(rc, gsm.opMode, gsm.vcp,
                    cuts, null);

            // TODO: Should persist this (wouldn't need clone code)
            if (rpsList != null)
                currentRpsLists.put(rc.getRadarID(), (RpsList) rpsList.clone());
        }

        if (rpsList != null)
            sendList(rc, rpsList);
    }

    /**
     * From AWIPS 1 ProductRequestList::addMiniVolumeProduct() (DCS 3411)
     * 
     * For TDWR VCP80, 2 products might be generated in one volume (2 mini
     * volumes) for some products. Parameter lowerLayer is used to specify these
     * 2 products: 1 - generated at mid volume; 2 - generated at end volume
     * (default) Product 2 is the default. Here we add product 1 for VCP80 --
     * implementation ---------------------------------------------------------
     * Searches list for mini volume products, add a new entry with lowerLayer 1
     * CZ(35-38),ET(41),VIL(57),STI(58),HI(59),TVS(61),MD(141),DMD(149)
     * 
     */
    /*
     * This must be kept in sync with 
     * com.raytheon.rcm.request.RpsList.getAdditionalMiniVolumeProductCount.
     */
    private void maybeAddSPGMiniVolumeProducts(RadarConfig rc,
            ArrayList<Request> reqs, int vcp) {
        if (vcp != 80)
            return;

        /*
         * We want to put these products at the end of the list so that they
         * do not have priority over existing products.
         */
        Selector sel = new Selector();
        sel.radarType = RadarType.TDWR;
        
        int end = reqs.size();
        for (int i = 0; i < end; ++i) {
            Request r = reqs.get(i);
            sel.code = (int) r.productCode;
            RadarProduct prod = ProductInfo.getInstance().selectOne(sel);
            if (prod != null && prod.params.contains(Param.MINI_VOLUME) &&
                    r.getMiniVolume() != 1) {
                Request r2 = (Request) r.clone();
                r2.setMiniVolume(1);
                reqs.add(r2);
            }
        }
    }

    private void sendList(RadarConfig rc, RpsList rpsList) {
        Request[] reqs = rpsList.getRequests();
        for (int i = 0; i < reqs.length; i++) {
            reqs[i].sequence = Sequence.next(rc.getRadarID());
        }
        int maxSize = -1;

        RadarStatus status = radarServer.getStatusManager().getRadarStatus(
                rc.getRadarID());
        if (status != null) {
            LinkResource lr = status.getLinkResource();
            if (lr != null)
                maxSize = lr.getMaxRpsListSize();
        }
        
        int requestCount = rpsList.getRequestCount(rc.getRadarID(),
                Util.getRadarType(rc));

        if (maxSize < 0)
            Log.warnf("Cannot determine maximum RPS list size for %s",
                    rc.getRadarID());
        if (requestCount < 0)
            Log.warnf("Cannot number of requests in RPS list for %s", rc
                    .getRadarID());
        if (maxSize >= 0 && requestCount >= 0 && requestCount > maxSize) {
            int truncCount = 0;
            int i;
            for (i = reqs.length - 1; i >= 0; --i) {
                truncCount += RpsList.getRequestCount(reqs[i], rc.getRadarID(), rpsList.getVcp(),
                        Util.getRadarType(rc));
                if (requestCount - truncCount <= maxSize)
                    break;
            }
            if (i < 0)
                i = 0;
            
            int originalCount = requestCount;
            requestCount = requestCount - truncCount;
            Log.warnf("Truncated list for %s from %d entries (%d requests) to %d entries (%d requests)",
                    rc.getRadarID(), reqs.length, originalCount, i, requestCount);
            // TODO: Also need to send a message to Guardian
            reqs = Arrays.copyOf(reqs, i);
            rpsList = new RpsList(rpsList.getOpMode(), rpsList.getVcp(), reqs);
        } else if (reqs.length < 1) {
            Log.warnf("Sending empty RPS list to %s", rc.getRadarID());
        }

        Log.eventf("%s: Sending RPS list with %d entries (%d requests)", rc.getRadarID(),
                rpsList.getRequests().length, requestCount);
        byte[] msg = ProductRequest.encode(rpsList.getRequests());
        radarServer.getConnectionManager().sendMessageToRadar(rc.getRadarID(),
                msg);

        if (generateCompatibilityRpsLists)
            generateCompatibilityRpsList(rc, rpsList);
    }

    private Object listGenerationLock = new Object();

    /**
     * Generate an AWIPS 1 style RPS list. Some AWIPS 1 applications (such as
     * FSI) need to know the current RPS list. The lists will be created in
     * <data archive radar dir>/lists/.
     */
    private void generateCompatibilityRpsList(RadarConfig rc, RpsList rpsList) {
        try {
            EndpointConfig ec = radarServer.getConfiguration()
                    .getEndpointConfig();
            if (ec == null)
                return;

            File dir = new File(ec.getArchiveRoot());
            if (ec.getPrefixPathWithRadar() != null
                    && (boolean) ec.getPrefixPathWithRadar())
                dir = new File(dir, "radar");
            dir = new File(dir, "lists");
            if (!dir.exists())
                dir.mkdirs();
            File path = new File(dir, rc.getRadarID().toUpperCase()
                    + ".current");

            synchronized (listGenerationLock) {

                FileOutputStream fo = new FileOutputStream(path);
                try {
                    PrintWriter p = new PrintWriter(fo);
                    RpsListFormatter.formatAwips1RpsList(rpsList,
                            path.getName(), p);
                    p.flush();
                } finally {
                    fo.close();
                }

                // It us unknown if any application rely on the currentVCP file.
                path = new File(dir, rc.getRadarID().toUpperCase()
                        + ".currentVCP");
                fo = new FileOutputStream(path);
                try {
                    PrintWriter p = new PrintWriter(fo);
                    p.printf("VCP%d\n", rpsList.getVcp());
                    p.flush();
                } finally {
                    fo.close();
                }

            }

        } catch (Exception e) {
            Log.errorf("Error generating AWIPS 1 compatibility RPS list: %s", e);
        }
    }

    private void regenerateCompatibilityRpsLists() {
        /*
         * Kludge to prevent lists from being purged. Should be removed when no
         * longer needed.
         */

        if (generateCompatibilityRpsLists) {
            Configuration config = radarServer.getConfiguration();
            for (String radarID : config.getConfiguredRadarList()) {
                RadarConfig rc = config.getConfigForRadar(radarID);
                RpsList rpsList = currentRpsLists.get(radarID);
                if (rc != null & rpsList != null)
                    generateCompatibilityRpsList(rc, rpsList);
            }
        }
    }

    /**
     * Constructs a an RPS list for the given parameters, merging national and
     * local lists as appropriate.
     * 
     * @param rc
     * @param opMode
     * @param vcp
     * @param elevList
     *            list of elevation angles used for template processing and
     *            duplicate filtering
     * @param local
     *            if not <code>null</code>, overrides the site-local list that
     *            would normally be used
     * @return the merged RPS list, or <code>null</code> if no RPS list
     *         resources were found
     */
    private RpsList getMergedRpsListForRadar(RadarConfig rc, int opMode,
            int vcp, int[] elevList, RpsList local) {

        Configuration config = radarServer.getConfiguration();
        RpsList natl = null;
        if (rc.isCollectionEnabled())
            natl = config.getNationalRpsList(rc.getRadarID(), opMode, vcp,
                    elevList);

        if (local == null)
            local = config.getLocalRpsList(rc.getRadarID(), opMode, vcp,
                    elevList);

        if (natl == null && local == null) {
            RadarType radarType = Util.getRadarType(rc);

            /*
             * We do not currently send RPS lists to ASRs so not having a list
             * in this case should not be considered an error.
             */
            if (radarType != RadarType.ASR && radarType == RadarType.ARSR)
                Log.errorf("No RPS lists defined for radar %s mode=%d vcp=%d",
                        rc.getRadarID(), opMode, vcp);

            return null;
        }

        ArrayList<Request> reqs = new ArrayList<Request>();
        /*
         * The duplicate filter code below favors entries earlier in the list.
         * Entries from the national list are supposed to have priority (ref:
         * SMM -- Does that mean temporal priority or max-list-size-cutoff
         * priority?), so they are added first.
         */
        if (natl != null)
            reqs.addAll(Arrays.asList(natl.getRequests()));
        if (local != null)
            reqs.addAll(Arrays.asList(local.getRequests()));

        validateRPSEntries(reqs);
        maybeAddSPGMiniVolumeProducts(rc, reqs, vcp);

        /*
         * AWIPS 1 disabled duplicate merging for TDWRs because some requests
         * in the national RPS list would disappear. This was due to incorrect
         * handling of multi-elevation requests. AWIPS 2 handles
         * multi-elevation request correctly. (See DCS 3472, DRs 19386, 20239, 
         * and 20244.) 
         * 
         * if (vcp == 80 || vcp == 90)
         *     elevList = null;
         */

        RadarType radarType = Util.getRadarType(rc);

        iLoop: for (int i = 0; i < reqs.size(); ++i) {
            Request ri = reqs.get(i);
            for (int j = i + 1; j < reqs.size(); ++j) {
                Request rj = reqs.get(j);
                if (ri.productCode == rj.productCode) {
                    Filter filter = Filter.getFilterForCode(ri.productCode);
                    Request mr = filter.mergeRequests(ri, rj, elevList,
                            radarType);
                    if (mr == ri) {
                        reqs.remove(j);
                        --j;
                    } else if (mr == rj) {
                        /*
                         * Move the "larger" request to appear earlier in the
                         * list. Reason: It would be strange to delete a
                         * single-elevation request from the list only to have a
                         * later multi-elevation request that subsumes it to be
                         * lost when the list is truncated to fix the maximum
                         * number of products.
                         * 
                         * On the other hand, this could cause products listed
                         * before index j to be lost because the RPG counts each
                         * elevation in a multi-elevation request as a separate
                         * request.
                         */
                        reqs.set(i, rj);
                        reqs.remove(j);
                        --i;
                        continue iLoop;
                    }
                }
            }
        }

        return new RpsList(opMode, vcp, reqs.toArray(new Request[0]));
    }

    /**
     * Check RPS list entries for correctness. 1. Ensures the repeat count is -1
     * and volume scan selection is 'current'. 2. Sets the sequence number to
     * RPS_LIST_REQUEST_SEQUENCE_NUMBER.
     */
    private void validateRPSEntries(ArrayList<Request> reqs) {
        int nOTR = 0;
        int nZero = 0;

        ListIterator<Request> l = reqs.listIterator();
        while (l.hasNext()) {
            Request req = l.next().clone();
            req.sequence = RPS_LIST_REQUEST_SEQUENCE_NUMBER;
            if (req.count != Request.CONTINUOUS
                    || req.getVolumeScanSelection() != Request.SELECT_CURRENT) {
                ++nOTR;
                req.count = Request.CONTINUOUS;
                req.selectCurrent();
            }
            if (req.interval < 1) {
                /*
                 * An interval of zero in the RPS list will kill the p_server
                 * process on the RPG.
                 */
                req.interval = 1;
                ++nZero;
            }
            l.set(req);
        }
        if (nOTR > 0)
            Log.warnf("Had to fix %d OTR-style entries in the RPS list.", nOTR);
        if (nZero > 0)
            Log.warnf("Had to fix %d zero-interval entries in the RPS list.",
                    nZero);
    }

    @Override
    public void handleConfigEvent(ConfigEvent event) {
        if (event.getRadarID() != null) {
            RadarConfig oldCfg = event.getOldConfig();
            RadarConfig newCfg = event.getNewConfig();
            if (oldCfg != null
                    && newCfg != null
                    && newCfg.isDedicated()
                    && (oldCfg.isCollectionEnabled() != newCfg
                            .isCollectionEnabled())) {
                resetRpsListForRadar(newCfg);
            }
        } else if (event.getCategory() == Category.NATIONAL_RPS_LISTS) {
            Configuration config = radarServer.getConfiguration();
            for (String radarID : config.getConfiguredRadarList()) {
                RadarConfig rc = config.getConfigForRadar(radarID);
                if (rc.isCollectionEnabled()) {
                    resetRpsListForRadar(rc);
                }
            }
        }
    }

    private void resetRpsListForRadar(RadarConfig rc) {
        String radarID = rc.getRadarID();
        RadarStatus status = radarServer.getStatusManager()
                .getRadarStatus(radarID);
        byte[] gsmData = null;
        if (status != null)
            gsmData = status.getCurrentGSM();
        currentVcps.remove(radarID);
        currentRpsLists.remove(radarID);
        if (gsmData != null) {
            handleGSM(rc, gsmData);
        } else {
            Log.debugf(
                    "RPS-relevant configuration changed for %s, but "
                            + "it is not connected.  Cannot send a list now.",
                    radarID);
        }
    }
}
