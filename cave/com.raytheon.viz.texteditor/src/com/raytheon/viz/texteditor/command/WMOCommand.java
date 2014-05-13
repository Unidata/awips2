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
package com.raytheon.viz.texteditor.command;

import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.dbsrv.IQueryTransport;
import com.raytheon.uf.common.dataplugin.text.request.ExecuteWmoCmdRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * Pairs a command and its associated type.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2009 2191       rjpeter     Initial creation
 * Apr 14, 2010 4734       mhuang      Corrected StdTextProduct import 
 *                                      dependency
 * 21May2010    2187        cjeanbap   Add operational mode functionality.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class WMOCommand implements ICommand {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WMOCommand.class);

    /**
     * 
     */
    private String wmoId = null;

    /**
     * 
     */
    private String site = null;

    /**
     * 
     */
    private String awipsid = null;

    /**
     * 
     */
    private String hdrtime = null;

    /**
     * 
     */
    private String bbbid = null;

    /**
     * 
     * @param browserCallBack
     * @param wmoId
     * @param site
     * 
     */
    public WMOCommand(String wmoId, String site, String awipsid,
            String hdrtime, String bbbid) {
        this.wmoId = wmoId;
        this.site = site;
        this.awipsid = awipsid;
        this.hdrtime = hdrtime;
        this.bbbid = bbbid;
    }

    /**
     * 
     */
    @Override
    public CommandType getType() {
        return CommandType.WMO;
    }

    /**
     * 
     */
    @Override
    public boolean isValid() {
        // TODO: implement
        return true;
    }

    /**
     * 
     */
    @Override
    public String[] getCommandTextFields() {
        String[] rval = new String[3];

        rval[0] = "WMO:";
        rval[1] = (wmoId != null ? wmoId : "");
        rval[2] = (site != null ? site : "");
        return rval;
    }

    /**
     * 
     */
    @SuppressWarnings("unchecked")
    @Override
    public List<StdTextProduct> executeCommand(IQueryTransport transport)
            throws CommandFailedException {
        // TODO verify both fields blank not allowed
        if (wmoId == null && site == null) {
            throw new CommandFailedException("WMO Id not set");
        } else if (transport == null) {
            throw new CommandFailedException("Command transport method not set");
        }

        ExecuteWmoCmdRequest req = new ExecuteWmoCmdRequest();
        CAVEMode mode = CAVEMode.getMode();

        if (wmoId != null && wmoId.length() > 0) {
            req.setWmoId(wmoId);
        }

        if (site != null && site.length() > 0) {
            req.setSite(site);
        }

        if (awipsid != null && awipsid.length() > 0) {
            req.setNnnXxx(awipsid);
        }

        if (hdrtime != null && hdrtime.length() > 0) {
            req.setHdrTime(hdrtime);
        }

        if (bbbid != null && bbbid.trim().length() > 0) {
            req.setBbb(bbbid);
        }

        req.setOperationalMode(CAVEMode.OPERATIONAL.equals(mode)
                || CAVEMode.TEST.equals(mode));

        List<StdTextProduct> response = null;
        try {
            long t3 = System.currentTimeMillis();
            Object resp = ThriftClient.sendRequest(req);
            System.out
                    .println("Time spent on WMOCommand.executeCommand()'s ThriftClient.sendRequest():"
                            + (System.currentTimeMillis() - t3));
            response = (List<StdTextProduct>) resp;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return response;
    }
}
