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
import com.raytheon.uf.common.dataplugin.text.request.ExecuteAwipsCmdRequest;
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
 * 21May2010    2187       cjeanbap    Add operational mode functionality.
 * 05Jun2011    9740       cjeanbap    Fixed invalid character, Form Feed.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class AWIPSCommand implements ICommand {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AWIPSCommand.class);

    /**
     * 
     */
    private String awipsid = null;

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
    public AWIPSCommand(String awipsid, String wmoId, String site,
            String hdrtime, String bbbid) {
        this.awipsid = awipsid;
        this.wmoId = wmoId;
        this.site = site;
        this.hdrtime = hdrtime;
        this.bbbid = bbbid;
    }

    /**
     * 
     */
    @Override
    public CommandType getType() {
        return CommandType.AWIPS;
    }

    /**
     * 
     */
    @Override
    public boolean isValid() {
        return awipsid != null && awipsid.length() == 6;
    }

    /**
     * 
     */
    @Override
    public String[] getCommandTextFields() {
        String[] rval = new String[2];
        rval[0] = "AWIPS:";
        rval[1] = (awipsid != null ? awipsid : "");
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.command.ICommand#executeCommand(com.raytheon
     * .uf.edex.services.textdbsrv.IQueryTransport)
     */
    @SuppressWarnings("unchecked")
    @Override
    public List<StdTextProduct> executeCommand(IQueryTransport transport)
            throws CommandFailedException {
        // TODO verify both fields blank not allowed
        if (awipsid == null) {
            throw new CommandFailedException("Awips Id not set");
        } else if (transport == null) {
            throw new CommandFailedException("Command transport method not set");
        }

        ExecuteAwipsCmdRequest req = new ExecuteAwipsCmdRequest();

        if (awipsid != null && awipsid.length() > 0) {
            req.setNnnXxx(awipsid);
        }

        if (wmoId != null && wmoId.length() > 0) {
            req.setWmoId(wmoId);
        }

        if (site != null && site.length() > 0) {
            req.setSite(site);
        }

        if (hdrtime != null && hdrtime.length() > 0) {
            req.setHdrTime(hdrtime);
        }

        if (bbbid != null && bbbid.trim().length() > 0) {
            req.setBbb(bbbid);
        }

        req.setFullDataRead(false);
        req.setOperationalMode(CAVEMode.OPERATIONAL.equals(CAVEMode.getMode())
                || CAVEMode.TEST.equals(CAVEMode.getMode()));

        List<StdTextProduct> response = null;
        try {
            long t3 = System.currentTimeMillis();
            Object resp = ThriftClient.sendRequest(req);
            System.out
                    .println("Time spent on AWIPSCommand.executeCommand()'s ThriftClient.sendRequest():"
                            + (System.currentTimeMillis() - t3));
            response = (List<StdTextProduct>) resp;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return response;
    }
}
