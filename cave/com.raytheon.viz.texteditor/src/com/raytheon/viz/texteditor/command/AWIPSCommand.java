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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.ExecuteAwipsCmdRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
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
 * 09Sep2014    3580       mapeters    Removed IQueryTransport usage 
 *                                     (no longer exists).
 * 12Feb2016    4716       rferrel     {@link #getCommandTextFields()} added site information.
 * 28Aug2016    5839       rferrel     Fix for version number queries.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class AWIPSCommand implements ICommand {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AWIPSCommand.class);

    private boolean singleProduct = false;

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
     */
    private String pastVersion;

    /**
     * 
     * @param awipsid
     * @param wmoId
     * @param site
     * @param hdrtime
     * @param bbbid
     * @param pastVersion
     * @param singleProduct
     */
    public AWIPSCommand(String awipsid, String wmoId, String site,
            String hdrtime, String pastVersion, String bbbid,
            boolean singleProduct) {
        this.awipsid = awipsid;
        this.wmoId = wmoId;
        this.site = site;
        this.hdrtime = hdrtime;
        this.pastVersion = pastVersion;
        this.bbbid = bbbid;
        this.singleProduct = singleProduct;
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
        return (awipsid != null) && (awipsid.length() >= 6);
    }

    /**
     * 
     */
    @Override
    public String[] getCommandTextFields() {
        String[] rval = new String[2];
        rval[0] = "AWIPS:";
        StringBuilder sb = new StringBuilder();
        if ("000000".equals(hdrtime)) {
            sb.append("ALL:");
        } else if (!StringUtil.isEmptyString(pastVersion)
                && !"0".equals(pastVersion)) {
            sb.append("-").append(pastVersion).append(":");
        }
        if ((site != null) && !"0000".equals(site)) {
            sb.append(site);
        }
        if (awipsid != null) {
            sb.append(awipsid);
        }
        rval[1] = sb.toString();
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
    public List<StdTextProduct> executeCommand() throws CommandFailedException {
        // TODO verify both fields blank not allowed
        if (awipsid == null) {
            throw new CommandFailedException("Awips Id not set");
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

        if ((pastVersion != null) && (pastVersion.trim().length() > 0)) {
            req.setPastVersion(pastVersion);
        }

        req.setFullDataRead(false);
        req.setOperationalMode(CAVEMode.OPERATIONAL.equals(CAVEMode.getMode())
                || CAVEMode.TEST.equals(CAVEMode.getMode()));

        List<StdTextProduct> response = null;
        try {
            long t3 = System.currentTimeMillis();
            Object resp = ThriftClient.sendRequest(req);
            System.out.println(
                    "Time spent on AWIPSCommand.executeCommand()'s ThriftClient.sendRequest():"
                            + (System.currentTimeMillis() - t3));
            response = (List<StdTextProduct>) resp;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        if (singleProduct && (response != null) && (response.size() > 1)) {
            StdTextProduct rval = response.get(0);
            StringBuilder builder = new StringBuilder(rval.getProduct());
            for (int i = 1; i < response.size(); i++) {
                StdTextProduct curProd = response.get(i);
                builder.append('\n');
                builder.append(curProd.getProduct());
            }
            rval.setProduct(builder.toString());
            response = new ArrayList<>(1);
            response.add(rval);
        }

        return response;
    }

    public boolean isSingleProduct() {
        return singleProduct;
    }

    public void setSingelProduct(boolean singleProduct) {
        this.singleProduct = singleProduct;
    }

    @Override
    public ICommand getNext() {
        int version = 0;
        if (!StringUtil.isEmptyString(pastVersion)) {
            try {
                version = Integer.valueOf(pastVersion) - 1;
                if (version < 0) {
                    version = 0;
                }
            } catch (NumberFormatException ex) {
                statusHandler.debug("Bad pastVersion value: " + pastVersion);
            }
        }

        ICommand cmd = new AWIPSCommand(awipsid, wmoId, site, null,
                Integer.toString(version), bbbid, true);

        return cmd;
    }

    @Override
    public ICommand getPrevious() {
        int version = 1;
        if (!StringUtil.isEmptyString(pastVersion)) {
            try {
                version = Integer.valueOf(pastVersion) + 1;
            } catch (NumberFormatException ex) {
                statusHandler.debug("Bad pastVersion value: " + pastVersion);
            }
        }
        ICommand cmd = new AWIPSCommand(awipsid, wmoId, site, null,
                Integer.toString(version), bbbid, true);
        return cmd;
    }

    @Override
    public ICommand getAll() {
        return new AWIPSCommand(awipsid, wmoId, site, "000000", null, bbbid,
                true);
    }

    @Override
    public ICommand getLatest() {
        return new AWIPSCommand(awipsid, wmoId, site, null, null, bbbid, true);
    }
}
