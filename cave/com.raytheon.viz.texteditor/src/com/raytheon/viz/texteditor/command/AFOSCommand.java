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

import com.raytheon.uf.common.dataplugin.text.StdTextProductContainer;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.ExecuteAfosCmdRequest;
import com.raytheon.uf.common.dataplugin.text.util.AFOSParser;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.AfosBrowserModel;

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
 * 02Aug2010    2187       cjeanbap    Update method signature to be consistent.
 * 20Mar2011    8561       jdortiz     Added enterEditor field.
 * May 23, 2012 14952      rferrel     Added refTime.
 * Sep 09, 2014 3580       mapeters    Removed IQueryTransport usage
 *                                     (no longer exists).
 * Aug 26, 2016 5839       rferrel     Implemented new ICommand methods
 * Oct 23, 2017 6045       tgurney     getFieldValue() check for null
 *
 * </pre>
 *
 * @author rjpeter
 */
public class AFOSCommand implements ICommand {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AFOSCommand.class);

    private AFOSParser parser = null;

    private boolean enterEditor = false;

    private Long refTime;

    public AFOSCommand(String afosCommand, String siteId, Long refTime) {
        parser = new AFOSParser(afosCommand, siteId);
        this.refTime = refTime;
    }

    @Override
    public CommandType getType() {
        return CommandType.AFOS;
    }

    @Override
    public boolean isValid() {
        return parser != null && parser.isValidCommand();
    }

    public boolean isEnterEditor() {
        return enterEditor;
    }

    @Override
    public String[] getCommandTextFields() {
        String[] rval = new String[1];
        rval[0] = parser.getAfosCommand();
        return rval;
    }

    @Override
    public List<StdTextProduct> executeCommand() throws CommandFailedException {
        if (parser == null) {
            throw new CommandFailedException("AFOS command not set");
        } else if (!parser.isValidCommand()) {
            throw new CommandFailedException(
                    "AFOS command is invalid: " + parser.getAfosCommand());
        }

        List<StdTextProduct> response = null;

        ExecuteAfosCmdRequest req = new ExecuteAfosCmdRequest();
        req.setAfosCommand(parser.getAfosCommand());

        req.setAfosLocale(AfosBrowserModel.getInstance().getLocalSite());

        if (refTime != null) {
            req.setRefTime(refTime);
            req.setReftimeMode(true);
        }

        CAVEMode mode = CAVEMode.getMode();
        boolean result = (CAVEMode.OPERATIONAL.equals(mode)
                || CAVEMode.TEST.equals(mode) ? true : false);
        req.setOperationalMode(result);

        // Check to see if the "enterEditor" flag was set.
        if (parser.isEnterEditor()) {
            this.enterEditor = true;
        }

        try {
            Object resp = ThriftClient.sendRequest(req);
            response = ((StdTextProductContainer) resp).getProductList();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        ArrayList<StdTextProduct> rvalList = null;

        if (response != null && !response.isEmpty()) {
            StdTextProduct rval = response.get(0);
            StringBuilder builder = new StringBuilder(rval.getProduct());
            StdTextProduct curProd = null;

            // the product will be all the products with a \n between, any other
            // field will be set if all of the returns have the same value for
            // that field
            for (int i = 1; i < response.size(); i++) {
                curProd = response.get(i);
                builder.append('\n');
                builder.append(curProd.getProduct());

                rval.setCccid(
                        getFieldValue(rval.getCccid(), curProd.getCccid()));
                rval.setNnnid(
                        getFieldValue(rval.getNnnid(), curProd.getNnnid()));
                rval.setXxxid(
                        getFieldValue(rval.getXxxid(), curProd.getXxxid()));
                rval.setBbbid(
                        getFieldValue(rval.getBbbid(), curProd.getBbbid()));
                rval.setHdrtime(
                        getFieldValue(rval.getHdrtime(), curProd.getHdrtime()));
                rval.setSite(getFieldValue(rval.getSite(), curProd.getSite()));
                rval.setWmoid(
                        getFieldValue(rval.getWmoid(), curProd.getWmoid()));
            }

            rval.setProduct(builder.toString());
            rvalList = new ArrayList<>(1);
            rvalList.add(rval);
        } else {
            rvalList = new ArrayList<>(0);
        }

        return rvalList;
    }

    private static String getFieldValue(String field1, String field2) {
        String rval = field1;

        if (field1 != null && !field1.isEmpty()) {
            if (!field1.equals(field2)) {
                rval = "";
            }
        }

        return rval;
    }

    public void launchSelectionDialog() {
        // no op
    }

    @Override
    public ICommand getNext() {
        int version = parser.getPastVersNumber();
        version--;
        String afosCommand;
        if (version > 0) {
            afosCommand = "-" + version + ":" + parser.getCcc()
                    + parser.getNnn() + parser.getXxx();
        } else {
            afosCommand = parser.getCcc() + parser.getNnn() + parser.getXxx();
        }

        String localSiteId = LocalizationManager.getInstance().getCurrentSite();
        ICommand command = new AFOSCommand(afosCommand, localSiteId, null);
        return command;
    }

    @Override
    public ICommand getPrevious() {
        int version = parser.getPastVersNumber();
        if (version > 0) {
            version++;
        } else {
            version = 1;
        }
        String localSiteId = LocalizationManager.getInstance().getCurrentSite();
        String afosCommand = "-" + version + ":" + parser.getCcc()
                + parser.getNnn() + parser.getXxx();
        return new AFOSCommand(afosCommand, localSiteId, null);
    }

    @Override
    public ICommand getAll() {
        String localSiteId = LocalizationManager.getInstance().getCurrentSite();
        String afosCommand = "ALL:" + parser.getCcc() + parser.getNnn()
                + parser.getXxx();
        return new AFOSCommand(afosCommand, localSiteId, null);
    }

    @Override
    public ICommand getLatest() {
        String localSiteId = LocalizationManager.getInstance().getCurrentSite();
        String afosCommand = parser.getCcc() + parser.getNnn()
                + parser.getXxx();
        return new AFOSCommand(afosCommand, localSiteId, null);
    }
}
