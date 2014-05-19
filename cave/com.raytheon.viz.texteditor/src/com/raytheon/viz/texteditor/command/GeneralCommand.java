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

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.dbsrv.IQueryTransport;
import com.raytheon.uf.common.dataplugin.text.dbsrv.TextDBQuery;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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
public class GeneralCommand implements ICommand {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(GeneralCommand.class);
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
     */
    private String lastHours = null;

    private String fullRead = null;

    /**
     * 
     * @param browserCallBack
     * @param wmoId
     * @param site
     * 
     */
    public GeneralCommand(String wmoId, String site, String awipsid,
            String hdrtime, String bbbid, String lastHours, String fullRead) {
        this.wmoId = wmoId;
        this.site = site;
        this.awipsid = awipsid;
        this.hdrtime = hdrtime;
        this.bbbid = bbbid;
        this.lastHours = lastHours;
        this.fullRead = fullRead;
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
        // verify at least 1 field is valid
        return true;
    }

    /**
     * 
     */
    @Override
    public String[] getCommandTextFields() {
        // no op
        return new String[0];
    }

    /**
     * 
     */
    @Override
    public List<StdTextProduct> executeCommand(IQueryTransport transport)
            throws CommandFailedException {
        // TODO verify at least 1 field not blank

        TextDBQuery dbQuery = new TextDBQuery(transport);
        dbQuery.setQueryViewName("text");
        dbQuery.setQueryOpName("GET");
        dbQuery.setQuerySubObName("JOINXML");
        Boolean result = new Boolean(CAVEMode.OPERATIONAL.equals(CAVEMode.getMode()) 
                || CAVEMode.TEST.equals(CAVEMode.getMode())? true : false);
        dbQuery.setQueryOperationalMode(result.toString().toUpperCase());

        if (awipsid != null && awipsid.length() > 0) {
            dbQuery.setQueryNnnXxx(awipsid);
        }

        if (wmoId != null && wmoId.length() > 0) {
            dbQuery.setQueryWmoId(wmoId);
        }

        if (site != null && site.length() > 0) {
            dbQuery.setQuerySite(site);
        }

        if (hdrtime != null && hdrtime.length() > 0) {
            dbQuery.setQueryHdrTime(hdrtime);
        }

        if (bbbid != null && bbbid.trim().length() > 0) {
            dbQuery.setQueryBBB(bbbid);
        }

        if (lastHours != null && lastHours.trim().length() > 0) {
            dbQuery.setQueryHour(lastHours);
        }

        if (fullRead != null && fullRead.trim().length() > 0) {
            dbQuery.setQueryFullDataRead(fullRead);
        }

        Message queryResponse = dbQuery.executeQuery();
        Property[] properties = queryResponse.getHeader().getProperties();
        List<StdTextProduct> response = new ArrayList<StdTextProduct>();

        // TODO Add Error Handling. STDERR??
        if (properties != null) {
            try {
                JAXBContext context = SerializationUtil.getJaxbContext();
                Unmarshaller unmarshaller = context.createUnmarshaller();
                for (Property p : properties) {
                    if ("STDOUT".equals(p.getName())) {
                        StringReader reader = new StringReader(p.getValue());
                        StdTextProduct prod = (StdTextProduct) unmarshaller
                                .unmarshal(reader);
                        response.add(prod);
                    }
                }
            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error occurred executing Command", e);
            }
        }

        return response;
    }
}
