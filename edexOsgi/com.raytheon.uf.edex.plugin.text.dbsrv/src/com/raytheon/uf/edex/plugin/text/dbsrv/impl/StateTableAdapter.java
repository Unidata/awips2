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
package com.raytheon.uf.edex.plugin.text.dbsrv.impl;

import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.StateMatch;
import com.raytheon.uf.common.dataplugin.text.dbsrv.ICommandExecutor;
import com.raytheon.uf.common.dataplugin.text.dbsrv.StateTableTags;
import com.raytheon.uf.common.dataplugin.text.dbsrv.TextDBSrvCommandTags;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.edex.plugin.text.db.TextDB;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2008  1538       jkorman     Initial creation
 * Aug 31, 2010 2103       cjeanbap    Check variable for null.
 * May 15, 2014 2536       bclement    moved from uf.edex.textdbsrv
 * Aug 22, 2014 2926       bclement    compatibility changes with new textdb service
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.dbsrv
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class StateTableAdapter implements ICommandExecutor {

    private TextDB textDB;

    /**
     * 
     */
    public StateTableAdapter() {
        textDB = new TextDB();
    }

    /**
     * 
     * @return
     */
    public static final String getViewTag() {
        return "state";
    }

    /**
     * Dispose of any local resources before discarding this instance.
     */
    @Override
    public void dispose() {

    }

    /**
     * 
     * @param cmdMessage
     * @return
     */
    public Message execute(Message cmdMessage) {

        Header sHeader = cmdMessage.getHeader();

        // Get the operation code
        String op = sHeader.getProperty(StateTableTags.OP.name());

        TextDBSrvCommandTags opTag = TextDBSrvCommandTags.valueOf(op);

        if (opTag != null) {
            switch (opTag) {

            case PUT: {
                String state = sHeader.getProperty(StateTableTags.STATE.name());
                String cccId = sHeader.getProperty(StateTableTags.CCC.name());
                String xxxId = sHeader.getProperty(StateTableTags.XXX.name());
                if ((state != null) && (cccId != null) && (xxxId != null)) {
                    addStateData(sHeader, state, xxxId, cccId);
                }
                break;
            }
            case GET: {
                String state = sHeader.getProperty(StateTableTags.STATE.name());
                if (state != null) {
                    getStateData(sHeader, state);
                }
                break;
            }
            case DELETE: {
                String state = sHeader.getProperty(StateTableTags.STATE.name());
                String cccId = sHeader.getProperty(StateTableTags.CCC.name());
                String xxxId = sHeader.getProperty(StateTableTags.XXX.name());
                if ((state != null) && (cccId != null) && (xxxId != null)) {
                    deleteStateData(sHeader, state, xxxId, cccId);
                }
                break;
            }
            default: {
                String tagName = (opTag != null) ? opTag.name() : "null";
                Property[] props = new Property[] { new Property(
                        CommandExecutor.STDERR, "ERROR:Invalid command tag = ["
                                + tagName + "]"), };
                sHeader.setProperties(props);
                break;
            }

            }

        }
        cmdMessage.setHeader(sHeader);
        return cmdMessage;
    }

    /**
     * 
     * @param state
     * @param xxxId
     * @param cccId
     * @return
     */
    private void addStateData(Header header, String state, String xxxId,
            String cccId) {
        Property newProperty = new Property(CommandExecutor.STDERR,
                "NORMAL:Adding a new state-ccc.");
        Property errProperty = new Property(CommandExecutor.STDERR,
                "ERROR:Failure adding to state_ccc table.");

        Property[] props = new Property[] { newProperty, };
        if (!textDB.addState(state, cccId, xxxId)) {
            props = new Property[] { newProperty, errProperty };
        }
        header.setProperties(props);
    }

    /**
     * 
     * @param state
     * @param xxxId
     * @param cccId
     * @return
     */
    private void getStateData(Header header, String state) {
        final int HEADER_LINES = 2;

        String PROP_FMT = CommandExecutor.STDOUT;

        Property[] props = null;

        List<StateMatch> dataList = textDB.queryState(state);

        if (dataList != null && dataList.size() > 0) {
            props = new Property[dataList.size() + HEADER_LINES];
            int i = 0;
            props[i++] = new Property(PROP_FMT, "XXX CCC");
            props[i++] = new Property(PROP_FMT, "-------");
            for (StateMatch s : dataList) {
                props[i++] = new Property(PROP_FMT, s.getPk().getXxx() + " "
                        + s.getPk().getCcc());
            }
        } else {
            props = new Property[] { new Property(CommandExecutor.STDERR,
                    "ERROR:Failure reading from state lookup table."), };
        }
        header.setProperties(props);
    }

    /**
     * 
     * @param state
     * @param xxxId
     * @param cccId
     * @return
     */
    private void deleteStateData(Header header, String state, String xxxId,
            String cccId) {

        Property[] props = null;

        if (textDB.removeState(state, xxxId, cccId)) {
            props = new Property[] { new Property(CommandExecutor.STDERR,
                    "NORMAL:Deleting state-ccc."), };
        } else {
            props = new Property[] { new Property(CommandExecutor.STDERR,
                    "ERROR:Failure deleting from state_ccc table."), };
        }
        header.setProperties(props);
    }

}
