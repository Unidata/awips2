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


import java.util.Collection;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.textdb.dbapi.impl.TextDB;
import com.raytheon.uf.common.dataplugin.text.db.WatchWarn;
import com.raytheon.uf.common.dataplugin.text.dbsrv.ICommandExecutor;
import com.raytheon.uf.common.dataplugin.text.dbsrv.PropConverter;
import com.raytheon.uf.common.dataplugin.text.dbsrv.TextDBSrvCommandTags;
import com.raytheon.uf.common.dataplugin.text.dbsrv.WarnTableTags;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Processes warning textdbsrv command messages
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2008        1538 jkorman     Initial creation
 * Aug 9,2010   3944       cjeanbap    Added logic to delete all records
 *                                     from WatchWarn table.
 * Sep 14,2010  3944       cjenabap    Added sendTextToQueue()
 * May 15, 2014 2536       bclement    moved from uf.edex.textdbsrv
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class WarnTableAdapter implements ICommandExecutor {

    private TextDB textDB;
    
    private static final String WATCH_WARN_QUEUE = "ldadWatchWarnDirect";

    private Log logger = LogFactory.getLog(getClass());
    
    /**
     * 
     */
    public WarnTableAdapter() {
        textDB = new TextDB();
    }

    /**
     * 
     * @return
     */
    public static final String getViewTag() {
        return "warn";
    }

    /**
     * 
     */
    public void dispose() {

    }

    /**
     * 
     */
    @Override
    public Message execute(Message cmdMessage) {

        Header sHeader = cmdMessage.getHeader();

        // Get the operation code
        String op = PropConverter.getProperty(sHeader, WarnTableTags.OP.name());

        TextDBSrvCommandTags opTag = TextDBSrvCommandTags.valueOf(op);

        if (opTag != null) {
            switch (opTag) {

            case PUT: {
                String productId = PropConverter.getProperty(sHeader,
                        WarnTableTags.PRODID.name());
                String script = PropConverter.getProperty(sHeader,
                        WarnTableTags.SCRIPT.name());

                addWatchWarn(sHeader, productId, script);
                sendTextToQueue(productId, WATCH_WARN_QUEUE);
                break;
            }

            case GET: {
                String productId = PropConverter.getProperty(sHeader,
                        WarnTableTags.PRODID.name());
                if (productId != null) {
                    getWatchWarn(sHeader, productId);
                }
                break;
            }
            case DELETE: {
                String productId = PropConverter.getProperty(sHeader,
                        WarnTableTags.PRODID.name());
                String script = PropConverter.getProperty(sHeader,
                        WarnTableTags.SCRIPT.name());

                if ((productId != null) && (script != null)) {
                    deleteWatchWarn(sHeader, productId, script);
                } else {
                    Collection<WatchWarn> watchWarns = textDB.queryAllWatchWarn();
                    Property[] tempProps = new Property[watchWarns.size()];
                    int i = 0;
                    for (WatchWarn ww: watchWarns) {
                        deleteWatchWarn(sHeader, ww.getProductid(), ww.getScript());                                                 
                        tempProps[i++] = sHeader.getProperties()[0];
                    }
                    sHeader.setProperties(tempProps);
                }
                break;
            }
            default: {
                String tagName = (opTag != null) ? opTag.name() : "null";
                Property[] props = new Property[] { new Property("STDERR",
                        PropConverter.asciiToHex("ERROR:Invalid command tag = ["
                                + tagName + "]")), };
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
    private void addWatchWarn(Header header, String productId, String script) {
        Property newProperty = new Property("STDERR",
                PropConverter.asciiToHex("NORMAL:Adding productId " + productId
                        + " to trigger."));
        Property errProperty = new Property("STDERR",
                PropConverter
                        .asciiToHex("ERROR:Failure adding to state_ccc table."));

        Property[] props = new Property[] { newProperty, };
        if (!textDB.addWatchWarn(productId, script)) {
            props = new Property[] { newProperty, errProperty };
        }
        header.setProperties(props);
    }

    /**
     * 
     * @param header
     * @param productId
     */
    private void getWatchWarn(Header header, String productId) {
        String PROP_FMT = "STDOUT";

        Property[] props = null;

        List<WatchWarn> dataList = textDB.queryWatchWarn(productId);

        if (dataList.size() > 0) {
            props = new Property[dataList.size() + 2];
            int i = 0;
            props[i] = new Property(PROP_FMT,
                    PropConverter.asciiToHex("PRODUCTID SCRIPT"));
            props[i] = new Property(PROP_FMT,
                    PropConverter.asciiToHex("--------- ------"));
            for (WatchWarn w : dataList) {
                props[i++] = new Property(PROP_FMT,
                        PropConverter.asciiToHex(String.format("%9s %s",
                                w
                        .getProductid(), w.getScript())));
            }
        } else {
            props = new Property[] { new Property("STDERR",
                    PropConverter
                            .asciiToHex("ERROR:Failure reading from watch warn table.")), };
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
    private void deleteWatchWarn(Header header, String productId, String script) {
        Property newProperty = new Property("STDERR",
                PropConverter.asciiToHex("NORMAL:Deleting product id "
                        + productId + " trigger."));
        Property errProperty = new Property("STDERR",
                PropConverter
                        .asciiToHex("ERROR:Failure adding to state_ccc table."));

        Property[] props = new Property[] { newProperty, };
        if (!textDB.deleteWatchWarn(productId, script)) {
            props = new Property[] { newProperty, errProperty };
        }
        header.setProperties(props);
    }
    
    /**
     * Sends an asynchronous message to the specified queue.
     * 
     * @param message
     *            the message to send
     * @param queue
     *            the queue to receive the message
     */
    private void sendTextToQueue(String message, String queue) {
        try {
            EDEXUtil.getMessageProducer().sendAsync(queue, message);
        } catch (EdexException e) {
            logger.warn("Unable to send product '" + message + "' to queue '"
                    + queue + "'", e);
        }
    }
}
