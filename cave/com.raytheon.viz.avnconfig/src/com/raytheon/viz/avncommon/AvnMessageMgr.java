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
package com.raytheon.viz.avncommon;

import java.util.HashMap;
import java.util.LinkedList;

import com.raytheon.viz.avnconfig.MessageViewerDlg;

/**
 * AvnFPS message manager stores and routes messages to the proper message viewer dialog.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2009            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public final class AvnMessageMgr
{
    /**
     * Class instance.
     */
    private static AvnMessageMgr classInstance;
    
    /**
     * Maximum number of message to keep.
     */
    private final int MAX_MSG_LOG_SIZE = 500;
    
    /**
     * Enumeration of status message types.  This will determine the message viewer that
     * will receive the messages.
     */
    public static enum StatusMessageType
    {
        Metar, WindRose, CigVis, CigVisTrend, TafMonitor, TafMonMetar,
        TafMonWindRose, TafMonCigVis, TafMonCigVisTrend, TafViewerEditor,
        WeatherPlot, MonitoringCriteria, TafSiteInfo, TafProdConfig;
    }
    
    /**
     * Map that contains linked lists that hold the status messages.
     */
    private HashMap<StatusMessageType, LinkedList<String>> messageMap;
    
    /**
     * Map of registered message viewers.
     */
    private HashMap<StatusMessageType, MessageViewerDlg> regMsgDlgMap;
    
    /**
     * Private constructor.
     */
    private AvnMessageMgr()
    {
        initialize();
    }
    
    /**
     * Method to return an instance of this class.
     * @return An instance of this class.
     */
    public static synchronized AvnMessageMgr getInstance()
    {
        if (classInstance == null)
        {
            classInstance = new AvnMessageMgr();
        }
        
        return classInstance;
    }
    
    /**
     * Initialize method.
     */
    private void initialize()
    {
        messageMap = new HashMap<StatusMessageType, LinkedList<String>>();
        
        regMsgDlgMap = new HashMap<StatusMessageType, MessageViewerDlg>();
        
        /*
         * Create message lists in the message map.
         */        
        for (StatusMessageType mt: StatusMessageType.values())
        {
            messageMap.put(mt, new LinkedList<String>());
        }
    }
    
    /**
     * Get the maximum amount of messages that are kept in the message lists.
     * @return Maximum number of messages kept.
     */
    public int getMaxLogMessages()
    {
        return MAX_MSG_LOG_SIZE;
    }
    
    /**
     * Register the message viewer to start receiving messages.  The current list of messages
     * is returned when the message viewer is registered.
     * @param msgType Status message type.
     * @param msgDlg Message viewer dialog.
     * @return List of current messages queued up.
     */
    public LinkedList<String> registerMessageDlg(StatusMessageType msgType, MessageViewerDlg msgDlg)
    {
        regMsgDlgMap.put(msgType, msgDlg);
        
        return messageMap.get(msgType);
    }
    
    /**
     * Unregister the message viewer associated with the message type.
     * @param msgType Status message type.
     */
    public void unregisterMessageDlg(StatusMessageType msgType)
    {
        regMsgDlgMap.remove(msgType);
    }
    
    /**
     * Add a message to the message list associated with the specified message type.
     * If there are any message viewers registered then pass along the message.
     * @param msgType Status message type.
     * @param message The message to be added.
     */
    public void addMessage(StatusMessageType msgType, String message)
    {
        /*
         * Add the message to the list associated with the message type.
         */
        if (messageMap.containsKey(msgType) == true)
        {
            LinkedList<String> currentList = messageMap.get(msgType);

            if (currentList.size() >= MAX_MSG_LOG_SIZE)
            {
                currentList.removeLast();
            }

            currentList.addFirst(message);
        }
        
        /*
         * If there is a registered message viewer then send the
         * message viewer the message.
         */
        if (regMsgDlgMap.containsKey(msgType) == true)
        {
            MessageViewerDlg msgDlg = regMsgDlgMap.get(msgType);
            msgDlg.addMessageText(message);
        }
    }
}
