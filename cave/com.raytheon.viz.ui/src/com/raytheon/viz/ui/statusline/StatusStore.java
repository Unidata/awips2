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
package com.raytheon.viz.ui.statusline;

import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListSet;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.statusline.StatusMessage.Importance;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 14, 2008				randerso	Initial creation
 * Sep 12, 2008             wdougherty  Added updateStatusTextI() method
 * Oct 22, 2012 1229        rferrel     Changes for non-blocking ViewMessagesDialog.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class StatusStore {
    public static interface IStatusListener {
        public abstract void update(StatusMessage message);
    }

    // TODO: Get message limit from gfeconfig
    private static int MESSAGE_LIMIT = 80;

    private static Map<String, StatusStore> storeMap;

    public static StatusStore getStatusStore(String id) {
        if (storeMap == null) {
            storeMap = new HashMap<String, StatusStore>();
        }

        StatusStore store = storeMap.get(id);
        if (store == null) {
            store = new StatusStore(id);
            storeMap.put(id, store);
        }
        return store;
    }

    public static void updateStatus(final String statusID,
            final String messageText, final Importance importance) {

        getStatusStore(statusID).addMessage(messageText, importance);

    }

    /**
     * Add a message to the status bar. This is the same as updateStatus, except
     * that it takes a string for importance rather than an instance of the
     * enum. This method was added so that Python code could set status messages
     * through Jep. It is difficult (impossible?) to access an inner enum like
     * Importance directly from Jep, so a routine that converts from something
     * Jep understands is needed.
     * 
     * @param statusID
     *            The status store ID to add the message to
     * @param messageText
     *            The message to add to the status bar
     * @param importanceString
     *            The string version of the importance enum
     */
    public static void updateStatusTextI(final String statusID,
            final String messageText, final String importanceString) {

        Importance importance = null;
        try {
            importance = Importance.valueOf(importanceString);
        } catch (IllegalArgumentException e) {
            importance = null;
        }
        getStatusStore(statusID).addMessage(messageText, importance);

    }

    private ConcurrentSkipListSet<IStatusListener> listenerList;

    private ViewMessagesDialog viewDialog;

    private Map<String, UrgentMessagesDialog> dialogDict;

    /**
     * Message Buffer of all messages for this session kept in reverse
     * chronological order
     * 
     */
    private LinkedList<StatusMessage> messageBuffer;

    private Map<Importance, MessageImportance> importanceDict;

    private Importance defaultImportance;

    private String label;

    public StatusStore(String label) {
        this.label = label;

        if (importanceDict == null) {
            setImportanceDict(MessageImportance.defaultImportanceDict());
        } else {
            setImportanceDict(importanceDict);
        }

        this.messageBuffer = new LinkedList<StatusMessage>();

        this.listenerList = new ConcurrentSkipListSet<IStatusListener>(
                new Comparator<IStatusListener>() {

                    @Override
                    public int compare(IStatusListener o1, IStatusListener o2) {
                        return o1.hashCode() - o2.hashCode();
                    }

                });

        // Dialogs for urgent/significant messages
        this.dialogDict = new HashMap<String, UrgentMessagesDialog>();
    }

    public void setImportanceDict(
            Map<Importance, MessageImportance> importanceDict) {
        this.importanceDict = importanceDict;

        this.defaultImportance = null;
        for (Importance key : this.importanceDict.keySet()) {
            this.defaultImportance = key;
            MessageImportance entry = this.importanceDict.get(key);
            if (entry.isDefaultImportance()) {
                this.defaultImportance = key;
                break;
            }
        }
    }

    public void addMsg(String text) {
        this.addMessage(text, null);
    }

    public void addMessage(String text, Importance importance) {
        // Gather information to create the StatusMessage

        // Strip any trailing whitespace and line feeds
        text = text.trim();

        // Default importance to default
        if (!this.importanceDict.keySet().contains(importance)) {
            importance = this.defaultImportance;
        }

        final StatusMessage message = new StatusMessage(text, importance);

        // Add the message to the Buffer and check messageLimit
        this.messageBuffer.addFirst(message);
        if (this.messageBuffer.size() > MESSAGE_LIMIT) {
            this.messageBuffer.removeLast();
        }

        // update the status bars and message displays
        Display.getDefault().syncExec(new Runnable() {

            @Override
            public void run() {
                for (IStatusListener listener : listenerList) {
                    listener.update(message);
                }

                // Update View Message display
                if (viewDialog != null && viewDialog.getShell() != null
                        && !viewDialog.isDisposed()) {
                    viewDialog.updateText();
                }
                // Update Urgent Messages display
                // If does not exist, create it
                // Otherwise, add message to it
                Importance importance = message.getImportance();
                String bannerName = importanceDict.get(importance)
                        .getBannerName();
                if (bannerName != null) {
                    UrgentMessagesDialog umd = dialogDict.get(bannerName);
                    if (umd == null) {
                        // Instantiate an UrgentMessageDialog for this banner
                        // name
                        Shell shell = null;
                        Display display = Display.getCurrent();
                        if (display == null) {
                            throw new RuntimeException(
                                    "No current display for status message.");
                        } else {
                            shell = display.getActiveShell();
                            if (shell == null) {
                                Shell[] shells = display.getShells();
                                if (shells != null && shells.length > 0) {
                                    shell = shells[0];
                                }
                            }
                        }
                        if (shell == null) {
                            throw new RuntimeException(
                                    "Unable to obtain a shell for status message.");
                        }
                        umd = new UrgentMessagesDialog(shell, bannerName,
                                importanceDict.get(importance)
                                        .getBannerFgColor(), importanceDict
                                        .get(importance).getBannerBgColor());
                        dialogDict.put(bannerName, umd);
                    }
                    umd.setBlockOnOpen(false);
                    umd.open();
                    umd.addMessage(message);
                }
            }

        });
    }

    public void openViewMessagesDialog() {
        if (viewDialog == null || viewDialog.getShell() == null
                || viewDialog.isDisposed()) {
            this.viewDialog = new ViewMessagesDialog(Display.getDefault()
                    .getActiveShell(), this.messageBuffer, this.importanceDict,
                    "View Messages");
            viewDialog.setBlockOnOpen(false);
            viewDialog.open();
        } else {
            viewDialog.bringToTop();
        }
    }

    public StatBar getStatusBar() {
        StatBar statBar = new StatBar(this);
        return statBar;
    }

    public LinkedList<StatusMessage> getMessageBuffer() {
        return messageBuffer;
    }

    public Map<Importance, MessageImportance> getImportanceDict() {
        return importanceDict;
    }

    public String getLabel() {
        return label;
    }

    public void addListener(IStatusListener listener) {
        listenerList.add(listener);
    }

    public void removeListener(IStatusListener listener) {
        listenerList.remove(listener);
    }
}
