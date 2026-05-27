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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.ui.util.MessageFormatter;

/**
 * This class displays the log dialog for the selected categories.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Oct 05, 2008           lvenable   Initial creation.
 * Apr 02, 2009           lvenable   TTR fixes.
 * Dec 01, 2010  5632     cjeanbap   Added sort messages based on Category and a
 *                                   new Category to sortable list.
 * Feb 02, 2011  6500     cjeanbap   Added additional space.
 * Mar 01, 2011  5632     cjeanbap   Added Category Sort functionality.
 * Feb 06, 2013  14501    Xiaochuan  Added getCategoriesFromConfig; object
 *                                   clearOptionCbo can't be changed when the
 *                                   logs coming and Clear actived.
 * Apr 03, 2018  6646     randerso   Fixed order of source/category. Changed
 *                                   time stamp to 24 hour time like the rest of
 *                                   AWIPS. Allow double click in log pane to
 *                                   show/hide details pane
 * Sep 27, 2018  7454     randerso   Use a common message format for text
 *                                   display and logs. Removed tabItem knowledge
 *                                   from this class.
 * Oct 01, 2018  7455     randerso   Moved handling of maxLogSize into
 *                                   TextMsgLog
 * Oct 15, 2018  7515     randerso   Removed unused disposeListener list, added
 *                                   clearListener list and
 *                                   getHighestPriority().
 * Dec 06, 2018  7513     randerso   Changes to support maxLogSize per category.
 *                                   Code cleanup.
 * Jun 24, 2019  7515     randerso   Fix getHighestPriority() which was broken
 *                                   by changes made for 7513.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class TextMsgLog {

    /**
     * Interface for notification when clearMessages() is called.
     */
    public static interface ClearListener {
        /**
         * Called when clearMessages is called
         */
        public void logCleared();
    }

    /**
     * Log list control.
     */
    private List logList;

    /**
     * List of messages currently displayed in the logList
     */
    private java.util.List<StatusMessage> messageList;

    private int maxLogSize;

    /**
     * Map of messages by category
     */
    private TreeMap<String, java.util.List<StatusMessage>> messageMap;

    /**
     * String array of category names.
     */
    private String[] categories;

    private MessageFormatter msgFormat;

    private String categoryFilter = "All";

    /**
     * index of associated TextMsgControlComp
     */
    private int index;

    private ListenerList<ClearListener> clearListeners;

    /**
     * Constructor.
     *
     * @param index
     *            index of the associated TextMsgControlComp
     *
     * @param categories
     *            String array of category names.
     * @param msgFormat
     *            message formatter
     * @param maxLogSize
     *            maximum length of log
     */
    public TextMsgLog(int index, String[] categories,
            MessageFormatter msgFormat, int maxLogSize) {
        this.index = index;
        this.categories = categories;
        this.msgFormat = msgFormat;
        this.maxLogSize = maxLogSize;
        this.messageList = new ArrayList<>();
        this.messageMap = new TreeMap<>();
        this.clearListeners = new ListenerList<>();
    }

    /**
     * @param listener
     */
    public void addClearListener(ClearListener listener) {
        clearListeners.add(listener);
    }

    /**
     * Get the full text of this log, displays all categories.
     *
     * @return The text.
     */
    public String getFullText() {
        return String.join(" ", categories);
    }

    /**
     * Populate the log list control with messages.
     */
    private void populateLogList() {
        if (logList != null && !logList.isDisposed()) {
            int selection = logList.getSelectionIndex();

            logList.removeAll();
            messageList.clear();

            if ("All".equals(categoryFilter)) {
                for (java.util.List<StatusMessage> catMessages : messageMap
                        .values()) {
                    messageList.addAll(catMessages);
                }
                Collections.sort(messageList,
                        Comparator.comparing(StatusMessage::getEventTime));
            } else {
                java.util.List<StatusMessage> catMessageList = messageMap
                        .get(categoryFilter);

                if (catMessageList != null) {
                    messageList.addAll(catMessageList);
                }
            }

            for (StatusMessage sm : messageList) {
                logList.add(msgFormat.getFormattedMessage(sm));
            }

            if (selection >= 0) {
                logList.setSelection(selection);
            }
        }
    }

    /**
     * Add a message.
     *
     * @param sm
     *            Status message.
     */
    public void addMessage(StatusMessage sm) {
        java.util.List<StatusMessage> catList = messageMap
                .get(sm.getCategory());
        if (catList == null) {
            catList = new ArrayList<>();
            messageMap.put(sm.getCategory(), catList);
            TabControlDlg dlg = TabControlDlg.getInstance(null);
            if (dlg != null) {
                dlg.updateCatCombo(this);
            }
        }
        catList.add(sm);

        enforceMaxLogSize(catList);
        populateLogList();
    }

    /**
     * @param maxLogSize
     */
    public void setMaxLogSize(int maxLogSize) {
        this.maxLogSize = maxLogSize;

        for (java.util.List<StatusMessage> catList : messageMap.values()) {
            enforceMaxLogSize(catList);
        }
    }

    private void enforceMaxLogSize(java.util.List<StatusMessage> catList) {
        while (catList.size() > maxLogSize) {
            catList.remove(0);
        }
        populateLogList();
    }

    /**
     * Get the details text for the currently selected log message
     *
     * @return The log text.
     */
    public String getDetails() {
        String logText = "";
        if (logList != null && !logList.isDisposed()) {
            int idx = logList.getSelectionIndex();
            if (idx > -1 && idx < messageList.size()) {
                StatusMessage sm = messageList.get(idx);
                logText = sm.getDetails();
            }
        }
        return logText;
    }

    /**
     * Clear the selected messages from the display.
     *
     */
    public void clearMessages() {
        if ("All".equals(categoryFilter)) {
            messageMap.clear();
        } else {
            messageMap.remove(categoryFilter);
        }
        categoryFilter = "All";

        TabControlDlg dlg = TabControlDlg.getInstance(null);
        if (dlg != null) {
            dlg.updateCatCombo(this);
        }

        populateLogList();

        for (ClearListener listener : clearListeners) {
            listener.logCleared();
        }
    }

    /**
     * Create controls on the parent composite
     *
     * @param parent
     * @return the composite containing the controls
     *
     */
    public Control createControls(Composite parent) {
        Composite tabComp = new Composite(parent, SWT.NONE);
        tabComp.setLayout(new GridLayout(1, false));
        tabComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label catLbl = new Label(tabComp, SWT.NONE);
        GC gc = new GC(catLbl);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();
        gd.widthHint = charWidth * 100;
        catLbl.setLayoutData(gd);
        String fullText = getFullText();
        catLbl.setText("Categories: " + fullText);
        catLbl.setToolTipText(fullText);

        // Create the log list control.
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        logList = new List(tabComp, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        gd.heightHint = logList.getItemHeight() * 10
                - logList.getHorizontalBar().getSize().y;
        logList.setLayoutData(gd);

        logList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                TabControlDlg.getInstance(null).toggleDetails();
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                int idx = logList.getSelectionIndex();
                if (idx < 0) {
                    return;
                }
                StatusMessage sm = messageList.get(idx);
                TabControlDlg.getInstance(null).updateDetails(sm.getDetails());
            }

        });

        populateLogList();
        logList.setSelection(0);

        TabControlDlg.getInstance(null).updateDetails(getDetails());

        return tabComp;
    }

    /**
     * Get the tab index of the log.
     *
     * @return Tab index.
     */
    public int getIndex() {
        return index;
    }

    public String[] getCategoriesFromConfig() {
        return categories;
    }

    public Set<String> getCurrentCategories() {
        return messageMap.keySet();
    }

    public void setCategoryFilter(String category) {
        this.categoryFilter = category;
        populateLogList();
    }

    public String getCategoryFilter() {
        return this.categoryFilter;
    }

    /**
     * @return the highest priority of the messages in the message list
     */
    public Priority getHighestPriority() {
        // NOTE: highestPriority is lowest ordinal in the Priority enum
        Priority highestPriority = null;

        for (java.util.List<StatusMessage> msgList : messageMap.values()) {
            for (StatusMessage sm : msgList) {
                if (highestPriority == null) {
                    highestPriority = sm.getPriority();
                } else if (sm.getPriority().compareTo(highestPriority) < 0) {
                    highestPriority = sm.getPriority();
                }
            }
        }
        return highestPriority;
    }
}
