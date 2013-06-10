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

import java.text.SimpleDateFormat;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.Container;

/**
 * This class displays the log dialog for the selected categories.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 02 Apr 2009             lvenable    TTR fixes.
 * 01 Dec 2010  5632       cjeanbap    Added sort messages based on Category 
 *                                     and a new Category to sortable list.
 * 02 Feb 2011  6500       cjeanbap    Added additional space.  
 * 01 Mar 2011  5632       cjeanbap    Added Category Sort functionality.
 * 06 Feb 2013	14501	   Xiaochuan   Added getCategoriesFromConfig; object
 * 									   clearOptionCbo can't be changed when
 * 									   the logs coming and Clear actived.		
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TextMsgLog {

    /**
     * Tab item for folder
     */
    private TabItem tabItem;

    /**
     * Log list control.
     */
    private List logList;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Clear options combo box.
     */
    private Combo clearOptionCbo;

    /**
     * Vector of messages.
     */
    private Vector<StatusMessage> messageVec;

    /**
     * Map of Category names.
     */
    private TreeMap<String, Object> catNames;

    /**
     * String array of category names.
     */
    private String[] categories;

    /**
     * Time format.
     */
    private SimpleDateFormat timeFormat = new SimpleDateFormat("hh:mm a");

    /**
     * Index of Tab in tabfolder
     */
    private int tabIndex;

    private int clearOptionCboSelectedIndex;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent TabFolder tabs will live in.
     * @param categories
     *            String array of category names.
     * @param controlXCoord
     *            X coordinate of the control displaying the log dialog.
     * @param messageVec
     *            Vector of messages.
     */
    public TextMsgLog(Composite parent, String[] categories, int controlXCoord,
            Vector<StatusMessage> messageVec) {
        this.messageVec = messageVec;
        this.categories = categories;

        getCategoriesFromMessages();
        clearOptionCboSelectedIndex = 0;
    }

    /**
     * Get the full text of this log, displays all categories.
     * 
     * @return The text.
     */
    public String getFullText() {
        StringBuilder tmp = new StringBuilder("");

        for (int i = 0; i < categories.length; i++) {
            tmp.append(" ").append(categories[i]);
        }

        return tmp.toString();
    }

    /**
     * Populate the clear combo box.
     */
    public void populateClearOptionsCombo() {
        if (clearOptionCbo != null) {
            clearOptionCbo.removeAll();

            clearOptionCbo.add("All");

            Set<String> keySet = catNames.keySet();

            clearOptionCbo.select(0);
        }
    }

    /**
     * Gets the options for combo box of different types of messages.
     * 
     * @return Set that is options for combo box.
     */
    public Set<String> getCatKeySet() {
        return catNames.keySet();
    }

    /**
     * Populate the log list control with messages.
     */
    public void populateLogList() {
        if (logList != null && logList.isDisposed() == false) {
            logList.removeAll();
            for (StatusMessage statMsg : messageVec) {
                logList.add(getFormattedMessage(statMsg));
            }

            if (logList.getItemCount() != 0) {
                logList.select(logList.getItemCount() - 1);
                logList.showSelection();
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
        messageVec.add(sm);
        if (logList != null && logList.isDisposed() == false) {
            logList.add(getFormattedMessage(sm));
            if (logList.getSelectionCount() == 0) {
                logList.select(logList.getItemCount() - 1);
            }
        }
    }

    /**
     * Remove the first message.
     */
    public void removeFirst() {
        if (logList != null && logList.isDisposed() == false) {
            logList.remove(0);
        }
    }

    /**
     * Get the log text.
     * 
     * @return The log text.
     */
    public String getLogText() {
        if (logList != null && logList.isDisposed() == false) {
            int idx = logList.getSelectionIndex();
            if (idx > -1 && idx < messageVec.size()) {
                StatusMessage sm = messageVec.get(idx);
                return sm.getDetails();
            }
        }
        return "";
    }

    /**
     * Get the category names from the messages.
     */
    private void getCategoriesFromMessages() {
        catNames = new TreeMap<String, Object>();

        for (StatusMessage msg : messageVec) {
            catNames.put(msg.getCategory(), null);
        }
    }

    /**
     * Get the formatted message for the display.
     * 
     * @param sm
     *            Status message.
     * @return The formatted message.
     */
    private String getFormattedMessage(StatusMessage sm) {
        StringBuilder strBld = new StringBuilder();
        String localTZ = System.getenv("FXA_LOCAL_TZ");
        if (localTZ == null) {
            localTZ = "GMT";
        }
        timeFormat.setTimeZone(TimeZone.getTimeZone(localTZ));

        strBld.append(timeFormat.format(sm.getEventTime())).append(" ");
        strBld.append("(").append(sm.getPriority().ordinal()).append(") | ");
        strBld.append(sm.getCategory()).append(" | ");
        strBld.append(sm.getSourceKey()).append(" : ");
        strBld.append(sm.getMessage());

        return strBld.toString();
    }

    /**
     * Clear the selected messages from the display.
     */
    public void clearMessages() {
        synchronized (messageVec) {
            String item = "All";
            if (clearOptionCbo != null) {
                item = clearOptionCbo.getItem(clearOptionCbo
                        .getSelectionIndex());
            }

            for (int i = messageVec.size() - 1; i >= 0; i--) {
                if (item.equals("All")
                        || item.compareTo(messageVec.get(i).getCategory()) == 0) {
                    messageVec.remove(i);
                }
            }
        }

        getCategoriesFromMessages();
        populateLogList();
    }

    /**
     * Dispose of the Tab.
     */
    public void disposeDialog() {
        if (tabItem != null && tabItem.isDisposed() == false) {
            tabItem.dispose();
        }
    }

    /**
     * If Tab is already created returns tab, otherwise returns newly created
     * tab.
     * 
     * @return TabItem to display.
     */
    public TabItem getTab(TabFolder parent) {
        TabItem rval = null;
        if (tabItem != null && !tabItem.isDisposed()) {
            rval = tabItem;
        } else {
            try {
                int index = TabControlDlg.getInstance(null).getTabIndex(
                        tabIndex);
                tabItem = new TabItem(parent, SWT.DEFAULT, index);
                // TODO: investigate why tool tips do not work for tab items
                tabItem.setToolTipText(getFullText());
            } catch (java.lang.IllegalArgumentException e) {
                Container
                        .logInternal(
                                Priority.ERROR,
                                "TextMsgLog: exception setting ToolTips on TabItem.",
                                e);
            }

            Composite tabComp = new Composite(parent, SWT.NONE);
            tabComp.setLayout(new GridLayout(1, false));
            tabComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                    false));

            StringBuilder sb = new StringBuilder("Categories: ");

            for (int i = 0; i < categories.length; i++) {
                if (i != 0) {
                    sb.append(", ");
                }
                sb.append(categories[i]);
            }

            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            Label catLbl = new Label(tabComp, SWT.NONE);
            catLbl.setLayoutData(gd);
            catLbl.setText(sb.toString());

            // Create the log list control.
            gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            logList = new List(tabComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                    | SWT.H_SCROLL);
            logList.setFont(controlFont);
            logList.setLayoutData(gd);

            logList.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    int idx = logList.getSelectionIndex();
                    if (idx < 0) {
                        return;
                    }
                    StatusMessage sm = messageVec.get(idx);
                    TabControlDlg.getInstance(null).updateDetails(
                            sm.getDetails());
                }

            });

            tabItem.setControl(tabComp);
            populateLogList();

            int index = logList.getSelectionIndex();
            if (index >= 0) {
                TabControlDlg.getInstance(null).updateDetails(
                        this.messageVec.get(index).getDetails());
            }

            rval = tabItem;
        }
        parent.setSelection(rval);
        return rval;
    }

    /**
     * Updates details box to display currently selected log entry.
     */
    public void updateMessage() {
        int index = logList.getSelectionIndex();
        if (index < 0) {
            return;
        }
        TabControlDlg.getInstance(null).updateDetails(
                this.messageVec.get(index).getDetails());
    }

    /**
     * Set the clear option combo box, used when clearing logs.
     * 
     * @param clearOptionCbo
     *            Clear option combo control.
     */
    public void setClearOptionCbo(Combo clearOptionCbo) {
        this.clearOptionCbo = clearOptionCbo;
    }

    /**
     * Gets the details of the currently selected log item.
     * 
     * @return The details.
     */
    public String getText() {
        if (logList == null || logList.isDisposed()) {
            return "";
        }
        int index = logList.getSelectionIndex();
        if (index < 0) {
            return "";
        }

        return messageVec.get(index).getDetails();
    }

    /**
     * Sets the tab index of this log.
     * 
     * @param index
     *            Tab index.
     */
    public void setIndex(int index) {
        tabIndex = index;
    }

    /**
     * Get the tab index of the log.
     * 
     * @return Tab index.
     */
    public int getIndex() {
        return tabIndex;
    }

    public String[] getCategoriesFromConfig()
    {
    	return categories;
    }
    
    public void displayCategoryMessages(String category) {
        logList.removeAll();

        for (StatusMessage sm : messageVec) {
            if (category.equalsIgnoreCase("all")
                    || sm.getCategory().equals(category)) {
                logList.add(getFormattedMessage(sm));
            }
        }
        logList.redraw();
    }

    /**
     * Populate the clear combo box.
     */
    public void updateClearOptionsCombo(String category) {
        if (clearOptionCbo != null && !clearOptionCbo.isDisposed()) {
            if (!catNames.containsKey(category)) {
				catNames.put(category, null);
			}
        }
    }

    /**
     * Get the index of current selected value of the combo box.
     * 
     * @return
     */
    public int getClearOptionCboSelectedIndex() {
        return this.clearOptionCboSelectedIndex;
    }

    /**
     * Set the index of the current selected value of thecombo box.
     * 
     * @param clearOptionCboSelectedIndex
     */
    public void setClearOptionCboSelectedIndex(int clearOptionCboSelectedIndex) {
        this.clearOptionCboSelectedIndex = clearOptionCboSelectedIndex;
    }
}
