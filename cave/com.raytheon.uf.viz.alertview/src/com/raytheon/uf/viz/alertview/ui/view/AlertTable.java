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
package com.raytheon.uf.viz.alertview.ui.view;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.filter.AlertFilter;
import com.raytheon.uf.viz.alertview.style.StyleManager;

/**
 * 
 * Table that displays {@link Alert}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 18, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertTable extends Composite {

    public static final String COLUMN_TIME = "Time";

    public static final String COLUMN_PRIORITY = "Priority";

    public static final String COLUMN_ORIGIN = "Origin";

    public static final String COLUMN_MESSAGE = "Message";

    private Table alertTable;

    private AlertFilter filter;

    private BlockingQueue<Alert> alertsToAdd = new LinkedBlockingQueue<Alert>();

    private StyleManager styles = new StyleManager();

    /**
     * When things are going terribly wrong and a gazillion alerts are coming in
     * the UI will freeze if they aren't throttled a bit. This job tries to be
     * reasonablish about how much CPU to suck updating the table.
     */
    private Job addAlertJob = new UIJob("Updating AlertView") {

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            Alert next = alertsToAdd.poll();
            if (next != null) {
                int processed = 0;
                while (next != null) {
                    addAlertInternal(next);
                    if (processed++ > 500) {
                        this.schedule(100);
                        break;
                    }
                    next = alertsToAdd.poll();
                }
                packColumns();
            }
            return Status.OK_STATUS;
        }
    };

    public AlertTable(Composite parent) {
        super(parent, SWT.NONE);
        this.setLayout(new FillLayout());
        createAlertTable();
    }

    protected void createAlertTable() {
        alertTable = new Table(this, SWT.H_SCROLL | SWT.V_SCROLL
                | SWT.FULL_SELECTION | SWT.MULTI);
        alertTable.setHeaderVisible(true);
        alertTable.setLinesVisible(true);
        rebuildColums();
        alertTable.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                alertSelected();
            }

        });
        alertTable.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseDoubleClick(MouseEvent e) {
                alertDoubleClick();
            }

        });

        alertTable.addMouseMoveListener(new MouseMoveListener() {

            @Override
            public void mouseMove(MouseEvent e) {
                alertTable.setToolTipText(null);
            }
        });

        alertTable.addMouseTrackListener(new MouseTrackAdapter() {

            @Override
            public void mouseHover(MouseEvent e) {
                TableItem item = alertTable.getItem(new Point(e.x, e.y));
                if (item == null) {
                    alertTable.setToolTipText(null);
                } else {
                    Alert alert = getAlert(item);
                    alertTable.setToolTipText(getToolTip(alert));
                }
            }
        });
        Menu menu = new Menu(alertTable);
        alertTable.setMenu(menu);
        menu.addMenuListener(new MenuAdapter() {

            @Override
            public void menuShown(MenuEvent e) {
                populateContextMenu(alertTable.getMenu());
            }

        });
    }

    public void addAlert(Alert alert) {
        alertsToAdd.add(alert);
        addAlertJob.schedule();
    }

    protected TableItem addAlertInternal(Alert alert) {
        if (filter != null && !filter.filter(alert)) {
            return null;
        }
        TableItem item = new TableItem(alertTable, SWT.NONE);
        item.setData(new ArrayList<Alert>(Arrays.asList(alert)));
        item.setText(getText(alert));
        applyStyle(item);
        alertTable.showItem(item);
        return item;
    }

    protected void applyStyle(TableItem item) {
        Alert alert = getAlert(item);
        item.setBackground(styles.getBackgroundColor(item.getDisplay(), alert));
        item.setForeground(styles.getForegroundColor(item.getDisplay(), alert));
        item.setFont(styles.getFont(item.getDisplay(), alert));
    }

    public void setFilter(AlertFilter filter) {
        this.filter = filter;
        for (TableItem item : alertTable.getSelection()) {
            Alert alert = getAlert(item);
            if (!filter.filter(alert)) {
                item.dispose();
            }
        }
        for (TableItem item : alertTable.getItems()) {
            Alert alert = getAlert(item);
            if (!filter.filter(alert)) {
                item.dispose();
            }
        }
    }

    public void refresh(Alert alert) {
        for (TableItem item : alertTable.getItems()) {
            List<Alert> alerts = getAlerts(item);
            if (alerts.contains(alert)) {
                boolean selected = false;
                alerts.remove(alert);
                if (alerts.isEmpty()) {
                    selected = alertTable.isSelected(alertTable.indexOf(item));
                    item.dispose();
                }
                TableItem newItem = addAlertInternal(alert);
                if (selected && newItem != null) {
                    alertTable.select(alertTable.indexOf(newItem));
                }
            }

        }
    }

    public void select(Alert alert) {
        if (alertsToAdd.contains(alert)) {
            TableItem item = addAlertInternal(alert);
            alertTable.setSelection(alertTable.indexOf(item));
            alertSelected();
            return;
        }
        for (TableItem item : alertTable.getItems()) {
            List<Alert> alerts = getAlerts(item);
            if (alerts.contains(alert)) {
                alertTable.setSelection(alertTable.indexOf(item));
                alertSelected();
                return;
            }
        }
    }

    public void packColumns() {
        for (TableColumn column : alertTable.getColumns()) {
            column.pack();
        }
    }

    protected String[] getText(Alert alert) {
        TableColumn[] columns = alertTable.getColumns();
        String[] text = new String[columns.length];
        for (int i = 0; i < columns.length; i += 1) {
            if (columns[i].getText().equals(COLUMN_MESSAGE)) {
                text[i] = alert.getMessage().split("\n")[0];
            } else if (columns[i].getText().equals(COLUMN_TIME)) {
                text[i] = new SimpleDateFormat("hh:mm a").format(alert
                        .getTime());
            } else if (columns[i].getText().equals(COLUMN_PRIORITY)) {
                text[i] = alert.getPriority().toString();
            } else if (columns[i].getText().equals(COLUMN_ORIGIN)) {
                text[i] = alert.getOrigin();
            } else {
                text[i] = "Unknown Field";
            }
        }
        return text;
    }

    // public void setFilter(StatusMessageFilter filter) {
    // this.filter = filter;
    // for (TableItem item : alertTable.getSelection()) {
    // StatusMessage message = getStatusMessage(item);
    // if (!filter.filter(message)) {
    // item.dispose();
    // }
    // }
    // for (TableItem item : alertTable.getItems()) {
    // StatusMessage message = getStatusMessage(item);
    // if (!filter.filter(message)) {
    // item.dispose();
    // }
    // }
    // }

    public void rebuildColums() {
        for (TableColumn column : alertTable.getColumns()) {
            column.dispose();
        }
        // TODO columns from config.
        for (String columnText : new String[] { COLUMN_TIME, COLUMN_PRIORITY,
                COLUMN_MESSAGE }) {
            TableColumn column = new TableColumn(alertTable, SWT.NONE);
            column.setText(columnText);
        }
        for (TableItem item : alertTable.getItems()) {
            item.setText(getText(getAlert(item)));
        }
        for (TableColumn column : alertTable.getColumns()) {
            column.pack();
        }
    }

    protected String getToolTip(Alert alert) {
        StringBuilder toolTip = new StringBuilder();
        if (alert.getTime() != null) {
            SimpleDateFormat sdf = new SimpleDateFormat(
                    "yyyy-MM-dd_HH:mm:ss.SSS");
            toolTip.append("Time: ").append(sdf.format(alert.getTime()))
                    .append("\n");
        }
        if (alert.getPriority() != null) {
            toolTip.append("Priority: ").append(alert.getPriority())
                    .append("\n");
        }
        if (toolTip.charAt(toolTip.length() - 1) == '\n') {
            toolTip.deleteCharAt(toolTip.length() - 1);
        }
        return toolTip.toString();
    }

    protected void populateContextMenu(Menu menu) {
        // TODO need standard way of filling menu with items.
        // for (MenuItem item : menu.getItems()) {
        // item.dispose();
        // }
        // List<StatusMessage> messages = getMultiSelection();
        // if (!messages.isEmpty()) {
        // manager.getMenuManager().populateContextMenu(menu, messages);
        // }
        // AcknowledgedFilter ackFilter = new AcknowledgedFilter();
        // boolean ackAll = false;
        // for (TableItem item : alertTable.getItems()) {
        // StatusMessage m = getStatusMessage(item);
        // if (ackFilter.filter(m) == false) {
        // ackAll = true;
        // break;
        // }
        // }
        // if (ackAll) {
        // new MenuItem(menu, SWT.SEPARATOR);
        // MenuItem item = new MenuItem(menu, SWT.NONE);
        // item.setText("Acknowledge All");
        // item.addSelectionListener(new SelectionAdapter() {
        //
        // @Override
        // public void widgetSelected(SelectionEvent e) {
        // AcknowledgedFilter filter = new AcknowledgedFilter();
        // AcknowledgeAction action = new AcknowledgeAction();
        // for (TableItem item : alertTable.getItems()) {
        // for (StatusMessage m : getStatusMessages(item)) {
        // if (filter.filter(m) == false) {
        // action.handle(m);
        // }
        // }
        // }
        // }
        //
        // });
        // }
    }

    public Alert getSingleSelection() {
        TableItem[] selection = alertTable.getSelection();
        if (selection == null || selection.length != 1) {
            return null;
        } else {
            return getAlert(selection[0]);
        }
    }

    public List<Alert> getMultiSelection() {
        TableItem[] selection = alertTable.getSelection();
        if (selection == null || selection.length == 0) {
            return Collections.emptyList();
        }
        List<Alert> result = new ArrayList<Alert>();
        for (int i = 0; i < selection.length; i += 1) {
            result.addAll(getAlerts(selection[i]));
        }
        return result;
    }

    public Alert getAlert(TableItem item) {
        return getAlerts(item).get(0);
    }

    @SuppressWarnings("unchecked")
    public List<Alert> getAlerts(TableItem item) {
        return (List<Alert>) item.getData();
    }

    @Override
    public void dispose() {
        super.dispose();
        styles.close();
    }

    protected void alertSelected() {
        // sub classes can override this easily
    }

    protected void alertDoubleClick() {
        // sub classes can override this easily

    }

}
