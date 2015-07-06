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
import org.eclipse.jface.action.ActionContributionItem;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.action.SaveToFileAction;
import com.raytheon.uf.viz.alertview.filter.AlertFilter;
import com.raytheon.uf.viz.alertview.style.StyleManager;
import com.raytheon.uf.viz.alertview.style.StyleManager.StyleListener;

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
 * Jun 23, 2015  4474     njensen   Added removeAll() and getFilter()
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertTable extends Composite implements StyleListener {

    public static final String COLUMN_TIME = "Time";

    public static final String COLUMN_PRIORITY = "Priority";

    public static final String COLUMN_ORIGIN = "Origin";

    public static final String COLUMN_MESSAGE = "Message";

    public static final String[] ALL_COLUMNS = { COLUMN_TIME, COLUMN_PRIORITY,
            COLUMN_ORIGIN, COLUMN_MESSAGE };

    private StyleManager styles = new StyleManager();

    private BlockingQueue<Alert> alertsToAdd = new LinkedBlockingQueue<Alert>();

    private Table alertTable;

    private AlertFilter filter;

    private int mergeRepeatInterval;

    /**
     * When things are going terribly wrong and a gazillion alerts are coming in
     * the UI will freeze if they aren't throttled a bit. This job tries to be
     * reasonablish about how much CPU to suck updating the table.
     */
    private Job addAlertJob = new UIJob("Updating AlertView") {

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            if (AlertTable.this.isDisposed()) {
                alertsToAdd.clear();
                return Status.OK_STATUS;
            }
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

    public AlertTable(Composite parent, List<String> columns) {
        super(parent, SWT.NONE);
        this.setLayout(new FillLayout());
        createAlertTable();
        rebuildColums(columns);
        styles.addListener(this);
    }

    protected void createAlertTable() {
        alertTable = new Table(this, SWT.H_SCROLL | SWT.V_SCROLL
                | SWT.FULL_SELECTION | SWT.MULTI);
        alertTable.setHeaderVisible(true);
        alertTable.setLinesVisible(true);
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
        TableItem newItem = null;
        for (int index = 0; index < alertTable.getItemCount(); index += 1) {
            TableItem item = alertTable.getItem(index);
            List<Alert> alerts = getAlerts(item);
            Alert sample = alerts.get(0);
            if (alerts.contains(alert)) {
                return item;
            } else if (equalsEnough(sample, alert)) {
                alerts.add(alert);
                return item;
            } else if (sample.getTime().before(alert.getTime())) {
                newItem = new TableItem(alertTable, SWT.NONE, index);
                break;
            }
        }
        if (newItem == null) {
            newItem = new TableItem(alertTable, SWT.NONE);
        }
        newItem.setData(new ArrayList<Alert>(Arrays.asList(alert)));
        newItem.setText(getText(alert));
        applyStyle(newItem);
        alertTable.showItem(newItem);
        return newItem;
    }

    /**
     * Used to determine if 2 messages are similar enough that they should not
     * be displayed as separate items.
     */
    protected boolean equalsEnough(Alert a1, Alert a2) {
        if (a1.getMessage() == null) {
            if (a2.getMessage() != null) {
                return false;
            }
        } else if (!a1.getMessage().equals(a2.getMessage())) {
            return false;
        }
        if (a1.getOrigin() == null) {
            if (a2.getOrigin() != null) {
                return false;
            }
        } else if (!a1.getOrigin().equals(a2.getOrigin())) {
            return false;
        }
        if (a1.getDetails() == null) {
            if (a2.getDetails() != null) {
                return false;
            }
        } else if (!a1.getDetails().equals(a2.getDetails())) {
            return false;
        }

        long timeDiff = Math.abs(a2.getTime().getTime()
                - a1.getTime().getTime());
        if (timeDiff > mergeRepeatInterval) {
            return false;
        } else {
            return true;
        }
    }

    public void setMergeRepeatInterval(int mergeRepeatInterval) {
        this.mergeRepeatInterval = mergeRepeatInterval;
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

    protected AlertFilter getFilter() {
        return this.filter;
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

    public void rebuildColums(List<String> columns) {
        for (TableColumn column : alertTable.getColumns()) {
            column.dispose();
        }
        for (String columnText : columns) {
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
        for (MenuItem item : menu.getItems()) {
            item.dispose();
        }
        Alert alert = getSingleSelection();
        if (alert != null) {
            new ActionContributionItem(new SaveToFileAction(alert)).fill(menu,
                    -1);
        }
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

    public void removeAll() {
        alertTable.removeAll();
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

    @Override
    public void updateStyle() {
        Display display = getDisplay();
        if (display.isDisposed()) {
            return;
        }
        display.asyncExec(new Runnable() {

            @Override
            public void run() {
                if (alertTable.isDisposed()) {
                    return;
                }
                for (TableItem item : alertTable.getItems()) {
                    applyStyle(item);
                }
            }

        });

    }

}
