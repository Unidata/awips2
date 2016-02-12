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

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.Activator;
import com.raytheon.uf.viz.alertviz.AlertvizException;
import com.raytheon.uf.viz.alertviz.AlertvizJob;
import com.raytheon.uf.viz.alertviz.Container;
import com.raytheon.uf.viz.alertviz.IAlertArrivedCallback;
import com.raytheon.uf.viz.alertviz.LogUtil;
import com.raytheon.uf.viz.alertviz.LogUtil.Order;
import com.raytheon.uf.viz.alertviz.SystemStatusHandler;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;

/**
 * Implements a basic log viewer capability
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2008 1433       chammack    Initial creation
 * Jun 02, 2015 4473       njensen     Cleaned up warnings
 * Jul 01, 2015 4473       njensen     Fix update of table on alert arrival
 * Jun 29, 2015 4311       randerso    Reworking AlertViz dialogs to be resizable.
 * Jan 25, 2016 5054       randerso    Converted to stand alone window
 * Feb 11, 2016 5314       dgilling    Fix System Log functionality.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SimpleLogViewer implements IAlertArrivedCallback {

    private Display display;

    private Shell shell;

    private SimpleDetailsComp detailsComp;

    private Button showLog;

    private Table table;

    private Color yellow;

    private Color red;

    private Color orange;

    private Color black;

    boolean first;

    /**
     * 
     * @param display
     */
    public SimpleLogViewer(Display display) {
        first = true;

        this.display = display;

        // Create a new shell object and set the text for the dialog.
        shell = new Shell(display, SWT.DIALOG_TRIM | SWT.MIN | SWT.TITLE
                | SWT.RESIZE);
        shell.setText("System Log");

        initializeComponents();
    }

    /**
     * @return true if shell is null or disposed;
     */
    public boolean isDisposed() {
        return ((shell != null) && shell.isDisposed());
    }

    /**
     * Bring dialog to the top
     */
    public void bringToTop() {
        if ((shell != null) && !shell.isDisposed()) {
            shell.setVisible(true);
            shell.forceFocus();
            shell.forceActive();
        }
    }

    private void initializeComponents() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        mainLayout.verticalSpacing = 0;
        shell.setLayout(mainLayout);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        shell.setLayoutData(gd);

        table = new Table(shell, SWT.BORDER | SWT.VIRTUAL);
        final TableColumn[] columns = new TableColumn[] {
                new TableColumn(table, SWT.NONE),
                new TableColumn(table, SWT.NONE),
                new TableColumn(table, SWT.NONE),
                new TableColumn(table, SWT.NONE),
                new TableColumn(table, SWT.NONE) };

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 800;
        gd.heightHint = 400;
        table.setLayoutData(gd);

        table.setHeaderVisible(true);
        columns[0].setText("Time");
        columns[0].setWidth(200);
        columns[1].setText("Priority");
        columns[1].setWidth(60);
        columns[2].setText("Source");
        columns[2].setWidth(100);
        columns[3].setText("Category");
        columns[3].setWidth(100);
        columns[4].setText("Message");
        columns[4].setWidth(100);

        int sz = 0;
        try {
            sz = SystemStatusHandler.getMessageCount();
        } catch (AlertvizException e2) {
            Container
                    .logInternal(
                            Priority.ERROR,
                            "SimpleLogViewer: exception getting current range from SystemStatusHandler.",
                            e2);
        }

        table.setSortColumn(columns[0]);
        table.setSortDirection(SWT.UP);

        red = new Color(display, new RGB(255, 0, 0));
        yellow = new Color(Display.getCurrent(), new RGB(255, 255, 0));
        orange = new Color(display, new RGB(255, 128, 0));
        black = new Color(display, new RGB(0, 0, 0));

        table.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Integer pk = (Integer) e.item.getData();
                int idx = table.getSelectionIndex();

                StatusMessage sm = null;
                try {
                    /*
                     * This event is triggered during initialization before the
                     * first TableItem has been assigned a PK in its data field.
                     * So we fall back to item selection index just in case.
                     */
                    if (pk != null) {
                        sm = SystemStatusHandler.retrieveByPk(pk.intValue());
                    } else {
                        sm = SystemStatusHandler.retrieveByRowOffset(idx);
                    }
                } catch (Exception e1) {
                    Container
                            .logInternal(
                                    Priority.ERROR,
                                    "SimpleLogViewer: exception retrieving StatusMessage by key from SystemStatusHandler: "
                                            + (idx + 1), e1);
                }
                detailsComp.displayDetails(sm);
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                showHideLog();
            }
        });

        table.addListener(SWT.SetData, new Listener() {
            @Override
            public void handleEvent(Event e) {
                TableItem item = (TableItem) e.item;
                int index = table.indexOf(item);
                try {
                    StatusMessage sm = SystemStatusHandler
                            .retrieveByRowOffset(index);
                    item.setText(0, "" + sm.getEventTime().toString());
                    item.setText(1, "" + sm.getPriority().ordinal());
                    item.setText(2, sm.getSourceKey());
                    item.setText(3, sm.getCategory());
                    item.setText(4, sm.getMessage());
                    item.setData(Integer.valueOf(sm.getPk()));

                    if (sm.getPriority() == Priority.CRITICAL) {
                        item.setForeground(red);
                        item.setBackground(black);
                    } else if (sm.getPriority() == Priority.SIGNIFICANT) {
                        item.setForeground(orange);
                        item.setBackground(black);
                    } else if (sm.getPriority() == Priority.PROBLEM) {
                        item.setForeground(yellow);
                        item.setBackground(black);

                    }

                } catch (AlertvizException e1) {
                    Container
                            .logInternal(
                                    Priority.ERROR,
                                    "SimpleLogViewer: exception retrieving StatusMessage by row offset from SystemStatusHandler: "
                                            + index, e1);
                    errorDialogWithStackTrace(Display.getCurrent()
                            .getActiveShell(), "Error fetching data",
                            "Error fetching the log data", e1);
                }
            }
        });

        table.setItemCount(sz);

        Composite buttons = new Composite(shell, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(3, false));

        // Open the shell to display the dialog.
        Button button = new Button(buttons, SWT.NONE);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, false, false);
        gd.widthHint = 100;
        button.setText("Export Log...");
        button.setLayoutData(gd);
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                FileDialog fd = new FileDialog(shell, SWT.SAVE);
                String fileName = fd.open();
                if (fileName != null) {
                    try {
                        LogUtil.saveLogToFile(new File(fileName),
                                new Timestamp(0), Order.AFTER);
                    } catch (AlertvizException e1) {
                        Container.logInternal(Priority.ERROR,
                                "SimpleLogViewer: exception saving log file: "
                                        + fileName, e1);
                        errorDialogWithStackTrace(Display.getCurrent()
                                .getActiveShell(), "Error saving log",
                                "Error saving log", e1);
                    }
                }
            }

        });

        Button close = new Button(buttons, SWT.NONE);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, false, false);
        gd.widthHint = 100;
        close.setText("Close");
        close.setLayoutData(gd);
        close.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                widgetSelected(e);
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.close();
            }

        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        showLog = new Button(buttons, SWT.NONE);
        showLog.setText("Show Log...");
        showLog.setLayoutData(gd);
        showLog.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                showHideLog();
            }
        });

        detailsComp = new SimpleDetailsComp(shell, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        detailsComp.setLayoutData(gd);

    }

    @Override
    public void alertArrived(StatusMessage statusMessage,
            AlertMetadata alertMetadata, Category category,
            TrayConfiguration globalConfiguration) {

        if (!table.isDisposed()) {
            int count = table.getItemCount();
            table.setItemCount(count + 1);
        }
    }

    /**
     * Opens the dialog (makes visible).
     * 
     * @return null
     */
    public Object open() {
        Point minSize = shell.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        shell.setMinimumSize(minSize);

        Point size = minSize;
        shell.setSize(size);

        showHideLog();

        AlertvizJob.getInstance().addAlertArrivedCallback(this);

        shell.open();

        if (table.getItemCount() > 0) {
            table.showItem(table.getItem(table.getItemCount() - 1));
            table.select(table.getItemCount() - 1);
        }

        // Wait until the shell is disposed.
        Display display = shell.getDisplay();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        AlertvizJob.getInstance().removeAlertArrivedCallback(this);
        table.dispose();
        red.dispose();
        yellow.dispose();
        black.dispose();
        orange.dispose();

        return null;
    }

    /**
     * 
     */
    private void showHideLog() {
        int delta = 0;
        if (first || detailsComp.isVisible()) {
            showLog.setText("Show Log...");

            ((GridData) detailsComp.getLayoutData()).exclude = true;
            detailsComp.setVisible(false);
            delta -= detailsComp.getSize().y;
            first = false;
        } else {
            showLog.setText("Hide Log...");
            ((GridData) detailsComp.getLayoutData()).exclude = false;
            detailsComp.setVisible(true);
            delta += detailsComp.getSize().y;
        }

        Point minSize = shell.getMinimumSize();
        minSize.y += delta;
        shell.setMinimumSize(minSize);

        Point size = shell.getSize();
        size.y += delta;
        shell.setSize(size);
    }

    private static void errorDialogWithStackTrace(Shell parentShell,
            String dialogTitle, String msg, Throwable t) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        t.printStackTrace(pw);
        String trace = sw.toString();

        Collection<Status> childStatuses = new ArrayList<>();

        String lineSep = System.getProperty("line.separator");
        for (String line : trace.split(lineSep)) {
            childStatuses.add(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                    line));
        }

        MultiStatus ms = new MultiStatus(Activator.PLUGIN_ID, IStatus.ERROR,
                childStatuses.toArray(new Status[0]), t.getLocalizedMessage(),
                t);

        ErrorDialog.openError(parentShell, dialogTitle, msg, ms);
    }
}
