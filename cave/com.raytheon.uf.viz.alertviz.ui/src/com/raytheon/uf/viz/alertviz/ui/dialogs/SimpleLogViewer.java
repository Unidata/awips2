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
import java.sql.Timestamp;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
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
import com.raytheon.uf.viz.alertviz.ILogUpdatedCallback;
import com.raytheon.uf.viz.alertviz.LogUtil;
import com.raytheon.uf.viz.alertviz.LogUtil.Order;
import com.raytheon.uf.viz.alertviz.SystemStatusHandler;

/**
 * Implements a basic log viewer capability
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2008 1433       chammack    Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class SimpleLogViewer extends Dialog {

    /**
     * 
     */
    private Shell shell;

    /**
     * 
     */
    private SimpleDetailsComp sdb;

    /**
     * 
     */
    private Button showLog;

    /**
     * 
     */
    private Table reference;

    /**
     * 
     */
    int[] range;

    /**
     * 
     * @param parent
     */
    public SimpleLogViewer(Shell parent) {
        super(parent, SWT.NONE);
    }

    /**
     * set focus to the dialog
     * 
     * @return false if not open or disposed, true otherwise
     */
    public boolean focus() {
        if (shell == null || shell.isDisposed()) {
            return false;
        } else {
            shell.forceFocus();
            return true;
        }
    }

    /**
     * Opens the dialog (makes visible).
     * 
     * @return Null
     */
    public Object open() {
        // Create a new shell object and set the text for the dialog.
        Shell parent = getParent();
        // shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE);
        shell = new Shell(parent.getDisplay(), SWT.DIALOG_TRIM | SWT.MIN
                | SWT.TITLE | SWT.RESIZE);
        shell.setText(getText());

        /* TODO: Max Code: */
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        mainLayout.verticalSpacing = 0;
        shell.setLayout(mainLayout);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        shell.setLayoutData(gd);

        // shell.setLayout(new GridLayout(1, false));

        // shell.setSize(800, 400);
        // shell.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
        // | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL
        // | GridData.VERTICAL_ALIGN_FILL));

        final Table table = new Table(shell, SWT.BORDER | SWT.VIRTUAL);
        reference = table;
        final TableColumn[] columns = new TableColumn[] {
                new TableColumn(table, SWT.NONE),
                new TableColumn(table, SWT.NONE),
                new TableColumn(table, SWT.NONE),
                new TableColumn(table, SWT.NONE),
                new TableColumn(table, SWT.NONE) };

        // table.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
        // | GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL
        // | GridData.VERTICAL_ALIGN_FILL));

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
        final int[] range;
        final int sz;
        try {
            range = SystemStatusHandler.getCurrentRange();
            this.range = range;
            sz = range[1] - range[0];
        } catch (AlertvizException e2) {
            Container
                    .logInternal(
                            Priority.ERROR,
                            "SimpleLogViewer: exception getting current range from SystemStatusHandler.",
                            e2);
            return null;
        }

        table.setSortColumn(columns[0]);
        table.setSortDirection(SWT.UP);

        final Color red = new Color(Display.getCurrent(), new RGB(255, 0, 0));
        final Color yellow = new Color(Display.getCurrent(), new RGB(255, 255,
                0));
        final Color orange = new Color(Display.getCurrent(), new RGB(255, 128,
                0));
        final Color black = new Color(Display.getCurrent(), new RGB(0, 0, 0));

        table.addMouseListener(new MouseListener() {

            @Override
            public void mouseDoubleClick(MouseEvent e) {

                // int idx = table.getSelectionIndex();
                //
                // try {
                // StatusMessage sm = StatusMessage.retrieveByPk(idx
                // + range[0]);
                // if ( sdb != null && !sdb.isOpened()) {
                // sdb.open(sm);
                // }else if ( sdb != null ) {
                // sdb.dispose();
                // } else {
                // sdb = new SimpleDetailsBox(shell);
                // sdb.setMagicOffset(40);
                // sdb.open(sm);
                // }
                // } catch (AlertvizException e1) {
                // e1.printStackTrace();
                // }
                showHideLog();
            }

            @Override
            public void mouseDown(MouseEvent e) {

            }

            @Override
            public void mouseUp(MouseEvent e) {

            }

        });

        table.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {

            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                int idx = table.getSelectionIndex();

                try {

                    StatusMessage sm = SystemStatusHandler.retrieveByPk(idx
                            + range[0]);
                    sdb.displayDetails(sm);
                } catch (Exception e1) {
                    Container
                            .logInternal(
                                    Priority.ERROR,
                                    "SimpleLogViewer: exception retrieving StatusMessage by key from SystemStatusHandler: "
                                            + (idx + range[0]), e1);
                }

            }

        });

        table.addListener(SWT.SetData, new Listener() {
            public void handleEvent(Event e) {
                TableItem item = (TableItem) e.item;
                int index = table.indexOf(item);
                try {
                    StatusMessage sm = SystemStatusHandler
                            .retrieveByPk(range[0] + index);
                    item.setText(0, "" + sm.getEventTime().toString());
                    item.setText(1, "" + sm.getPriority().ordinal());
                    item.setText(2, sm.getSourceKey());
                    item.setText(3, sm.getCategory());
                    item.setText(4, sm.getMessage());

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
                    Status s = new Status(Status.ERROR, Activator.PLUGIN_ID,
                            "Error fetching the data", e1);
                    ErrorDialog.openError(
                            Display.getCurrent().getActiveShell(),
                            "Error fetching data",
                            "Error fetching the log data", s);
                }

            }
        });

        table.setItemCount(sz);

        ILogUpdatedCallback cb = new ILogUpdatedCallback() {

            @Override
            public void logUpdated(int minPk, int maxPk) {
                if (table.isDisposed()) {
                    return;
                }

                int idx = table.getTopIndex();
                int curSz = maxPk - minPk + 1;
                boolean setToEnd = false;
                if (curSz - idx < 40) {
                    setToEnd = true;
                }

                int sz = maxPk - minPk + 1;
                table.setItemCount(sz);

                if (setToEnd) {
                    table.setTopIndex(idx + 1);
                }
            }

        };

        AlertvizJob.addLogUpdatedCallback(cb);

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
        button.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {

            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                FileDialog fd = new FileDialog(shell, SWT.SAVE);
                String fileName = fd.open();
                if (fileName != null) {
                    try {
                        LogUtil.saveLogToFile(new File(fileName),
                                new Timestamp(0), Order.AFTER);
                    } catch (AlertvizException e1) {
                        final Status s = new Status(Status.ERROR,
                                Activator.PLUGIN_ID, "Error saving log", e1);
                        ErrorDialog.openError(shell, "Error saving log",
                                "Error saving log", s);
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
        showLog.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {

            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                showHideLog();
            }

        });

        sdb = new SimpleDetailsComp(shell, SWT.NONE);

        shell.pack();
        shell.open();

        // Wait until the shell is disposed.
        Display display = parent.getDisplay();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        AlertvizJob.removeLogUpdatedCallback(cb);
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
        int idx = reference.getSelectionIndex();
        if (idx < 0) {
            return;
        }
        try {
            StatusMessage sm = SystemStatusHandler.retrieveByPk(idx + range[0]);
            if (sdb.isVisible() == false) {
                sdb.setVisible(true);
                sdb.displayDetails(sm);
                showLog.setText("Hide Log...");
            } else {
                sdb.setVisible(false);
                showLog.setText("Show Log...");
            }
        } catch (AlertvizException e1) {
            Container
                    .logInternal(
                            Priority.ERROR,
                            "SimpleLogViewer: exception retrieving StatusMessage by key from SystemStatusHandler: "
                                    + (idx + range[0]), e1);
        }
    }
}
