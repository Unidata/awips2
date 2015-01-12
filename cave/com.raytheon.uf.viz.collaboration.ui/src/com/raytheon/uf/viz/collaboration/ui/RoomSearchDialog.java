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
package com.raytheon.uf.viz.collaboration.ui;

import java.util.Collection;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.layout.TableColumnLayout;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;
import com.raytheon.uf.viz.collaboration.ui.actions.AddBookmarkAction;
import com.raytheon.uf.viz.collaboration.ui.actions.JoinRoomAction;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog for finding public chat rooms on server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2014  3705      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class RoomSearchDialog extends CaveSWTDialog {

    private Table resultTable;

    /**
     * @param parentShell
     */
    public RoomSearchDialog(Shell parentShell) {
        super(parentShell);
        setText("Room Search");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        initializeResultsTable(shell);
        initializeButtonBar(shell);
    }

    private void initializeResultsTable(Shell shell) {
        Composite tableComp = new Composite(shell, SWT.NONE);
        GridData gridData = new GridData(500, 200);
        gridData.horizontalAlignment = SWT.FILL;
        gridData.verticalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        tableComp.setLayoutData(gridData);
        resultTable = new Table(tableComp, SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.MULTI | SWT.FULL_SELECTION);
        resultTable.setHeaderVisible(true);
        resultTable.setLinesVisible(true);
        TableColumn nameColumn = new TableColumn(resultTable, SWT.LEFT);
        nameColumn.setText("Room Name");
        TableColumn idColumn = new TableColumn(resultTable, SWT.LEFT);
        idColumn.setText("Service");

        TableColumnLayout tcl = new TableColumnLayout();
        tableComp.setLayout(tcl);
        tcl.setColumnData(nameColumn, new ColumnWeightData(40));
        tcl.setColumnData(idColumn, new ColumnWeightData(60));

        MenuManager menuMgr = new MenuManager();
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {

            @Override
            public void menuAboutToShow(IMenuManager manager) {
                fillContextMenu(manager);
            }

        });
        Menu menu = menuMgr.createContextMenu(resultTable);
        resultTable.setMenu(menu);
        search();
    }

    private void initializeButtonBar(Shell shell) {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayoutData(new GridData(SWT.RIGHT, SWT.NONE, false,
                false, 1, 1));
        RowLayout layout = new RowLayout(SWT.HORIZONTAL);
        layout.pack = false;
        buttonComp.setLayout(layout);

        Button closeButton = new Button(buttonComp, SWT.PUSH);
        closeButton.setText("Close");
        closeButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }

        });
        closeButton.setLayoutData(new RowData(90, SWT.DEFAULT));
    }

    private void fillContextMenu(IMenuManager manager) {
        TableItem[] selection = resultTable.getSelection();
        if (selection == null || selection.length == 0) {
            return;
        }
        VenueId[] rooms = new VenueId[selection.length];
        for (int i = 0; i < rooms.length; i += 1) {
            rooms[i] = (VenueId) selection[i].getData();
        }
        if (rooms.length == 1) {
            manager.add(new JoinRoomAction(rooms[0], false));
            manager.add(new JoinRoomAction(rooms[0], true));
        }
        manager.add(new AddBookmarkAction(rooms));
    }

    private void search() {
        CollaborationConnection conn = CollaborationConnection.getConnection();
        Collection<VenueId> results = conn.getPublicRooms();
        resultTable.removeAll();
        if (results.size() > 0) {
            for (VenueId room : results) {
                TableItem ti = new TableItem(resultTable, SWT.NONE);
                ti.setText(0, room.getName());
                ti.setText(1, room.getHost());
                ti.setData(room);
            }
            resultTable.setEnabled(true);
        } else {
            TableItem ti = new TableItem(resultTable, SWT.NONE);
            ti.setText("No rooms found.");
            resultTable.setEnabled(false);
        }
    }

}
