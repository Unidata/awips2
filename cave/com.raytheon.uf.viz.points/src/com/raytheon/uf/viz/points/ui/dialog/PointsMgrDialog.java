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
package com.raytheon.uf.viz.points.ui.dialog;

import java.io.File;
import java.util.Collection;
import java.util.Iterator;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.TableColumnLayout;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.GroupNode;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.uf.viz.points.data.PointTransfer;
import com.raytheon.uf.viz.points.ui.layer.PointsToolLayer;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Dialog to manage points and point groups.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * October-2010              epolster    Initial Creation. 
 * Jul 31, 2012 #875       rferrel     Integrated into CAVE.
 * Oct  2, 2012 #1234      rferrel     Clicking on new group/point when no
 *                                      node selected will now add the new
 *                                      to the root node.
 * Apr 01, 2014 #2976      lvenable     Added SWT dispose checks in runAsync call.
 * 
 * </pre>
 * 
 * @author epolster
 * @version 1.0
 */
public class PointsMgrDialog extends CaveJFACEDialog implements
        IPointChangedListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointsMgrDialog.class);

    private static Rectangle DIALOG_BOUNDS = null;

    private static final int INITIAL_HEIGHT = 450;

    private static final int NEW_GROUP_ID = IDialogConstants.CLIENT_ID + 4;

    private static final int DELETE_POINT_ID = IDialogConstants.CLIENT_ID + 3;

    private static final int EDIT_POINT_ID = IDialogConstants.CLIENT_ID + 2;

    private static final int NEW_POINT_ID = IDialogConstants.CLIENT_ID + 1;

    Composite rootDialogArea;

    Composite mainDialogPanel;

    GridData dialogGridData;

    TableColumnLayout tableColumnLayout;

    TableViewerColumn pointNameTableViewerColumn;

    TableColumn pointNameTableColumn;

    TableViewerColumn pointMovableTableViewerColumn;

    TableColumn pointMovableTableColumn;

    TableViewerColumn pointHiddenTableViewerColumn;

    TableColumn pointHiddenTableColumn;

    Button newButton;

    Button newGroupButton;

    Button editButton;

    Button deleteButton;

    Button closeButton;

    PointsToolLayer toolLayer;

    private Shell currShell;

    protected TreeViewer pointsTreeViewer;

    private IPointNode topLevel;

    private PointsDataManager dataManager;

    private TreeEditor treeEditor;

    private Action createGroupAction;

    private Action createPointAction;

    private Action editNodeAction;

    private Action deleteNodeAction;

    protected IPointNode selectedNode = null;

    private boolean editSelectedNode = false;

    /**
     * Create the dialog.
     * 
     * @param parentShell
     */
    public PointsMgrDialog(Shell parentShell, PointsToolLayer layer) {
        super(parentShell);
        dataManager = PointsDataManager.getInstance();
        setShellStyle(SWT.SHELL_TRIM);
        toolLayer = layer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        currShell = newShell;
        newShell.setText("Points List");
        if (PointsMgrDialog.DIALOG_BOUNDS != null) {
            newShell.setBounds(PointsMgrDialog.DIALOG_BOUNDS);
        }
    }

    /**
     * Create contents of the dialog.
     * 
     * @param parent
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        rootDialogArea = (Composite) super.createDialogArea(parent);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        rootDialogArea.setLayoutData(gd);

        mainDialogPanel = new Composite(rootDialogArea, SWT.NONE);
        dialogGridData = new GridData(SWT.LEFT, SWT.CENTER, true, true, 1, 1);
        dialogGridData.heightHint = 193;
        dialogGridData.widthHint = 422;
        mainDialogPanel.setLayoutData(dialogGridData);
        mainDialogPanel.setLayout(new GridLayout(1, true));
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainDialogPanel.setLayoutData(gd);

        Composite tc = new Composite(mainDialogPanel, SWT.NONE);
        tc.setLayout(new GridLayout(1, false));
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tc.setLayoutData(gd);
        pointsTreeViewer = new TreeViewer(tc, SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.FULL_SELECTION);
        Tree tree = pointsTreeViewer.getTree();
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tree.setLayoutData(gd);

        tree.setHeaderVisible(true);
        TreeViewerColumn tvc = new TreeViewerColumn(pointsTreeViewer, SWT.LEFT,
                0);
        TreeColumn column = tvc.getColumn();
        column.setWidth(300);
        column.setText("Point Name");
        tvc = new TreeViewerColumn(pointsTreeViewer, SWT.LEFT, 1);
        column = tvc.getColumn();
        column.setWidth(80);
        column.setText("Movable");
        tvc.setEditingSupport(new PointMovableEditingSupport(pointsTreeViewer,
                toolLayer));

        tvc = new TreeViewerColumn(pointsTreeViewer, SWT.LEFT | SWT.CHECK, 2);
        column = tvc.getColumn();
        column.setWidth(80);
        column.setText("Hidden");
        tvc.setEditingSupport(new PointHiddenEditingSupport(pointsTreeViewer,
                toolLayer));

        pointsTreeViewer.setContentProvider(new PointTreeContentProvider());
        ILabelDecorator decorator = PlatformUI.getWorkbench()
                .getDecoratorManager().getLabelDecorator();
        PointTreeLabelProvider labelProvider = new PointTreeLabelProvider();
        labelProvider.setImageBackground(pointsTreeViewer.getTree()
                .getBackground());
        pointsTreeViewer.setLabelProvider(new PointDecoratingLabelProvider(
                labelProvider, decorator));
        pointsTreeViewer.setSorter(new PointTreeViewerSorter());
        loadTreeInput();
        pointsTreeViewer.setInput(topLevel);
        pointsTreeViewer.refresh();

        treeEditor = new TreeEditor(pointsTreeViewer.getTree());
        treeEditor.horizontalAlignment = SWT.LEFT;
        treeEditor.grabHorizontal = true;
        treeEditor.minimumWidth = 50;

        createGroupAction = new Action() {
            @Override
            public void run() {
                createGroup();
            }
        };
        createGroupAction.setText("New Group");

        createPointAction = new Action() {
            @Override
            public void run() {
                createPoint();
            }
        };
        createPointAction.setText("New Point...");

        editNodeAction = new Action() {
            @Override
            public void run() {
                editNode();
            }
        };
        editNodeAction.setText("Edit...");

        deleteNodeAction = new Action() {
            @Override
            public void run() {
                deleteNode();
            }
        };
        deleteNodeAction.setText("Delete");

        MenuManager menuMgr = new MenuManager();
        Menu menu = menuMgr.createContextMenu(pointsTreeViewer.getControl());
        menuMgr.add(createGroupAction);
        menuMgr.add(createPointAction);
        menuMgr.add(editNodeAction);
        menuMgr.add(deleteNodeAction);

        menuMgr.addMenuListener(new IMenuListener() {

            @Override
            public void menuAboutToShow(IMenuManager manager) {
                // TODO only add action if mouse is hovering over a row instead
                // of an empty area.
                manager.add(createGroupAction);
                manager.add(createPointAction);
                manager.add(editNodeAction);
                manager.add(deleteNodeAction);
            }
        });

        menuMgr.setRemoveAllWhenShown(true);
        pointsTreeViewer.getControl().setMenu(menu);

        int operations = DND.DROP_COPY | DND.DROP_MOVE;
        Transfer[] transferTypes = new Transfer[] { PointTransfer.getInstance() };
        pointsTreeViewer.addDragSupport(operations, transferTypes,
                new PointTreeDragSourceListener(pointsTreeViewer));

        pointsTreeViewer.addDropSupport(operations, transferTypes,
                new PointTreeDropListener(this));
        return rootDialogArea;
    }

    private Point[] getTableInput() {
        Collection<Point> points = dataManager.getPoints();
        Iterator<Point> iter = points.iterator();
        Point[] pointsArray = new Point[points.size()];
        for (int i = 0; i < points.size(); i++) {
            pointsArray[i] = iter.next();
        }
        return pointsArray;
    }

    private void loadTreeInput() {
        topLevel = dataManager.getPoint("");
    }

    public void refreshFromModel() {

        if (pointsTreeViewer != null && getShell() != null
                && getShell().getDisplay() != null
                && !getShell().getDisplay().isDisposed()) {
            loadTreeInput();
            getShell().getDisplay().syncExec(new Runnable() {
                public void run() {
                    pointsTreeViewer.setInput(getTableInput());
                }
            });
        }
    }

    /**
     * Create contents of the button bar.
     * 
     * @param parent
     */
    @Override
    protected void createButtonsForButtonBar(final Composite parent) {
        newGroupButton = createButton(parent, NEW_GROUP_ID, "New Group", false);
        newGroupButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                createGroup();
            }
        });

        newButton = createButton(parent, NEW_POINT_ID, "New Point...", false);
        newButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                createPoint();
            }
        });

        editButton = createButton(parent, EDIT_POINT_ID, "Edit...", false);
        editButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                editNode();
            }
        });

        deleteButton = createButton(parent, DELETE_POINT_ID, "Delete", false);
        deleteButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                deleteNode();
            }

        });

        closeButton = createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, false);
        closeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    private void createGroup() {
        IPointNode parentNode = null;

        selectedNode = getSelectedPoint();
        if (selectedNode == null) {
            parentNode = dataManager.getPoint("");
        } else {
            parentNode = dataManager.getParent(selectedNode);
        }

        try {
            setCursorBusy(true);
            editSelectedNode = true;
            selectedNode = dataManager.createTempGroup(parentNode);
        } catch (LocalizationOpFailedException e1) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Unable to create a temporary group under: "
                            + parentNode.getName());
            return;
        }
    }

    private void createPoint() {
        Point point = getSelectedPoint();
        if (point == null) {
            point = dataManager.getPoint("");
        }
        if (point != null) {
            ICloseCallback cb = new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Point) {
                        Point newPoint = (Point) returnValue;
                        setCursorBusy(true);
                        selectedNode = newPoint;
                        toolLayer.addPoint(newPoint);
                    }
                }
            };
            PointEditDialog.createNewPointViaDialog(toolLayer, point, cb);
        }
    }

    private TreeItem findItem(IPointNode point, TreeItem[] items) {
        TreeItem item = null;
        ViewerComparator vc = pointsTreeViewer.getComparator();
        for (TreeItem it : items) {
            IPointNode itPoint = (IPointNode) it.getData();
            if (itPoint != null) {
                if (vc.compare(pointsTreeViewer, point, itPoint) == 0) {
                    item = it;
                    break;
                } else if (it.getItemCount() > 0) {
                    item = findItem(point, it.getItems());
                    if (item != null) {
                        break;
                    }
                }
            }
        }
        return item;
    }

    private void editNode() {
        final Point point = getSelectedPoint();
        if (point != null) {
            if (point.isGroup()) {
                editGroupName();
            } else {
                setCursorBusy(true);
                ICloseCallback cb = new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof Point) {
                            if (returnValue instanceof Point) {
                                Point em = (Point) returnValue;
                                dataManager.updatePoint(point, em);
                            }
                        } else {
                            setCursorBusy(false);
                        }
                    }
                };
                PointEditDialog.editPointViaDialog(toolLayer, point, cb);
            }
        } else {
            MessageDialog.openInformation(getShell(), "Message",
                    "Please select a point to edit.");
        }
    }

    private void deleteNode() {
        Point point = getSelectedPoint();
        if (point != null) {
            setCursorBusy(true);
            if (!toolLayer.deletePoint(point)) {
                setCursorBusy(false);
            }
        }
    }

    private void editGroupName() {
        TreeSelection selection = (TreeSelection) pointsTreeViewer
                .getSelection();
        Control oldEditor = treeEditor.getEditor();
        if (oldEditor != null) {
            oldEditor.dispose();
        }
        final IPointNode entry = (IPointNode) selection.getFirstElement();

        final Composite comp = new Composite(pointsTreeViewer.getTree(),
                SWT.NONE);
        comp.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));
        final Text text = new Text(comp, SWT.NONE);
        text.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                Text text = (Text) treeEditor.getEditor();
                treeEditor.getItem().setText(text.getText());
            }
        });

        final TreeItem item = pointsTreeViewer.getTree().getSelection()[0];
        boolean showBorder = true;
        final Composite composite = new Composite(pointsTreeViewer.getTree(),
                SWT.NONE);
        if (showBorder)
            composite.setBackground(Display.getCurrent().getSystemColor(
                    SWT.COLOR_BLACK));
        final Text modText = new Text(composite, SWT.NONE);
        final int inset = showBorder ? 1 : 0;
        composite.addListener(SWT.Resize, new Listener() {
            public void handleEvent(Event e) {
                Rectangle rect = composite.getClientArea();
                modText.setBounds(rect.x + inset, rect.y + inset, rect.width
                        - inset * 2, rect.height - inset * 2);
            }
        });
        Listener textListener = new Listener() {
            public void handleEvent(final Event e) {
                switch (e.type) {
                case SWT.KeyUp:
                    if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
                        // do nothing, want to go on to the focus out
                    } else {
                        break;
                    }
                case SWT.FocusOut:
                    final String text = modText.getText().trim();
                    if (text.length() == 0 || entry.getName().equals(text)
                            || groupExists(dataManager.getParent(entry), text)) {
                        item.setText(entry.getName());
                    } else {
                        GroupNode node = new GroupNode((GroupNode) entry);
                        node.setName(text);
                        StringBuilder sb = new StringBuilder(node.getGroup());
                        sb.setLength(sb.lastIndexOf(File.separator) + 1);
                        selectedNode = node;
                        sb.append(text);

                        item.setText(text);
                        setCursorBusy(true);
                        if (!dataManager.renameGroup(entry, text)) {
                            setCursorBusy(false);
                        }
                    }
                    composite.dispose();
                    break;
                case SWT.Verify:
                    if (!e.text.matches("[0-9A-Za-z_ ]*")) {
                        e.doit = false;
                    }

                    break;
                default:
                    statusHandler.handle(Priority.PROBLEM, "Unhandled type: "
                            + e.type);
                }
            }
        };
        modText.addListener(SWT.KeyUp, textListener);
        modText.addListener(SWT.Verify, textListener);
        modText.addListener(SWT.FocusOut, textListener);
        treeEditor.setEditor(composite, item);
        String tmpText = item.getText();
        modText.setText(tmpText);
        modText.selectAll();
        modText.setFocus();
    }

    private boolean groupExists(IPointNode parent, String name) {
        for (IPointNode child : dataManager.getChildren(parent, true)) {
            if (child.isGroup() && name.equals(child.getName())) {
                return true;
            }
        }
        return false;
    }

    private Point getSelectedPoint() {
        TreeItem[] selItems = pointsTreeViewer.getTree().getSelection();
        Point point = null;
        if (selItems.length > 0) {
            point = (Point) selItems[0].getData();
        }
        return point;
    }

    protected void setCursorBusy(boolean state) {
        Cursor cursor = null;
        if (state) {
            cursor = getShell().getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
        }
        getShell().setCursor(cursor);
        pointsTreeViewer.getTree().setCursor(cursor);
    }

    @Override
    public boolean close() {
        dataManager.removePointsChangedListener(this);
        DIALOG_BOUNDS = currShell.getBounds();
        return super.close();
    }

    @Override
    public int open() {
        dataManager.addPointsChangedListener(this);
        return super.open();
    }

    @Override
    public void pointChanged() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (pointsTreeViewer.getTree().isDisposed()) {
                    return;
                }

                if (selectedNode == null) {
                    selectedNode = getSelectedPoint();
                }

                // Bug in viewers refresh that causes stack overflow when
                // selected item no longer exists and sometimes when the
                // selected item was modified.
                pointsTreeViewer.setSelection(null);
                pointsTreeViewer.refresh(topLevel);

                if (selectedNode != null) {
                    Tree tree = pointsTreeViewer.getTree();
                    TreeItem item = findItem(selectedNode, tree.getItems());
                    if (item != null) {
                        tree.showItem(item);
                        tree.select(item);
                        if (editSelectedNode) {
                            editNode();
                            editSelectedNode = false;
                        }
                    }
                    selectedNode = null;
                }
                setCursorBusy(false);
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
     */
    @Override
    protected org.eclipse.swt.graphics.Point getInitialSize() {
        if (DIALOG_BOUNDS == null) {
            org.eclipse.swt.graphics.Point pt = super.getInitialSize();
            if (pt.y < INITIAL_HEIGHT) {
                pt.y = INITIAL_HEIGHT;
            }
            return pt;
        }
        return new org.eclipse.swt.graphics.Point(DIALOG_BOUNDS.width,
                DIALOG_BOUNDS.height);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#getInitialLocation(org.eclipse.swt.graphics
     * .Point)
     */
    @Override
    protected org.eclipse.swt.graphics.Point getInitialLocation(
            org.eclipse.swt.graphics.Point initialSize) {
        if (DIALOG_BOUNDS == null) {
            return super.getInitialLocation(initialSize);
        }
        return new org.eclipse.swt.graphics.Point(DIALOG_BOUNDS.x,
                DIALOG_BOUNDS.y);
    }
}
