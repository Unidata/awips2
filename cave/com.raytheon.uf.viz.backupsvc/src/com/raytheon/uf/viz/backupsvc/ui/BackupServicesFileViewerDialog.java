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
package com.raytheon.uf.viz.backupsvc.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcJobInfo;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileDeleteRequest;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileSaveRequest;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Basic Dialog to display file contents in a StyledText widget.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2021 84475      Robert.Blum     Initial creation
 * Jun 08, 2021 84475      Robert.Blum     Added FileTree and other updates.
 * Jun 22, 2021 92788      Gang Chen       Replace info.getJobName() with path
 * Aug 03, 2021 94861      Lisa.Singh      Fixed bug where file would not open in viewer.
 * Aug 13, 2021 92922      Robert.Blum     Updated to work with request blobs.
 * Aug 13, 2021 93179      Amanuel Challa  Moved LocalizationFileSaveRequest/DeleteRequest to
 *                                         com.raytheon.uf.common.localization.backup.request
 *
 * </pre>
 *
 * @author Robert.Blum
 */

public class BackupServicesFileViewerDialog extends CaveSWTDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(this.getClass());

    private final List<BackupSvcJobInfo> infos;

    private Tree tree;

    private TabFolder folder;

    private List<String> nodes = new ArrayList<>();

    private Map<String, List<String>> children = new HashMap<>();

    private BackupSvcJobInfo selectedInfo;

    protected BackupServicesFileViewerDialog(Shell parent,
            List<BackupSvcJobInfo> infos) {
        super(parent, SWT.SHELL_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT);
        setText("Backup Services - File Viewer");
        this.infos = infos;

        // Construct the nodes and child mapping needed for the file tree
        for (BackupSvcJobInfo info : infos) {
            String path = info.getJobFileLocation() + info.getJobFilePath();
            String[] dirs = path.split(IPathManager.SEPARATOR);
            String node = null;
            for (int i = 1; i < dirs.length; i++) {
                String dir = dirs[i];
                if (node != null) {
                    node += IPathManager.SEPARATOR;
                    node += dir;
                } else {
                    node = dir;
                }

                // Keep order but don't allow dups
                if (!nodes.contains(node)) {
                    nodes.add(node);
                }

                if (!children.containsKey(node)) {
                    children.put(node, new ArrayList<>());
                }
                if (i + 1 < dirs.length) {
                    children.get(node)
                            .add(node + IPathManager.SEPARATOR + dirs[i + 1]);
                }
            }
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainComp.setLayoutData(gridData);
        mainComp.setLayout(new GridLayout(2, false));

        createFileTree(mainComp);
        createFileViewer(mainComp);

        Button closeButton = new Button(mainComp, SWT.PUSH);
        gridData = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gridData.horizontalSpan = 2;
        closeButton.setLayoutData(gridData);
        closeButton.setText("Close");
        closeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Creation of the tree component
     *
     * @param parent
     *            composite to add tree to
     */
    private void createFileTree(Composite parent) {
        Composite treeComp = new Composite(parent, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.widthHint = 300;
        gridData.heightHint = 400;
        treeComp.setLayoutData(gridData);
        treeComp.setLayout(new GridLayout(1, false));

        tree = new Tree(treeComp, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        tree.setRedraw(false);
        tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        tree.setHeaderVisible(true);
        tree.setLinesVisible(true);

        TreeColumn column = new TreeColumn(tree, SWT.None);
        column.setMoveable(false);
        column.setText("Location:");

        List<TreeItem> treeItems = new ArrayList<>();
        for (String node : nodes) {
            TreeItem parentItem = getParentTreeItem(node, treeItems);
            TreeItem treeItem = null;
            if (parentItem == null) {
                treeItem = new TreeItem(tree, SWT.NONE);
            } else {
                treeItem = new TreeItem(parentItem, SWT.NONE);
            }
            String displayText = node;
            if (node.contains(IPathManager.SEPARATOR)) {
                displayText = node.substring(
                        node.lastIndexOf(IPathManager.SEPARATOR) + 1);
            }
            treeItem.setText(displayText);
            treeItem.setData(node);
            treeItem.setExpanded(true);

            treeItems.add(treeItem);
        }

        // only Select first child item
        outer: for (TreeItem item : treeItems) {
            if (item.getItems().length == 0) {
                tree.setSelection(item);
                String path;
                for (BackupSvcJobInfo info : infos) {
                    path = info.getJobFileLocation() + info.getJobFilePath();
                    if (path.contains(((String) item.getData()))) {
                        selectedInfo = info;
                        break outer;
                    }
                }
            }
        }

        tree.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (tree.getSelectionCount() != 0) {
                    BackupSvcJobInfo info = getInfoForTreeItem();
                    if (info != null) {
                        syncFileTreeAndTabFolder(info);
                    }
                }
            }
        });

        column.pack();
        tree.setRedraw(true);
    }

    private BackupSvcJobInfo getInfoForTreeItem() {
        // Tree is only single select
        TreeItem item = tree.getSelection()[0];
        if (item.getItems().length == 0) {
            String path;
            for (BackupSvcJobInfo info : infos) {
                path = info.getJobFileLocation() + info.getJobFilePath();
                if (path.contains(((String) item.getData()))) {
                    return info;
                }
            }
        }
        return null;
    }

    private TreeItem getParentTreeItem(String node, List<TreeItem> treeItems) {
        for (Entry<String, List<String>> entry : children.entrySet()) {
            if (entry.getValue().contains(node)) {
                for (TreeItem item : treeItems) {
                    if (item.getData().equals(entry.getKey())) {
                        return item;
                    }
                }
            }
        }
        return null;
    }

    private void createFileViewer(Composite mainComp) {
        folder = new TabFolder(mainComp, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        folder.setLayoutData(gridData);
        String path;
        for (BackupSvcJobInfo info : infos) {
            path = info.getJobFileLocation() + info.getJobFilePath();
            TabItem fileTab = new TabItem(folder, SWT.NONE);
            String tabText = path
                    .substring(path.lastIndexOf(IPathManager.SEPARATOR) + 1);
            fileTab.setText(tabText);
            fileTab.setData(info);

            Composite tabComp = new Composite(folder, SWT.NONE);
            gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
            tabComp.setLayoutData(gridData);
            tabComp.setLayout(new GridLayout(1, true));

            fileTab.setControl(tabComp);

            StyledText fileContents = new StyledText(tabComp,
                    SWT.READ_ONLY | SWT.V_SCROLL | SWT.H_SCROLL);
            fileContents.setLayoutData(
                    new GridData(SWT.FILL, SWT.FILL, true, true));

            byte[] blob = info.getRequestBlob();
            IServerRequest request = null;
            try {
                request = (IServerRequest) DynamicSerializationManager
                        .getManager(SerializationType.Thrift).deserialize(blob);
            } catch (SerializationException e) {
                statusHandler.error(
                        "Failed to deserialize request from BackupJob", e);
            }
            if (request instanceof LocalizationFileSaveRequest) {
                LocalizationFileSaveRequest saveRequest = (LocalizationFileSaveRequest) request;
                fileContents.setText(new String(saveRequest.getBytes()));
            } else if (request instanceof LocalizationFileDeleteRequest) {
                fileContents.setText("Localization File was Deleted");
            }
        }
        for (TabItem item : folder.getItems()) {
            if (selectedInfo.equals(item.getData())) {
                folder.setSelection(item);
                break;
            }
        }

        folder.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                syncFileTreeAndTabFolder((BackupSvcJobInfo) folder
                        .getItem(folder.getSelectionIndex()).getData());
            }
        });
        folder.pack();
    }

    private void syncFileTreeAndTabFolder(BackupSvcJobInfo info) {
        selectedInfo = info;
        for (TreeItem item : tree.getItems()) {
            if (selectActiveTreeItem(info, item)) {
                break;
            }
        }
        for (TabItem item : folder.getItems()) {
            if (item.getData().equals(info)) {
                folder.setSelection(item);
                break;
            }
        }
    }

    private boolean selectActiveTreeItem(BackupSvcJobInfo info, TreeItem item) {
        String path = info.getJobFileLocation() + info.getJobFilePath();
        if (item.getItems().length == 0) {
            if (path.contains((String) item.getData())) {
                tree.setSelection(item);
                return true;
            }
            return false;
        } else {
            for (TreeItem childItem : item.getItems()) {
                if (selectActiveTreeItem(info, childItem)) {
                    return true;
                }
            }
        }
        return false;
    }
}
