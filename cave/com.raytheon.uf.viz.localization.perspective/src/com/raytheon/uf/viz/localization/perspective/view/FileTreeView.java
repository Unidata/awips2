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
package com.raytheon.uf.viz.localization.perspective.view;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.compare.CompareUI;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.FileTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationNotificationObserver;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.localization.LocalizationEditorInput;
import com.raytheon.uf.viz.localization.LocalizationPerspectiveUtils;
import com.raytheon.uf.viz.localization.adapter.LocalizationPerspectiveAdapter;
import com.raytheon.uf.viz.localization.filetreeview.FileTreeEntryData;
import com.raytheon.uf.viz.localization.filetreeview.LocalizationFileEntryData;
import com.raytheon.uf.viz.localization.filetreeview.LocalizationFileGroupData;
import com.raytheon.uf.viz.localization.filetreeview.PathData;
import com.raytheon.uf.viz.localization.perspective.Activator;
import com.raytheon.uf.viz.localization.perspective.ui.compare.LocalizationCompareEditorInput;
import com.raytheon.uf.viz.localization.perspective.view.actions.CopyToAction;
import com.raytheon.uf.viz.localization.perspective.view.actions.DeleteAction;
import com.raytheon.uf.viz.localization.perspective.view.actions.ImportFileAction;
import com.raytheon.uf.viz.localization.perspective.view.actions.MoveFileAction;
import com.raytheon.uf.viz.localization.perspective.view.actions.OpenAction;
import com.raytheon.uf.viz.localization.perspective.view.actions.OpenWithAction;
import com.raytheon.uf.viz.localization.perspective.view.actions.PasteFileAction;
import com.raytheon.uf.viz.localization.perspective.view.actions.ShowAllAction;
import com.raytheon.uf.viz.localization.service.ILocalizationService;

/**
 * File Tree View for the localization perspective.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 26, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class FileTreeView extends ViewPart implements IPartListener2,
        ILocalizationFileObserver, ILocalizationService,
        IResourceChangeListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FileTreeView.class);

    private static class FileTreeFileComparator implements
            Comparator<LocalizationFile> {

        /*
         * (non-Javadoc)
         * 
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(LocalizationFile o1, LocalizationFile o2) {
            if (o1.isDirectory() && (o2.isDirectory() == false)) {
                return 1;
            } else if ((o1.isDirectory() == false) && o2.isDirectory()) {
                return -1;
            } else {
                int nameVal = o1.getName().compareTo(o2.getName());
                if (nameVal == 0) {
                    // exact same name, check levels
                    LocalizationLevel l1 = o1.getContext()
                            .getLocalizationLevel();
                    LocalizationLevel l2 = o2.getContext()
                            .getLocalizationLevel();
                    if (l1 == l2) {
                        // exact same level, check ctx name
                        String ctxName1 = o1.getContext().getContextName();
                        String ctxName2 = o2.getContext().getContextName();
                        String myContext = LocalizationManager
                                .getContextName(l1);
                        if (myContext == null) {
                            if (ctxName1 == null) {
                                return -1;
                            } else if (ctxName2 == null) {
                                return 1;
                            }
                            return ctxName1.compareTo(ctxName2);
                        } else {
                            if (myContext.equals(ctxName1)) {
                                return -1;
                            } else if (myContext.equals(ctxName2)) {
                                return 1;
                            }
                            return ctxName1.compareTo(ctxName2);
                        }
                    } else {
                        return l1.compareTo(l2);
                    }
                } else {
                    return nameVal;
                }
            }
        }

    }

    private static final String PATH_DEFINITION_ID = "com.raytheon.uf.viz.localization.localizationpath";

    private static final LocalizationPerspectiveAdapter DEFAULT_ADAPTER = new LocalizationPerspectiveAdapter();

    private boolean linkWithEditor = true;

    private Set<LocalizationLevel> showAllSet = new HashSet<LocalizationLevel>();

    private Map<LocalizationLevel, Set<String>> contextMap = new HashMap<LocalizationLevel, Set<String>>();

    /**
     * The File Tree widget.
     */
    private TreeViewer viewer;

    /** Image map */
    private Map<ImageDescriptor, Image> imageMap = new HashMap<ImageDescriptor, Image>();

    /**
     * Director Game
     */
    private Image dirImg;

    /**
     * Extension list
     */
    private IExtension[] extensions;

    /**
     * The wait mouse pointer.
     */
    private Cursor waitCursor = null;

    /**
     * The normal arrow mouse pointer.
     */
    private Cursor arrowCursor = null;

    private LocalizationFile copyFile = null;

    /** Application map for root tree nodes */
    private Map<String, List<TreeItem>> applicationMap = new TreeMap<String, List<TreeItem>>();

    private IProject localizationProject;

    public FileTreeView() {
        super();
        localizationProject = ResourcesPlugin
                .getWorkspace()
                .getRoot()
                .getProject(
                        com.raytheon.uf.viz.localization.Activator.LOCALIZATION_PROJECT);
        dirImg = getImageDescriptor("directory.gif").createImage();

        IExtensionRegistry registry = Platform.getExtensionRegistry();

        IExtensionPoint point = registry.getExtensionPoint(PATH_DEFINITION_ID);
        if (point != null) {
            extensions = point.getExtensions();
        } else {
            extensions = new IExtension[0];
        }

        Display display = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell().getDisplay();
        waitCursor = display.getSystemCursor(SWT.CURSOR_WAIT);
        arrowCursor = display.getSystemCursor(SWT.CURSOR_ARROW);
    }

    /**
     * @return the tree
     */
    public Tree getTree() {
        return viewer.getTree();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
     */
    @Override
    public void init(IViewSite site) throws PartInitException {
        super.init(site);

        site.getPage().addPartListener(this);
        LocalizationNotificationObserver.getInstance()
                .addGlobalFileChangeObserver(this);

        ResourcesPlugin.getWorkspace().addResourceChangeListener(this,
                IResourceChangeEvent.POST_CHANGE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
        if (dirImg.isDisposed() == false) {
            dirImg.dispose();
        }

        for (Image img : imageMap.values()) {
            if (img.isDisposed() == false) {
                img.dispose();
            }
        }

        if (getTree().isDisposed() == false) {
            getTree().dispose();
        }

        getSite().getPage().removePartListener(this);
        LocalizationNotificationObserver.getInstance()
                .removeGlobalFileChangeObserver(this);

        ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        // construct tree
        createFileTree(parent);
        populateTree();
    }

    /**
     * Creation of the tree component
     * 
     * @param parent
     *            composite to add tree to
     */
    private void createFileTree(Composite parent) {
        Tree tree = new Tree(parent, SWT.BORDER | SWT.MULTI);
        tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        viewer = new TreeViewer(tree);

        tree.addListener(SWT.Expand, new Listener() {
            @Override
            public void handleEvent(Event event) {
                populateNode((TreeItem) event.item);
            }
        });

        tree.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                Tree tree = getTree();
                TreeItem[] items = tree.getSelection();
                if (items.length == 1) {
                    Object data = items[0].getData();
                    if (data instanceof LocalizationFileEntryData) {
                        IFile file = ((LocalizationFileEntryData) data)
                                .getResource();
                        IWorkbenchPage page = getSite().getPage();
                        for (IEditorReference ref : page.getEditorReferences()) {
                            IEditorPart part = ref.getEditor(false);
                            IEditorInput input = part.getEditorInput();
                            if (input instanceof LocalizationEditorInput) {
                                if (file == ((LocalizationEditorInput) input)
                                        .getFile()) {
                                    page.activate(part);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        });

        tree.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                Tree t = (Tree) e.getSource();
                TreeItem[] selections = t.getSelection();
                if (selections.length == 0) {
                    return;
                }
                TreeItem ti = selections[0];
                if (ti.getData() instanceof LocalizationFileEntryData) {
                    FileTreeEntryData data = (FileTreeEntryData) ti.getData();
                    if (data instanceof LocalizationFileEntryData) {
                        new OpenAction(
                                getSite().getPage(),
                                new LocalizationFileEntryData[] { (LocalizationFileEntryData) data })
                                .run();
                    }
                } else {
                    populateNode(ti);
                    if (ti.getExpanded() == false) {
                        ti.setExpanded(true);
                    } else {
                        ti.setExpanded(false);
                    }
                }
            }
        });

        MenuManager menuMgr = new MenuManager();
        menuMgr.setRemoveAllWhenShown(true);
        menuMgr.addMenuListener(new IMenuListener() {
            public void menuAboutToShow(IMenuManager mgr) {
                fillContextMenu(mgr);
            }
        });
        Menu menu = menuMgr.createContextMenu(tree);
        tree.setMenu(menu);

        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        Action linkAction = new Action("Link with Editor", IAction.AS_CHECK_BOX) {
            @Override
            public void run() {
                linkWithEditor = !linkWithEditor;
                if (linkWithEditor) {
                    IEditorPart part = getSite().getPage().getActiveEditor();
                    if (part != null) {
                        IEditorInput input = part.getEditorInput();
                        if (input instanceof LocalizationEditorInput) {
                            selectFile(((LocalizationEditorInput) input)
                                    .getLocalizationFile());
                        }
                    }
                }
                setChecked(linkWithEditor);
            }
        };
        linkAction.setChecked(linkWithEditor);
        linkAction.setImageDescriptor(getImageDescriptor("link.gif"));

        Action collapseAction = new Action("Collapse All",
                IAction.AS_PUSH_BUTTON) {
            @Override
            public void run() {
                Tree tree = getTree();
                for (TreeItem item : tree.getItems()) {
                    recursiveCollapse(item);
                }
            }

            private void recursiveCollapse(TreeItem item) {
                for (TreeItem ti : item.getItems()) {
                    recursiveCollapse(ti);
                }
                item.setExpanded(false);
            }
        };
        collapseAction.setImageDescriptor(getImageDescriptor("collapse.gif"));

        mgr.add(collapseAction);
        mgr.add(linkAction);

        IMenuManager viewMenu = getViewSite().getActionBars().getMenuManager();
        viewMenu.add(new ShowAllAction(this));
        viewMenu.add(new Separator());
        viewMenu.add(linkAction);

        LocalizationFileDragNDropSource dragNDropSource = new LocalizationFileDragNDropSource(
                this, viewer);
        viewer.addDragSupport(DND.DROP_MOVE | DND.DROP_COPY,
                new Transfer[] { FileTransfer.getInstance() }, dragNDropSource);
        viewer.addDropSupport(DND.DROP_MOVE | DND.DROP_COPY,
                new Transfer[] { FileTransfer.getInstance() }, dragNDropSource);
    }

    /**
     * Repopulates all tree items in the tree.
     * 
     * @param item
     */
    private void repopulateTree(Tree tree) {
        for (TreeItem item : tree.getItems()) {
            repopulateTree(item);
        }
    }

    /**
     * Repopulates all tree items from this item down.
     * 
     * @param item
     */
    private void repopulateTree(TreeItem item) {
        repopulateTree(item, false);
    }

    private void repopulateTree(TreeItem item, boolean force) {
        if (item.getData() == null) {
            return;
        } else if (item.getData() instanceof LocalizationFileEntryData) {
            IResource rsc = ((LocalizationFileEntryData) item.getData())
                    .getResource();
            try {
                rsc.refreshLocal(IResource.DEPTH_INFINITE, null);
            } catch (CoreException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error refreshing file: " + e.getLocalizedMessage(), e);
            }
            return;
        }
        if (item.getData() instanceof LocalizationFileGroupData == false) {
            for (TreeItem ti : item.getItems()) {
                repopulateTree(ti, force);
            }
        }

        boolean removeEmpty = true;
        if (item.getData() instanceof FileTreeEntryData) {
            FileTreeEntryData data = (FileTreeEntryData) item.getData();
            if (data.hasRequestedChildren() || force) {
                boolean wasExpanded = item.getExpanded();
                item.removeAll();
                data.setRequestedChildren(false);
                // We only want to remove empty nodes if we successfully
                // populated the node (no errors)
                removeEmpty = populateNode(item);
                item.setExpanded(wasExpanded);
            }
        }

        if (removeEmpty && item.getItemCount() == 0) {
            removeEmptyNode(item);
        }
    }

    /**
     * @param item
     */
    private void removeEmptyNode(TreeItem item) {
        TreeItem parentItem = item.getParentItem();
        if (parentItem != null && parentItem.getItemCount() == 1) {
            removeEmptyNode(parentItem);
        } else {
            item.dispose();
        }
    }

    /**
     * Initial population of the tree.
     */
    private void populateTree() {
        List<PathData> pathList = getPathData();
        for (PathData pd : pathList) {
            addPathDataTreeNode(pd);
        }
    }

    /**
     * Adds nodes to tree for path data object
     * 
     * @param pd
     */
    private void addPathDataTreeNode(PathData pd) {
        if (pd.getApplication().contains(File.separator)) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Problem adding "
                                    + pd.getApplication()
                                    + " folder: Path names cannot contain file separator string");
            return;
        }
        if (pd.getName().contains(File.separator)) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Problem adding "
                                    + pd.getName()
                                    + " folder: Path names cannot contain file separator string");
            return;
        }

        String application = pd.getApplication();
        List<TreeItem> itemList = null;
        TreeItem root = null;

        if (applicationMap.containsKey(application) == false) {
            itemList = new ArrayList<TreeItem>();
            root = new TreeItem(getTree(), SWT.NONE, getInsertionIndex(
                    getTree().getItems(), application));
            root.setText(application);
            root.setImage(getImage((String) null));
            applicationMap.put(application, itemList);

            // Create folder in project for application
            IFolder folder = localizationProject.getFolder(application);
            if (folder.exists() == false) {
                try {
                    folder.create(true, true, null);
                } catch (CoreException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error creating application folder", e);
                }
            }
            root.setData(folder);
        } else {
            itemList = applicationMap.get(application);
            root = itemList.get(0).getParentItem();
        }

        FileTreeEntryData treeData = new FileTreeEntryData(pd, pd.getPath());
        treeData.setName(pd.getName());
        TreeItem dataTypeItem = new TreeItem(root, SWT.NONE, getInsertionIndex(
                root.getItems(), treeData.getName()));
        dataTypeItem.setData(treeData);
        dataTypeItem.setText(treeData.getName());
        dataTypeItem.setImage(getImage((String) null));
        new TreeItem(dataTypeItem, SWT.NONE);
        itemList.add(dataTypeItem);

        // Create folder for PathData
        treeData.setResource(createFolder((IFolder) root.getData(),
                treeData.getName()));
    }

    private IFolder createFolder(IFolder parent, String name) {
        IFolder folder = parent.getFolder(name);
        if (folder.exists() == false) {
            try {
                folder.create(true, true, null);
            } catch (CoreException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error creating application folder", e);
            }
        }
        return folder;
    }

    /**
     * Get the insertion index if one were to insert a tree item into the
     * curItems list with text of name
     * 
     * @param curItems
     * @param name
     * @return
     */
    private int getInsertionIndex(TreeItem[] curItems, String name) {
        int idx = curItems.length; // add to end
        for (int i = 0; i < curItems.length; ++i) {
            String text = curItems[i].getText();
            int comp = text.compareToIgnoreCase(name);
            if (comp >= 0) {
                idx = i;
                break;
            }
        }
        return idx;
    }

    /**
     * Fill the context menu with localization file operatiosn
     * 
     * @param mgr
     *            menu manager to fill
     */
    private void fillContextMenu(IMenuManager mgr) {
        Tree tree = getTree();
        final TreeItem[] selected = tree.getSelection();

        if (selected.length > 0) {
            List<FileTreeEntryData> dataList = new ArrayList<FileTreeEntryData>();

            // Get the selected items
            for (TreeItem item : selected) {
                if (item.getData() instanceof FileTreeEntryData) {
                    FileTreeEntryData data = (FileTreeEntryData) item.getData();
                    dataList.add(data);
                } else {
                    dataList.clear();
                    break;
                }
            }

            if (dataList.size() > 0) {
                // Verify the PathData objects are the same
                PathData prev = dataList.get(0).getPathData();
                for (FileTreeEntryData data : dataList) {
                    if (data.getPathData() != prev) {
                        dataList.clear();
                        break;
                    } else {
                        prev = data.getPathData();
                    }
                }

                if (dataList.size() > 0) {
                    // All the same... add adapter items
                    if (prev.getAdapter().addContextMenuItems(
                            mgr,
                            dataList.toArray(new FileTreeEntryData[dataList
                                    .size()]))) {
                        mgr.add(new Separator());
                    }
                }
            }
        }

        final List<LocalizationFile> fileList = new ArrayList<LocalizationFile>();
        final List<LocalizationFileEntryData> fileDataList = new ArrayList<LocalizationFileEntryData>();
        // get list of files selected
        if (selected.length > 0) {
            // Open File(s) list
            for (TreeItem item : selected) {
                if (item.getData() instanceof LocalizationFileEntryData) {
                    LocalizationFileEntryData data = (LocalizationFileEntryData) item
                            .getData();
                    fileDataList.add(data);
                    fileList.add(data.getFile());
                }
            }
        }

        // Add "Open" group
        if (fileList.size() > 0) {
            mgr.add(new OpenAction(getSite().getPage(), fileDataList
                    .toArray(new LocalizationFileEntryData[fileList.size()])));
            if (fileList.size() == 1 && selected.length == 1) {
                // add open with...
                LocalizationFileEntryData data = (LocalizationFileEntryData) selected[0]
                        .getData();
                mgr.add(new OpenWithAction(getSite().getPage(), fileDataList
                        .get(0), data.getResource(), data.getPathData()
                        .getAdapter()));
            }
            mgr.add(new Separator());
        }

        // Add Copy/Paste/Delete
        if (fileList.size() == 1 && selected.length == 1) {
            LocalizationFile selectedFile = fileList.get(0);
            mgr.add(new Action("Copy") {
                @Override
                public void run() {
                    copyFile = fileList.get(0);
                }
            });

            mgr.add(new CopyToAction(selectedFile, this));
            mgr.add(new DeleteAction(getSite().getPage(), fileList
                    .toArray(new LocalizationFile[fileList.size()])));

            mgr.add(new Separator());
        } else if (selected.length == 1
                && selected[0].getData() instanceof LocalizationFileGroupData
                && copyFile != null) {
            mgr.add(new PasteFileAction(this, this.copyFile,
                    (LocalizationFileGroupData) selected[0].getData()));
            mgr.add(new Separator());
        } else if (fileList.size() == selected.length) {
            mgr.add(new DeleteAction(getSite().getPage(), fileList
                    .toArray(new LocalizationFile[fileList.size()])));
            mgr.add(new Separator());
        }

        // Add the move to item
        if (selected.length == 1 && fileList.size() == 1) {
            mgr.add(new MoveFileAction(getSite().getPage(), fileList.get(0),
                    this));
            mgr.add(new Separator());
        }

        // Add the compare item
        if (selected.length == 2 && fileList.size() == 2) {
            mgr.add(new Separator());
            mgr.add(new Action("Compare") {
                @Override
                public void run() {
                    loadCompare(
                            (LocalizationFileEntryData) selected[0].getData(),
                            (LocalizationFileEntryData) selected[1].getData());
                }

                @Override
                public boolean isEnabled() {
                    if ((getTree().getSelection()[0].getData() instanceof LocalizationFileEntryData)
                            && (getTree().getSelection()[1].getData() instanceof LocalizationFileEntryData)) {
                        return true;
                    }
                    return false;
                }
            });
            mgr.add(new Separator());
        }

        if (selected.length == 1) {
            mgr.add(new Action("Refresh") {
                @Override
                public void run() {
                    refresh(selected[0]);
                }
            });
        }

        if (selected.length == 1) {
            Object data = selected[0].getData();
            if (data instanceof FileTreeEntryData) {
                FileTreeEntryData fdata = (FileTreeEntryData) data;
                if (fdata.isDirectory()
                        && fdata instanceof LocalizationFileGroupData == false) {
                    // We can import into true directories, not group datas
                    mgr.add(new Separator());
                    mgr.add(new ImportFileAction(fdata.getPathData().getType(),
                            fdata.getPath(), fdata.getPathData().getFilter()));
                }
            }
        }
    }

    /**
     * Refresh the selected tree items
     */
    public void refreshSelected() {
        TreeItem[] selected = getTree().getSelection();
        if (selected != null && selected.length == 1) {
            refresh(selected[0]);
        }
    }

    private void refresh(TreeItem refresh) {
        refresh.getParent().getShell().setCursor(waitCursor);
        TreeItem[] items = new TreeItem[] { refresh };
        if (refresh.getData() instanceof FileTreeEntryData == false) {
            items = refresh.getItems();
        }

        for (TreeItem item : items) {
            FileTreeEntryData data = (FileTreeEntryData) item.getData();
            IResource rsc = data.getResource();
            if (rsc instanceof IFile) {
                try {
                    rsc.refreshLocal(IResource.DEPTH_INFINITE, null);
                } catch (CoreException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error refreshing file", e);
                }
            } else {
                boolean wasExpanded = item.getExpanded();
                List<FileTreeEntryData> expanded = new ArrayList<FileTreeEntryData>();
                if (wasExpanded) {
                    fillExpanded(item, expanded);
                }
                item.removeAll();
                data.setRequestedChildren(false);
                populateNode(item);
                item.setExpanded(wasExpanded);
                for (FileTreeEntryData expand : expanded) {
                    TreeItem found = find(
                            expand.getPath(),
                            PathManagerFactory.getPathManager().getContext(
                                    expand.getPathData().getType(),
                                    LocalizationLevel.BASE), false);
                    if (found != null) {
                        populateNode(found);
                        recursiveExpand(found);
                    }
                }
            }
        }

        refresh.getParent().getShell().setCursor(arrowCursor);
    }

    private void recursiveExpand(TreeItem item) {
        if (item.getExpanded() == false) {
            recursiveExpand(item.getParentItem());
            item.setExpanded(true);
        }
    }

    private boolean fillExpanded(TreeItem item, List<FileTreeEntryData> expanded) {
        boolean hasExpandedChildren = false;
        for (TreeItem ti : item.getItems()) {
            if (ti.getExpanded()) {
                hasExpandedChildren = true;
                if (!fillExpanded(ti, expanded)) {
                    expanded.add((FileTreeEntryData) ti.getData());
                }
            }
        }
        return hasExpandedChildren;
    }

    /**
     * Expand the given tree node requesting data as needed
     * 
     * @param parentItem
     *            The TreeItem node to expand
     */
    private boolean populateNode(TreeItem parentItem) {
        if (parentItem == null) {
            return false;
        }

        // Top (Application) level has no data to request
        if ((parentItem.getData() instanceof FileTreeEntryData) == false) {
            return false;
        }

        FileTreeEntryData data = (FileTreeEntryData) parentItem.getData();
        if (data instanceof LocalizationFileEntryData
                || data.hasRequestedChildren()) {
            // Can't expand a file, or we've already requested
            return true;
        }

        // Remove all children to get rid of placeholder child
        setWaitCursor();

        try {
            boolean checkName = !(data instanceof LocalizationFileGroupData);

            PathData pd = data.getPathData();
            String path = data.getPath();
            String[] filter = pd.getFilter();
            boolean recursive = pd.isRecursive();
            LocalizationType type = pd.getType();

            IPathManager pathManager = PathManagerFactory.getPathManager();

            // Request for base/site/user

            List<LocalizationContext> searchContexts = new ArrayList<LocalizationContext>(
                    Arrays.asList(pathManager.getLocalSearchHierarchy(type)));

            // Use of LocalizationLevels.values() in this case should be okay
            // since
            // we are requesting all possible context names for the level,
            // doesn't
            // matter if our local context for the level is set
            LocalizationLevel[] levels = pathManager.getAvailableLevels();
            for (LocalizationLevel level : levels) {
                if (showAllSet.contains(level)) {
                    Set<String> contexts = contextMap.get(level);
                    if (contexts == null) {
                        contexts = new HashSet<String>(
                                Arrays.asList(PathManagerFactory
                                        .getPathManager().getContextList(level)));
                        contextMap.put(level, contexts);
                    }
                    String myContext = LocalizationManager
                            .getContextName(level);

                    for (String context : contexts) {
                        if ((myContext != null && myContext.equals(context))
                                || (myContext == null && context == null)) {
                            continue;
                        }

                        LocalizationContext ctx = pathManager.getContext(type,
                                level);
                        ctx.setContextName(context);
                        searchContexts.add(ctx);
                    }
                }
            }

            boolean success = false;
            List<LocalizationFile> currentList = new ArrayList<LocalizationFile>();
            LocalizationFile[] files = pathManager.listFiles(searchContexts
                    .toArray(new LocalizationContext[searchContexts.size()]),
                    path, filter, false, !recursive);
            if (files == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error getting list of files");
            } else {
                if (parentItem.getItemCount() == 1
                        && parentItem.getItems()[0].getData() == null) {
                    parentItem.removeAll();
                }

                for (LocalizationFile file : files) {
                    if (checkName
                            && (file.getName().isEmpty() || data.getPath()
                                    .equals(file.getName()))) {
                        continue;
                    }
                    if (file.exists()) {
                        currentList.add(file);
                    }
                }

                // Sort files using specific ordering...
                Collections.sort(currentList, new FileTreeFileComparator());

                if (data instanceof LocalizationFileGroupData) {
                    populateGroupDataNode(parentItem, currentList);
                } else {
                    populateDirectoryDataNode(parentItem, currentList);
                }
                success = true;
            }

            return success;
        } finally {
            setArrowCursor();
        }
    }

    private void populateDirectoryDataNode(TreeItem parentItem,
            List<LocalizationFile> files) {
        FileTreeEntryData data = (FileTreeEntryData) parentItem.getData();
        data.setRequestedChildren(true);
        PathData pd = data.getPathData();
        Map<String, List<LocalizationFile>> processedFiles = new HashMap<String, List<LocalizationFile>>();
        Set<String> processedPaths = new HashSet<String>();
        // we are expanding a folder
        for (LocalizationFile file : files) {
            String name = file.getName();
            if (processedPaths.contains(name) == false) {
                FileTreeEntryData treeData = null;
                if (file.isDirectory()) {
                    treeData = new FileTreeEntryData(pd, name);
                } else {
                    treeData = new LocalizationFileGroupData(pd, name);
                }

                addTreeItem(parentItem, treeData);
                processedPaths.add(name);
            }
            if (file.isDirectory() == false) {
                List<LocalizationFile> entryFiles = processedFiles.get(name);
                if (entryFiles == null) {
                    entryFiles = new ArrayList<LocalizationFile>();
                    processedFiles.put(name, entryFiles);
                }
                entryFiles.add(file);
            }
        }

        TreeItem[] children = parentItem.getItems();
        for (TreeItem child : children) {
            String path = ((FileTreeEntryData) child.getData()).getPath();
            if (processedPaths.contains(path) == false) {
                // File no longer exists, delete
                Object objData = child.getData();
                if (objData instanceof FileTreeEntryData) {
                    IResource rsc = ((FileTreeEntryData) objData).getResource();
                    if (rsc != null) {
                        try {
                            rsc.delete(true, null);
                        } catch (CoreException e) {
                            statusHandler.handle(
                                    Priority.PROBLEM,
                                    "Error deleting resource: "
                                            + e.getLocalizedMessage(), e);
                        }
                    }
                }
                child.dispose();
            } else if (processedFiles.containsKey(path)) {
                populateGroupDataNode(child, processedFiles.get(path));
            }
        }
    }

    private void populateGroupDataNode(TreeItem parentItem,
            List<LocalizationFile> files) {
        List<Object> oldData = new ArrayList<Object>();
        for (TreeItem oldItem : parentItem.getItems()) {
            oldData.add(oldItem.getData());
        }

        parentItem.removeAll();
        LocalizationFileGroupData fData = (LocalizationFileGroupData) parentItem
                .getData();
        fData.setRequestedChildren(true);
        PathData pd = fData.getPathData();
        for (LocalizationFile file : files) {
            FileTreeEntryData treeData = null;
            if (!file.isDirectory()) {
                treeData = new LocalizationFileEntryData(pd, file);
                addTreeItem(parentItem, treeData);
                fData.addChildData((LocalizationFileEntryData) treeData);
            }
        }

        for (Object oldItemData : oldData) {
            if (oldItemData == null) {
                continue;
            }
            LocalizationFileEntryData oldFileData = (LocalizationFileEntryData) oldItemData;
            boolean found = false;
            for (TreeItem item : parentItem.getItems()) {
                LocalizationFileEntryData newData = (LocalizationFileEntryData) item
                        .getData();
                if (newData.getFile().equals(oldFileData.getFile())) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                IResource rsc = oldFileData.getResource();
                if (rsc != null) {
                    try {
                        rsc.delete(true, null);
                    } catch (CoreException e) {
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "Error deleting resource: "
                                        + e.getLocalizedMessage(), e);
                    }
                }
            }
        }
    }

    /**
     * Adds a tree item given the data and the parent item
     * 
     * @param parentItem
     * @param treeData
     */
    private TreeItem addTreeItem(TreeItem parentItem, FileTreeEntryData treeData) {
        String name = treeData.getName();
        LocalizationFile file = null;
        int idx = parentItem.getItemCount();
        if (treeData instanceof LocalizationFileEntryData) {
            file = ((LocalizationFileEntryData) treeData).getFile();
            LocalizationContext ctx = file.getContext();
            LocalizationLevel level = ctx.getLocalizationLevel();
            name = level.toString();
            if (level != LocalizationLevel.BASE) {
                name += " (" + ctx.getContextName() + ")";
            }
        } else {
            List<TreeItem> applicableItems = new ArrayList<TreeItem>();
            int start = -1;
            TreeItem[] children = parentItem.getItems();
            for (int i = 0; i < children.length; ++i) {
                TreeItem item = children[i];
                if (treeData.getClass().equals(item.getData().getClass())) {
                    if (start == -1) {
                        start = i;
                    }
                    applicableItems.add(item);
                }
            }
            start = Math.max(start, 0);

            int insert = getInsertionIndex(
                    applicableItems
                            .toArray(new TreeItem[applicableItems.size()]),
                    treeData.getName());

            if (insert == -1) {
                return null;
            }
            idx = start + insert;
        }

        TreeItem fileItem = new TreeItem(parentItem, SWT.NONE, idx);
        fileItem.setText(name);
        fileItem.setData(treeData);
        if (file != null) {
            fileItem.setImage(getImage(file));
        } else {
            if (treeData instanceof LocalizationFileGroupData) {
                fileItem.setImage(getImage(name));
            } else {
                fileItem.setImage(getImage((String) null));
            }
        }

        if (file == null) {
            new TreeItem(fileItem, SWT.NONE);
        }

        FileTreeEntryData parentData = (FileTreeEntryData) parentItem.getData();
        IFolder folder = (IFolder) parentData.getResource();
        IResource rsc = null;
        if (file != null) {
            rsc = folder.getFile(fileItem.getText() + " - "
                    + parentItem.getText());
        } else {
            rsc = createFolder(folder, fileItem.getText());
        }
        treeData.setResource(rsc);

        return fileItem;
    }

    /**
     * Display the compare editor.
     */
    private void loadCompare(LocalizationFileEntryData left,
            LocalizationFileEntryData right) {
        LocalizationCompareEditorInput editorInput = new LocalizationCompareEditorInput(
                new LocalizationEditorInput(left.getFile(), left.getResource()),
                new LocalizationEditorInput(right.getFile(), right
                        .getResource()));
        CompareUI.openCompareEditor(editorInput);
    }

    /**
     * Get the ImageDescriptor for the provided file name.
     * 
     * @param string
     *            The file name
     * @return The ImageDescriptor
     */
    private ImageDescriptor getImageDescriptor(String string) {
        String iconPath = "icons" + File.separator;
        URL url = FileLocator.find(Activator.getDefault().getBundle(),
                new Path(iconPath + string), null);
        if (url.getFile() == null) {
            url = FileLocator.find(Activator.getDefault().getBundle(),
                    new Path(".." + File.separator + iconPath + string), null);
        }

        return ImageDescriptor.createFromURL(url);
    }

    /**
     * Get the image for the provided LocalizationFile.
     * 
     * @param file
     *            The LocalizationFile
     * @return The image
     */
    private Image getImage(LocalizationFile file) {
        String name = null;
        if (file != null) {
            name = file.getName();
        }
        return getImage(name);
    }

    /**
     * Get the image for the provided path.
     * 
     * @param filePath
     *            The file path
     * @return The image
     */
    private Image getImage(String filePath) {
        if (filePath == null) {
            return dirImg;
        }
        ImageDescriptor desc = LocalizationPerspectiveUtils.getEditorRegistry()
                .getImageDescriptor(filePath);
        if (desc != null) {
            Image img = imageMap.get(desc);
            if (img != null) {
                return img;
            } else {
                img = desc.createImage();
                imageMap.put(desc, img);
            }
            return img;
        } else {
            return dirImg;
        }
    }

    /**
     * Get the path data for the file tree for all open perspectives
     * 
     * @return List of PathData objects
     */
    private List<PathData> getPathData() {
        List<PathData> pathList = new ArrayList<PathData>();
        for (IExtension ext : extensions) {
            IConfigurationElement[] config = ext.getConfigurationElements();
            for (IConfigurationElement element : config) {
                if (element.getName().equals("path")) {
                    PathData pd = new PathData();
                    pd.setName(element.getAttribute("name"));
                    pd.setPath(element.getAttribute("value"));
                    pd.setType(LocalizationType.valueOf(element
                            .getAttribute("localizationType")));
                    if (pd.getType() == null) {
                        // Skip if bad localization type specified
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Skipping path with name: "
                                                + pd.getName()
                                                + " and path: "
                                                + pd.getPath()
                                                + " with invalid localiation type: "
                                                + element
                                                        .getAttribute("localizationType"));
                        continue;
                    }
                    pd.setFilter(element.getAttribute("extensionFilter"));
                    pd.setApplication(element.getAttribute("application"));
                    LocalizationPerspectiveAdapter adapter = DEFAULT_ADAPTER;
                    try {
                        if (element.getAttribute("localizationAdapter") != null) {
                            adapter = (LocalizationPerspectiveAdapter) element
                                    .createExecutableExtension("localizationAdapter");
                        }
                    } catch (Throwable t) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Skipping path with name: "
                                                + pd.getName()
                                                + " and path: "
                                                + pd.getPath()
                                                + " due to error constructing adapter: "
                                                + t.getLocalizedMessage(), t);
                    }
                    pd.setAdapter(adapter);

                    String recurse = element.getAttribute("recursive");
                    if ((recurse != null) && recurse.equalsIgnoreCase("true")) {
                        pd.setRecursive(true);
                    }
                    pd.setElement(element);

                    pathList.add(pd);
                }
            }
        }
        return pathList;
    }

    /**
     * Finds the first TreeItem in the tree which matches the localization file
     * (minus level). If file is a directory, will return TreeItem which
     * corresponds to that directory
     * 
     * @param file
     * @param nullIfNotFound
     *            if true, will return null when file is not found and
     *            populating the node would be required
     * @return
     */
    private TreeItem find(LocalizationFile file, boolean nullIfNotFound) {
        return find(file.getName(), file.getContext(), nullIfNotFound);
    }

    private TreeItem find(String path, LocalizationContext context,
            boolean nullIfNotFound) {
        TreeItem rval = null;
        Tree tree = getTree();
        TreeItem[] items = tree.getItems();
        String[] parts = LocalizationUtil.splitUnique(path);
        for (TreeItem item : items) {
            TreeItem found = find(item, context, parts, 0, nullIfNotFound);
            if (found != null) {
                rval = found;
                break;
            }
        }
        return rval;
    }

    /**
     * Finds the first TreeItem in the application tree node which contains the
     * localization file. If file is a directory, will return TreeItem which
     * corresponds to that directory
     * 
     * @param item
     * @param file
     * @return
     */
    private TreeItem find(TreeItem item, LocalizationContext ctx,
            String[] parts, int index, boolean nullIfNotFound) {
        if ((item.getData() != null)
                && (item.getData() instanceof FileTreeEntryData)) {
            FileTreeEntryData itemData = (FileTreeEntryData) item.getData();
            if (itemData.hasRequestedChildren() == false) {
                if (nullIfNotFound) {
                    return null;
                } else {
                    populateNode(item);
                }
            }
        }

        if (index == parts.length) {
            if (item.getData() instanceof LocalizationFileGroupData) {
                TreeItem[] children = item.getItems();
                for (TreeItem child : children) {
                    if (((LocalizationFileEntryData) child.getData()).getFile()
                            .getContext().equals(ctx)) {
                        return child;
                    }
                }
            }
            return item;
        }

        TreeItem[] children = item.getItems();
        for (TreeItem child : children) {
            FileTreeEntryData data = (FileTreeEntryData) child.getData();
            PathData pd = data.getPathData();
            if (pd.getType() == ctx.getLocalizationType()) {
                // same type, check PathData path
                String[] childParts = new String[] { data.getName() };
                if ((item.getData() instanceof FileTreeEntryData) == false) {
                    childParts = LocalizationUtil.splitUnique(data.getPath());
                }
                boolean equal = true;
                for (int i = 0; (i < childParts.length) && equal; ++i) {
                    if ((i + index >= parts.length)
                            || (parts[i + index].equals(childParts[i]) == false)) {
                        equal = false;
                    }
                }

                if (equal) {
                    if (parts.length == (index + childParts.length)) {
                        return child;
                    } else {
                        TreeItem found = find(child, ctx, parts, index
                                + childParts.length, nullIfNotFound);
                        return found != null ? found : child;
                    }
                }
            }
        }

        return null;
    }

    /**
     * Select the TreeItem corresponding to the open/active editor.
     * 
     * @param partRef
     *            The IWorkbenchPartReference
     */
    private void selectItem(IEditorReference partRef) {
        IEditorInput input = null;
        try {
            input = partRef.getEditorInput();
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM, "Error activating editor: "
                    + e.getLocalizedMessage(), e);
        }
        if (input instanceof LocalizationEditorInput && linkWithEditor) {
            selectFile(((LocalizationEditorInput) input).getLocalizationFile());
        }
    }

    private void setWaitCursor() {
        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
                .setCursor(waitCursor);
    }

    private void setArrowCursor() {
        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
                .setCursor(arrowCursor);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org
     * .eclipse.core.resources.IResourceChangeEvent)
     */
    @Override
    public void resourceChanged(IResourceChangeEvent event) {
        for (IEditorReference ref : getSite().getPage().getEditorReferences()) {
            IEditorInput editorInput = ref.getEditor(false).getEditorInput();
            if (editorInput instanceof LocalizationEditorInput) {
                LocalizationEditorInput input = (LocalizationEditorInput) editorInput;
                IFile inputFile = input.getFile();
                IResourceDelta rootDelta = event.getDelta();
                IResourceDelta docDelta = rootDelta.findMember(inputFile
                        .getFullPath());
                if (docDelta != null
                        && docDelta.getKind() == IResourceDelta.CHANGED
                        && (docDelta.getFlags() & IResourceDelta.CONTENT) == IResourceDelta.CONTENT) {
                    try {
                        LocalizationFile file = input.getLocalizationFile();
                        if (file.getContext().getLocalizationLevel()
                                .isSystemLevel() == false) {
                            input.getLocalizationFile().save();
                        }
                    } catch (LocalizationOpFailedException e) {
                        statusHandler
                                .handle(Priority.PROBLEM, "Error saving file: "
                                        + e.getLocalizedMessage(), e);
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.localization.ILocalizationFileObserver#fileUpdated
     * (com.raytheon.uf.common.localization.FileUpdatedMessage)
     */
    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        if (getTree().isDisposed()) {
            // Make sure we are not disposed
            return;
        }

        LocalizationContext context = message.getContext();

        Set<String> contexts = contextMap.get(context.getLocalizationLevel());
        if (contexts != null) {
            contexts.add(context.getContextName());
        }

        String filePath = message.getFileName();
        IPathManager pathManager = PathManagerFactory.getPathManager();

        final LocalizationFile file = pathManager.getLocalizationFile(context,
                filePath);

        if ((file.exists() == false && (message.getChangeType() == FileChangeType.ADDED || message
                .getChangeType() == FileChangeType.UPDATED))
                || (file.exists() && message.getChangeType() == FileChangeType.DELETED)) {
            System.out.println("Got weird state in update for " + file
                    + ": exists=" + file.exists() + ", changeType="
                    + message.getChangeType());
        }

        if (file != null) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    TreeItem[] selections = getTree().getSelection();
                    List<LocalizationFile> files = new ArrayList<LocalizationFile>();
                    for (TreeItem item : selections) {
                        if (item.getData() instanceof LocalizationFileEntryData) {
                            LocalizationFileEntryData data = (LocalizationFileEntryData) item
                                    .getData();
                            files.add(data.getFile());
                        }
                    }

                    TreeItem toRefresh = find(file, true);
                    if (toRefresh != null) {
                        if (isParentOf(toRefresh, file)) {
                            repopulateTree(toRefresh);
                        }
                    }

                    for (LocalizationFile file : files) {
                        selectFile(file);
                    }
                }
            });
        }
    }

    private boolean isParentOf(TreeItem item, LocalizationFile file) {
        Object obj = item.getData();
        if (obj instanceof FileTreeEntryData) {
            FileTreeEntryData data = (FileTreeEntryData) obj;
            if (data instanceof LocalizationFileGroupData) {
                return true;
            }

            String[] parentParts = LocalizationUtil.splitUnique(data.getPath());
            String[] fileParts = LocalizationUtil.splitUnique(file.getName());
            if (fileParts.length > 1
                    && parentParts.length > 0
                    && fileParts[fileParts.length - 2]
                            .equals(parentParts[parentParts.length - 1])) {
                return true;
            }
        }
        return false;
    }

    /** NO-OP Interface operations (Required to implement) */
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
    }

    /** Editor operations **/
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partActivated(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partActivated(IWorkbenchPartReference partRef) {
        if (partRef instanceof IEditorReference) {
            selectItem((IEditorReference) partRef);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partBroughtToTop(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partBroughtToTop(IWorkbenchPartReference partRef) {
        if (partRef instanceof IEditorReference) {
            selectItem((IEditorReference) partRef);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partOpened(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partOpened(IWorkbenchPartReference partRef) {
        if (partRef instanceof IEditorReference) {
            selectItem((IEditorReference) partRef);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partClosed(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partClosed(IWorkbenchPartReference partRef) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partDeactivated(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partDeactivated(IWorkbenchPartReference partRef) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partHidden(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partHidden(IWorkbenchPartReference partRef) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partVisible(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IPartListener2#partInputChanged(org.eclipse.ui.
     * IWorkbenchPartReference)
     */
    @Override
    public void partInputChanged(IWorkbenchPartReference partRef) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
     */
    @SuppressWarnings("rawtypes")
    @Override
    public Object getAdapter(Class adapter) {
        if (adapter.equals(ILocalizationService.class)) {
            return this;
        }
        return super.getAdapter(adapter);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.localization.service.ILocalizationService#selectFile
     * (com.raytheon.uf.common.localization.LocalizationFile)
     */
    @Override
    public void selectFile(LocalizationFile file) {
        TreeItem item = find(file, false);
        if (item != null) {
            FileTreeEntryData data = (FileTreeEntryData) item.getData();
            if (data.getPath().equals(file.getName())) {
                if (data instanceof LocalizationFileGroupData) {
                    for (TreeItem child : item.getItems()) {
                        LocalizationFileEntryData lfed = (LocalizationFileEntryData) child
                                .getData();
                        if (lfed != null && lfed.getFile().equals(file)) {
                            getTree().setSelection(child);
                            return;
                        }
                    }
                }
                getTree().setSelection(item);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.localization.service.ILocalizationService#
     * refresh(com.raytheon.uf.common.localization.LocalizationFile)
     */
    @Override
    public void refresh(LocalizationFile file) {
        if (file != null) {
            TreeItem item = find(file, false);
            if (item != null) {
                repopulateTree(item);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.localization.service.ILocalizationService#
     * openFile(com.raytheon.uf.common.localization.LocalizationFile)
     */
    @Override
    public void openFile(LocalizationFile file) {
        IWorkbenchPage page = this.getSite().getPage();
        TreeItem item = find(file, true);
        if (item != null) {
            LocalizationFileGroupData groupData = (LocalizationFileGroupData) item
                    .getData();
            IFile data = null;
            for (LocalizationFileEntryData child : groupData.getChildrenData()) {
                if (file.equals(child.getFile())) {
                    data = child.getResource();
                    break;
                }
            }
            if (data != null) {
                LocalizationPerspectiveUtils.openInEditor(page,
                        new LocalizationEditorInput(file, data));
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.localization.service.ILocalizationService#activateEditor
     * (org.eclipse.ui.IEditorPart)
     */
    @Override
    public void activateEditor(IEditorPart editor) {
        selectItem((IEditorReference) (getSite().getPage().getReference(editor)));
    }

    public void toggleShowAllLevel(LocalizationLevel level) {
        if (showAllSet.contains(level)) {
            showAllSet.remove(level);
        } else {
            showAllSet.add(level);
        }
        repopulateTree(getTree());
    }

    public boolean isAllShown(LocalizationLevel level) {
        return showAllSet.contains(level);
    }
}
