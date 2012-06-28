package com.raytheon.viz.ui.personalities.awips;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.eclipse.ui.internal.presentations.SystemMenuClose;
import org.eclipse.ui.internal.presentations.SystemMenuCloseAll;
import org.eclipse.ui.internal.presentations.SystemMenuCloseOthers;
import org.eclipse.ui.internal.presentations.SystemMenuMaximize;
import org.eclipse.ui.internal.presentations.SystemMenuMinimize;
import org.eclipse.ui.internal.presentations.SystemMenuMove;
import org.eclipse.ui.internal.presentations.SystemMenuRestore;
import org.eclipse.ui.internal.presentations.UpdatingActionContributionItem;
import org.eclipse.ui.internal.presentations.util.ISystemMenu;
import org.eclipse.ui.presentations.IPresentablePart;
import org.eclipse.ui.presentations.IStackPresentationSite;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.actions.ContributedEditorMenuAction;
import com.raytheon.viz.ui.editor.AbstractEditor;

public class VizEditorSystemMenu implements ISystemMenu {

    private static final String EDITOR_MENU_EXTENSION_POINT = "com.raytheon.viz.ui.editorMenuAddition";

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VizEditorSystemMenu.class);

    private static class CustomCloseAll extends SystemMenuCloseAll {

        private IStackPresentationSite presentation;

        public CustomCloseAll(IStackPresentationSite presentation) {
            super(presentation);
            this.presentation = presentation;
        }

        public void run() {
            boolean dirty = false;
            List<IEditorReference> editorRefs = new ArrayList<IEditorReference>();
            IWorkbenchPartSite site = EditorUtil.getActiveEditor().getSite();
            for (IEditorReference ref : site.getPage().getEditorReferences()) {
                IEditorPart editor = ref.getEditor(false);
                if (editor instanceof AbstractEditor) {
                    if (!((AbstractEditor) editor).isCloseable()) {
                        site.getPage().closeEditor(editor, true);
                        continue;
                    }
                } else {
                    super.run();
                    return;
                }
                dirty = dirty | ref.isDirty();
                editorRefs.add(ref);
            }
            boolean close = !editorRefs.isEmpty();
            if (dirty) {
                close = MessageDialog.openQuestion(site.getShell(),
                        "Close Editors?",
                        "Are you sure you want to close All editors?");

            }
            if (close) {
                site.getPage().closeEditors(
                        editorRefs.toArray(new IEditorReference[editorRefs
                                .size()]), false);
            }
        }

    }

    private static class CustomCloseOthers extends SystemMenuCloseOthers {

        private IStackPresentationSite presentation;

        public CustomCloseOthers(IStackPresentationSite presentation) {
            super(presentation);
            this.presentation = presentation;
        }

        public void run() {
            boolean dirty = false;
            List<IEditorReference> editorRefs = new ArrayList<IEditorReference>();
            IWorkbenchPartSite site = EditorUtil.getActiveEditor().getSite();
            for (IEditorReference ref : site.getPage().getEditorReferences()) {
                if (ref.getEditor(false) == site.getPage().getActiveEditor()) {
                    continue;
                } else {
                    IEditorPart editor = ref.getEditor(false);
                    if (editor instanceof AbstractEditor) {
                        if (!((AbstractEditor) editor).isCloseable()) {
                            site.getPage().closeEditor(editor, true);
                            continue;
                        }
                    } else {
                        super.run();
                        return;
                    }
                    dirty = dirty | ref.isDirty();
                    editorRefs.add(ref);
                }
            }
            boolean close = !editorRefs.isEmpty();
            if (dirty) {
                close = MessageDialog.openQuestion(site.getShell(),
                        "Close Editors?",
                        "Are you sure you want to close All Other editors?");

            }
            if (close) {
                site.getPage().closeEditors(
                        editorRefs.toArray(new IEditorReference[editorRefs
                                .size()]), false);
            }
        }

    }

    private MenuManager menuManager = new MenuManager();

    private SystemMenuRestore restore;

    private SystemMenuMove move;

    private SystemMenuMinimize minimize;

    private SystemMenuMaximize maximize;

    private SystemMenuClose close;

    private SystemMenuCloseOthers closeOthers;

    private SystemMenuCloseAll closeAll;

    private List<ContributedEditorMenuAction> userContributionActions;

    private ActionFactory.IWorkbenchAction openAgain;

    private Menu lastShown;

    /**
     * Create the standard view menu
     * 
     * @param site
     *            the site to associate the view with
     */
    public VizEditorSystemMenu(IStackPresentationSite site) {
        restore = new SystemMenuRestore(site);
        move = new SystemMenuMove(site, getMoveMenuText(), false);
        minimize = new SystemMenuMinimize(site);
        maximize = new SystemMenuMaximize(site);
        close = new SystemMenuClose(site);

        { // Initialize system menu
            menuManager.add(new GroupMarker("misc")); //$NON-NLS-1$
            menuManager.add(new GroupMarker("restore")); //$NON-NLS-1$
            menuManager.add(new UpdatingActionContributionItem(restore));

            menuManager.add(move);
            menuManager.add(new GroupMarker("size")); //$NON-NLS-1$
            menuManager.add(new GroupMarker("state")); //$NON-NLS-1$
            menuManager.add(new UpdatingActionContributionItem(minimize));

            menuManager.add(new UpdatingActionContributionItem(maximize));
            menuManager.add(new Separator("close")); //$NON-NLS-1$
            menuManager.add(close);

            site.addSystemActions(menuManager);
        } // End of system menu initialization
        closeOthers = new CustomCloseOthers(site);
        closeAll = new CustomCloseAll(site);
        openAgain = ActionFactory.NEW_EDITOR.create(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow());
        menuManager.add(closeOthers);
        menuManager.add(closeAll);
        menuManager.add(new Separator());
        menuManager.add(openAgain);

        // grab any user contributed items using an extension point
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry
                .getExtensionPoint(EDITOR_MENU_EXTENSION_POINT);
        if (point != null) {
            menuManager.add(new Separator());
            userContributionActions = new ArrayList<ContributedEditorMenuAction>();

            for (IExtension ext : point.getExtensions()) {
                IConfigurationElement[] element = ext
                        .getConfigurationElements();
                for (final IConfigurationElement el : element) {
                    Object ob;
                    try {
                        ob = el.createExecutableExtension("class");
                        ContributedEditorMenuAction action = (ContributedEditorMenuAction) ob;
                        String icon = el.getAttribute("icon");
                        if (icon != null) {
                            action.setImageDescriptor(IconUtil
                                    .getImageDescriptor(el.getContributor()
                                            .getName(), icon));
                        }
                        action.setId(el.getAttribute("name"));
                        action.setText(el.getAttribute("name"));
                        userContributionActions.add(action);
                    } catch (CoreException e) {
                        statusHandler.error(
                                "Error creating custom editor menu action", e);
                    }
                }
            }
        }
    }

    String getMoveMenuText() {
        return WorkbenchMessages.EditorPane_moveEditor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.internal.presentations.util.ISystemMenu#show(org.eclipse
     * .swt.graphics.Point, org.eclipse.ui.presentations.IPresentablePart)
     */
    public void show(Control parent, Point displayCoordinates,
            IPresentablePart currentSelection) {
        if (lastShown != null) {
            lastShown.dispose();
        }

        closeOthers.setTarget(currentSelection);
        closeAll.update();
        restore.update();
        move.setTarget(currentSelection);
        move.update();
        minimize.update();
        maximize.update();
        close.setTarget(currentSelection);

        MenuManager toShow = new MenuManager(menuManager.getMenuText(),
                menuManager.getId());
        for (IContributionItem item : menuManager.getItems()) {
            toShow.add(item);
        }

        for (ContributedEditorMenuAction action : userContributionActions) {
            if (action.shouldBeVisible()) {
                action.setPart(currentSelection);
                toShow.add(new ActionContributionItem(action));
            }
        }

        Menu aMenu = lastShown = toShow.createContextMenu(parent);
        toShow.update(true);
        aMenu.setLocation(displayCoordinates.x, displayCoordinates.y);
        aMenu.setVisible(true);
    }

    /**
     * Dispose resources associated with this menu
     */
    public void dispose() {
        openAgain.dispose();
        menuManager.dispose();
        menuManager.removeAll();
    }

}
