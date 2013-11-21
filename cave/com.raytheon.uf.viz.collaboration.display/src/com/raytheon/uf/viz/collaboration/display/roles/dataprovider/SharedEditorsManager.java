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
package com.raytheon.uf.viz.collaboration.display.roles.dataprovider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.display.Activator;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer;
import com.raytheon.uf.viz.collaboration.display.editor.ActivateRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.editor.CreateRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.editor.DisposeRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.editor.RemoteDisplayRequested;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.rsc.DataProviderRscData;
import com.raytheon.uf.viz.collaboration.display.rsc.CollaborationWrapperResource;
import com.raytheon.uf.viz.collaboration.display.rsc.CollaborationWrapperResourceData;
import com.raytheon.uf.viz.collaboration.display.rsc.SelfAddingSystemResourceListener;
import com.raytheon.uf.viz.collaboration.display.rsc.event.SharedResource;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatcherFactory;
import com.raytheon.uf.viz.remote.graphics.DispatchingGraphicsFactory;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Manager class for managing the sharing of editors in an
 * {@link ISharedDisplaySession}
 * 
 * TODO: Handle DataProviderRsc adding/removing! Maybe put in wrap/unwrap
 * resource?
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SharedEditorsManager implements IRemoteDisplayContainer {

    private final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    public class RemoteDisplayEventHandler {
        @Subscribe
        public void remoteDisplayRequested(RemoteDisplayRequested event) {
            String userId = event.getUserId();
            UserId user = null;
            for (UserId uid : session.getVenue().getParticipants()) {
                if (uid.getFQName().equals(userId)) {
                    user = uid;
                    break;
                }
            }
            if (user != null) {
                int displayId = event.getDisplayId();
                RemoteDisplay requested = null;
                for (DisplayData data : displayData.values()) {
                    if (data.displayId == displayId) {
                        requested = new RemoteDisplay(data.displayId,
                                data.display);
                        break;
                    }
                }
                if (requested == null) {
                    requested = getActiveDisplay();
                    if (requested != null) {
                        ActivateRemoteDisplay activate = new ActivateRemoteDisplay();
                        activate.setDisplayId(requested.getDisplayId());
                        sendEvent(activate);
                    }
                } else {
                    CreateRemoteDisplay creation = new CreateRemoteDisplay();
                    creation.setDisplayId(requested.getDisplayId());
                    creation.setDisplay(createRemoteDisplay(requested
                            .getDisplay()));
                    sendEvent(creation);
                }
            }
        }
    }

    private class SharedEditorDisplayChangedListener implements
            IRenderableDisplayChangedListener {

        private AbstractEditor sharedEditor;

        private SharedEditorDisplayChangedListener(AbstractEditor sharedEditor) {
            this.sharedEditor = sharedEditor;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.IRenderableDisplayChangedListener#
         * renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane,
         * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
         * com.raytheon.uf
         * .viz.core.IRenderableDisplayChangedListener.DisplayChangeType)
         */
        @Override
        public void renderableDisplayChanged(IDisplayPane pane,
                IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
            if (type == DisplayChangeType.ADD) {
                try {
                    if (displayData.containsKey(newRenderableDisplay) == false) {
                        // We aren't sharing this display, but is anyone else?
                        if (isBeingShared(newRenderableDisplay)) {
                            // if so, remove it from their session
                            removeDisplay(pane);
                        }
                        shareDisplay(pane);
                    }
                    if (activeSharedEditor == sharedEditor) {
                        activeSharedEditor = null;
                        activateEditor(sharedEditor);
                    }
                } catch (CollaborationException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }

    }

    private class UnsharedEditorDisplayChangedListener implements
            IRenderableDisplayChangedListener {

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.IRenderableDisplayChangedListener#
         * renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane,
         * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
         * com.raytheon.uf
         * .viz.core.IRenderableDisplayChangedListener.DisplayChangeType)
         */
        @Override
        public void renderableDisplayChanged(IDisplayPane pane,
                IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
            // An unshared editor had the renderable display changed, check if
            // newRenderableDisplay is being shared by a manager
            if (type == DisplayChangeType.ADD
                    && displayData.containsKey(newRenderableDisplay)) {
                AbstractEditor editor = (AbstractEditor) newRenderableDisplay
                        .getContainer();
                if (allSharedEditors.contains(editor) == false) {
                    try {
                        shareEditor(editor);
                    } catch (CollaborationException e) {
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        }

    }

    private class SharedEditorPartListener implements IPartListener {

        private List<IWorkbenchPage> pages = new LinkedList<IWorkbenchPage>();

        private void addPage(IWorkbenchPage page) {
            if (pages.contains(page) == false) {
                for (IEditorReference ref : page.getEditorReferences()) {
                    partOpened(ref.getEditor(false));
                }
                page.addPartListener(this);
                pages.add(page);
            }
        }

        private void removePage(IWorkbenchPage page) {
            if (pages.contains(page)) {
                // We are listening on this page, should we be?
                for (AbstractEditor editor : sharedEditors) {
                    if (editor.getSite().getPage() == page) {
                        // Another shared editor is on this page, keep listening
                        return;
                    }
                }
                // Made it here, we should not be listening anymore
                page.removePartListener(this);
                pages.remove(page);
            }
        }

        @Override
        public void partActivated(IWorkbenchPart part) {
            if (sharedEditors.contains(part)) {
                activateEditor((AbstractEditor) part);
            } else if (part instanceof IEditorPart) {
                deactivateEditors();
            }
        }

        @Override
        public void partClosed(IWorkbenchPart part) {
            if (sharedEditors.contains(part)) {
                try {
                    removeEditor((AbstractEditor) part);
                } catch (CollaborationException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }

        @Override
        public void partDeactivated(IWorkbenchPart part) {

        }

        @Override
        public void partOpened(IWorkbenchPart part) {
            if (part instanceof AbstractEditor) {
                AbstractEditor editor = (AbstractEditor) part;
                editor.addRenderableDisplayChangedListener(unsharedEditorListener);
            }
        }

        @Override
        public void partBroughtToTop(IWorkbenchPart part) {
            if (activeSharedEditor == null && sharedEditors.contains(part)) {
                activateEditor((AbstractEditor) part);
            }
        }
    }

    private class SharedEditorDispatcherFactory implements DispatcherFactory {
        @Override
        public Dispatcher createNewDispatcher(IRenderableDisplay display)
                throws InstantiationException {
            try {
                CollaborationDispatcher dispatcher = new CollaborationDispatcher(
                        session, display);
                DisplayData data = new DisplayData();
                data.display = display;
                data.dispatcher = dispatcher;
                data.displayId = dispatcher.getDispatcherId();
                data.resourceListener = new SharedEditorResourceWrapperListener(
                        display, data.displayId);
                displayData.put(display, data);
                return dispatcher;
            } catch (Exception e) {
                InstantiationException ie = new InstantiationException(
                        "Error creating new dispatcher: "
                                + e.getLocalizedMessage());
                ie.initCause(e);
                throw ie;
            }
        }
    }

    private class SharedEditorResourceWrapperListener extends
            SelfAddingSystemResourceListener implements AddListener {

        private int displayId;

        private SharedEditorResourceWrapperListener(IRenderableDisplay display,
                int displayId) throws VizException {
            super(new DataProviderRscData(session.getSessionId(), displayId),
                    display.getDescriptor());
            this.displayId = displayId;
            this.descriptor.getResourceList().addPostAddListener(this);
        }

        @Override
        public void notifyAdd(ResourcePair rp) throws VizException {
            wrapResourcePair(rp);
            if (shouldBeLocal(rp)) {
                sendSharedResource(displayId, rp, false);
            }
        }

        @Override
        public void notifyRemove(ResourcePair rp) throws VizException {
            super.notifyRemove(rp);
            if (rp.getResource() instanceof CollaborationWrapperResource) {
                // Send event to venue to unload
                sendSharedResource(displayId, rp, true);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.collaboration.display.rsc.
         * SelfAddingSystemResourceListener#dispose()
         */
        @Override
        public void dispose() {
            super.dispose();
            this.descriptor.getResourceList().removePostAddListener(this);
        }

    }

    private static class DisplayData {
        private SharedEditorResourceWrapperListener resourceListener;

        private CollaborationDispatcher dispatcher;

        private IRenderableDisplay display;

        private int displayId;
    }

    private static Map<String, SharedEditorsManager> managerMap = new HashMap<String, SharedEditorsManager>();

    private static List<AbstractEditor> allSharedEditors = new LinkedList<AbstractEditor>();

    /**
     * Gets/Creates the SharedEditorManager for the given session
     * 
     * @param session
     * @return
     */
    public static SharedEditorsManager getManager(ISharedDisplaySession session) {
        SharedEditorsManager manager = managerMap.get(session.getSessionId());
        if (manager == null) {
            manager = new SharedEditorsManager(session);
            managerMap.put(session.getSessionId(), manager);
        }
        return manager;
    }

    /**
     * 
     * @param editor
     * @return
     */
    public static ISharedDisplaySession getSharedEditorSession(
            AbstractEditor editor) {
        for (String sessionId : managerMap.keySet()) {
            SharedEditorsManager manager = managerMap.get(sessionId);
            if (manager.sharedEditors.contains(editor)) {
                return manager.session;
            }
        }
        return null;
    }

    /**
     * Returns the shared display id for the renderable display. Returns -1 if
     * display not shared
     * 
     * @param display
     * @return
     */
    public static int getSharedDisplayId(IRenderableDisplay display) {
        for (SharedEditorsManager manager : managerMap.values()) {
            if (manager.displayData.containsKey(display)) {
                return manager.displayData.get(display).displayId;
            }
        }
        return -1;
    }

    public static boolean isBeingShared(AbstractEditor editor) {
        return allSharedEditors.contains(editor);
    }

    public static boolean isBeingShared(IRenderableDisplay display) {
        return getSharedDisplayId(display) != -1;
    }

    private DispatcherFactory factory = new SharedEditorDispatcherFactory();

    private SharedEditorPartListener partListener = new SharedEditorPartListener();

    private Map<AbstractEditor, SharedEditorDisplayChangedListener> listenerMap = new IdentityHashMap<AbstractEditor, SharedEditorsManager.SharedEditorDisplayChangedListener>();

    private List<AbstractEditor> sharedEditors = new LinkedList<AbstractEditor>();

    private IRenderableDisplayChangedListener unsharedEditorListener = new UnsharedEditorDisplayChangedListener();

    private Map<IRenderableDisplay, DisplayData> displayData = new IdentityHashMap<IRenderableDisplay, DisplayData>();

    private Set<IRemoteDisplayChangedListener> listeners = new LinkedHashSet<IRemoteDisplayChangedListener>();

    private SharedEditorsManagerListenerHandler sharedEditorsManagerListenerHandler = new SharedEditorsManagerListenerHandler();

    private RemoteDisplayEventHandler eventHandler = new RemoteDisplayEventHandler();

    private AbstractEditor activeSharedEditor;

    private String editorTitleSuffix;

    private ISharedDisplaySession session;

    private SharedEditorsManager(ISharedDisplaySession session) {
        this.session = session;
        session.registerEventHandler(eventHandler);
        String title;
        try {
        IVenueInfo info = session.getVenue().getInfo();
            title = info.getVenueDescription();
        } catch (CollaborationException e) {
            log.error("Unable to get venue information", e);
            title = session.getVenue().getName();
        }
        editorTitleSuffix = " (" + title + ")";
    }

    public int getDisplayId(IRenderableDisplay display) {
        int displayId = -1;
        DisplayData data = displayData.get(display);
        if (data != null) {
            displayId = data.displayId;
        }
        return displayId;
    }

    public AbstractEditor getActiveSharedEditor() {
        return activeSharedEditor;
    }

    public List<AbstractEditor> getSharedEditors() {
        return new ArrayList<AbstractEditor>(sharedEditors);
    }

    /**
     * Shares the AbstractEditor for the session. Throws exception if editor is
     * already being shared
     * 
     * @param editor
     * @throws CollaborationException
     */
    public void shareEditor(AbstractEditor editor)
            throws CollaborationException {
        if (sharedEditors.contains(editor) == false) {
            if (allSharedEditors.contains(sharedEditors) == false) {
                // Add to tracking lists
                sharedEditors.add(editor);
                allSharedEditors.add(editor);

                editor.removeRenderableDisplayChangedListener(unsharedEditorListener);

                try {
                    for (IDisplayPane pane : editor.getDisplayPanes()) {
                        shareDisplay(pane);
                    }
                } catch (CollaborationException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            "Error sharing editor: " + e.getLocalizedMessage(),
                            e);
                    removeEditor(editor);
                    return;
                }

                // Add and manage display listener
                SharedEditorDisplayChangedListener listener = new SharedEditorDisplayChangedListener(
                        editor);
                editor.addRenderableDisplayChangedListener(listener);
                listenerMap.put(editor, listener);

                // Update tab title
                editor.setTabTitle(editor.getPartName() + editorTitleSuffix);

                partListener.addPage(editor.getSite().getPage());

                // If editor is currently active, activate it!
                if (EditorUtil.getActiveEditor(editor.getSite()
                        .getWorkbenchWindow()) == editor) {
                    activateEditor(editor);
                }
                sharedEditorsManagerListenerHandler.fireEditorShared(editor);
            } else {
                throw new CollaborationException(
                        "Cannot shared editor already being shared with another session");
            }
        } else {
            System.out.println("Unable to share editor!");
            Thread.dumpStack();
            throw new CollaborationException(
                    "Cannot share editor, it is already being shared with the session");
        }
    }

    /**
     * Removes the editor from being shared in the session. Throws exception if
     * editor is not being shared by this session
     * 
     * @param editor
     * @throws CollaborationException
     */
    public void removeEditor(AbstractEditor editor)
            throws CollaborationException {
        if (sharedEditors.contains(editor)) {
            if (activeSharedEditor == editor) {
                activeSharedEditor = null;
            }

            // Remove renderable display listener
            editor.removeRenderableDisplayChangedListener(listenerMap
                    .remove(editor));
            // Update tab title
            String name = editor.getPartName();
            int endIndex = (name.length() < editorTitleSuffix.length()) ? name
                    .length() : name.length() - editorTitleSuffix.length();
            editor.setTabTitle(name.substring(0, endIndex));

            sharedEditors.remove(editor);
            partListener.removePage(editor.getSite().getPage());

            for (IDisplayPane pane : editor.getDisplayPanes()) {
                removeDisplay(pane);
            }

            // This will trigger the editor to have the unshared editor listener
            // added and support swapping in
            partListener.partOpened(editor);
            allSharedEditors.remove(editor);

            sharedEditorsManagerListenerHandler.fireEditorRemoved(editor);
        } else {
            throw new CollaborationException(
                    "Cannot unshare editor that isn't being shared by session");
        }
    }

    @Override
    public void disposeContainer() {
        // Copy because remove will be called in the loop and we don't want
        // concurrent modification.
        List<AbstractEditor> copy = new ArrayList<AbstractEditor>(sharedEditors);
        for (AbstractEditor editor : copy) {
            try {
                removeEditor(editor);
            } catch (CollaborationException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }

        // Clean up any dispatchers not removed from editors
        // Copy because remove will be called in the loop and we don't want
        // concurrent modification.
        Collection<DisplayData> data = new ArrayList<DisplayData>(
                this.displayData.values());
        for (DisplayData d : data) {
            IRenderableDisplay display = d.display;
            IDisplayPaneContainer container = display.getContainer();
            for (IDisplayPane pane : container.getDisplayPanes()) {
                if (pane.getRenderableDisplay() == display) {
                    removeDisplay(pane);
                    break;
                }
            }
        }

        session.unregisterEventHandler(eventHandler);
        managerMap.remove(session.getSessionId());
    }

    /**
     * Shares the display on the pane with the venue by creating a remote object
     * representation for the participants, wrapping the local resources and
     * injecting the remote target functionality. <b>Should only be called if
     * the display on the pane is not already being shared</b>
     * 
     * @param pane
     * @throws CollaborationException
     */
    private void shareDisplay(IDisplayPane pane) throws CollaborationException {
        IRenderableDisplay display = pane.getRenderableDisplay();
        if (displayData.containsKey(display) == false
                && display instanceof AbstractRenderableDisplay) {
            // Clone the display
            AbstractRenderableDisplay clonedDisplay = createRemoteDisplay(display);
            if (clonedDisplay != null) {
                wrapDisplay(display);
                // Inject with remote target, have to find pane
                try {
                    DispatchingGraphicsFactory.injectRemoteFunctionality(pane,
                            factory);
                } catch (InstantiationException e) {
                    throw new CollaborationException(
                            "Error injecting remote functionality", e);
                }
                DisplayData data = displayData.get(display);
                if (data != null) {
                    fireListeners(data, RemoteDisplayChangeType.CREATED);
                    CreateRemoteDisplay event = new CreateRemoteDisplay();
                    event.setDisplayId(data.displayId);
                    event.setDisplay(clonedDisplay);
                    sendEvent(event);
                } else {
                    throw new CollaborationException(
                            "Remote functionality injection failed");
                }
            }
        }
    }

    /**
     * Removes the shared display capability from the display pane
     * 
     * @param pane
     */
    private void removeDisplay(IDisplayPane pane) {
        IRenderableDisplay display = pane.getRenderableDisplay();
        unwrapDisplay(display);
        DispatchingGraphicsFactory.extractRemoteFunctionality(pane);
        DisplayData data = displayData.remove(display);
        if (data != null) {
            fireListeners(data, RemoteDisplayChangeType.DISPOSED);
            data.dispatcher.dispose();
            data.resourceListener.dispose();
            DisposeRemoteDisplay event = new DisposeRemoteDisplay();
            event.setDisplayId(data.displayId);
            sendEvent(event);
        }
    }

    /**
     * Creates a copy of the realDisplay object that can be used to send a
     * {@link CreateRemoteDisplay} event. Does not matter if display is
     * currently shared or not
     * 
     * @param realDisplay
     * @return
     */
    private AbstractRenderableDisplay createRemoteDisplay(
            IRenderableDisplay realDisplay) {
        AbstractRenderableDisplay clonedDisplay = null;
        if (realDisplay instanceof AbstractRenderableDisplay) {
            clonedDisplay = ((AbstractRenderableDisplay) realDisplay)
                    .cloneDisplay();
            ResourceList clonedList = clonedDisplay.getDescriptor()
                    .getResourceList();
            clonedList.clear();
            List<ResourcePair> toKeep = new ArrayList<ResourcePair>();
            for (ResourcePair rp : realDisplay.getDescriptor()
                    .getResourceList()) {
                if (shouldBeLocal(rp)) {
                    AbstractResourceData resourceData = rp.getResourceData();
                    if (resourceData != null) {
                        if (resourceData instanceof CollaborationWrapperResourceData) {
                            resourceData = ((CollaborationWrapperResourceData) resourceData)
                                    .getWrappedResourceData();
                        }

                        // Copy ResourcePair for our remote display
                        ResourcePair copy = new ResourcePair();
                        copy.setLoadProperties(rp.getLoadProperties());
                        copy.setProperties(rp.getProperties());
                        copy.setResourceData(resourceData);
                        toKeep.add(copy);
                    }
                }
            }
            clonedList.addAll(toKeep);
        }
        return clonedDisplay;
    }

    /**
     * Wraps the resources that will not be shared and drawn locally.
     * 
     * @param display
     */
    private void wrapDisplay(IRenderableDisplay display) {
        ResourceList list = display.getDescriptor().getResourceList();
        for (ResourcePair rp : list) {
            wrapResourcePair(rp);
        }
    }

    /**
     * Unwraps the local wrapped resources from the display.
     * 
     * @param display
     */
    private void unwrapDisplay(IRenderableDisplay display) {
        ResourceList list = display.getDescriptor().getResourceList();
        for (ResourcePair rp : list) {
            unwrapResourcePair(rp);
        }
    }

    private boolean wrapResourcePair(ResourcePair rp) {
        boolean wrapped = false;
        if (shouldBeLocal(rp)
                && rp.getResource() instanceof CollaborationWrapperResource == false
                && rp.getResourceData() instanceof CollaborationWrapperResourceData == false) {
            AbstractResourceData resourceData = rp.getResourceData();
            // Wrap resource data with wrapper object
            CollaborationWrapperResourceData wrapperRscData = new CollaborationWrapperResourceData();
            wrapperRscData.setSession(session);
            wrapperRscData.setWrappedResourceData(resourceData);
            rp.setResourceData(wrapperRscData);
            if (rp.getResource() != null) {
                // If resource is set in pair, set here too
                rp.setResource(new CollaborationWrapperResource(wrapperRscData,
                        rp.getLoadProperties(), rp.getResource()));
            }
            wrapped = true;
        }
        return wrapped;
    }

    private boolean unwrapResourcePair(ResourcePair rp) {
        boolean wasWrapped = false;
        if (rp.getResourceData() instanceof CollaborationWrapperResourceData
                || rp.getResource() instanceof CollaborationWrapperResource) {
            if (rp.getResource() instanceof CollaborationWrapperResource) {
                rp.setResource(((CollaborationWrapperResource) rp.getResource())
                        .getWrappedResource());
                rp.setResourceData(rp.getResource().getResourceData());
            } else {
                rp.setResourceData(((CollaborationWrapperResourceData) rp
                        .getResourceData()).getWrappedResourceData());
            }
            wasWrapped = true;
        }
        return wasWrapped;
    }

    /**
     * Determines if the {@link ResourcePair} should be kept as a locally drawn
     * resource or not
     * 
     * @param rp
     * @return
     */
    private boolean shouldBeLocal(ResourcePair rp) {
        return rp.getProperties().isMapLayer();
    }

    /**
     * Sends an event to the venue marking the active display on the editor as
     * the actively painting display
     * 
     * @param editor
     */
    private void activateEditor(AbstractEditor editor) {
        if (activeSharedEditor != editor) {
            activeSharedEditor = editor;
            IDisplayPane pane = editor.getActiveDisplayPane();
            IRenderableDisplay display = pane.getRenderableDisplay();
            DisplayData data = displayData.get(display);
            if (data != null) {
                for (DisplayData d : displayData.values()) {
                    d.dispatcher.setActiveDisplay(display);
                }
                fireListeners(data, RemoteDisplayChangeType.ACTIVATED);
                ActivateRemoteDisplay event = new ActivateRemoteDisplay();
                event.setDisplayId(data.displayId);
                sendEvent(event);
            }
        }
    }

    /**
     * Sends an event to the venue indicating that no shared displaysa re
     * currently active.
     * 
     * @param editor
     */
    private void deactivateEditors() {
        if (activeSharedEditor != null) {
            activeSharedEditor = null;
            for (DisplayData d : displayData.values()) {
                d.dispatcher.setActiveDisplay(null);
            }
            fireListeners(null, RemoteDisplayChangeType.ACTIVATED);
            ActivateRemoteDisplay event = new ActivateRemoteDisplay();
            event.setDisplayId(-1);
            sendEvent(event);
        }
    }

    /**
     * Sends a shared resource event
     * 
     * @param rp
     *            resource to send
     * @param remove
     *            whether the resource should be added or removed by the
     *            participants
     */
    private void sendSharedResource(int displayId, ResourcePair rp,
            boolean remove) {
        ResourcePair copy = new ResourcePair();
        copy.setLoadProperties(rp.getLoadProperties());
        copy.setProperties(rp.getProperties().clone());
        AbstractResourceData resourceData = rp.getResourceData();
        if (resourceData instanceof CollaborationWrapperResourceData) {
            resourceData = ((CollaborationWrapperResourceData) resourceData)
                    .getWrappedResourceData();
        } else if (rp.getResource() instanceof CollaborationWrapperResource) {
            copy.setResourceData(rp.getResource().getResourceData());
        }
        copy.setResourceData(resourceData);
        SharedResource resource = new SharedResource();
        resource.setDisplayId(displayId);
        resource.setRemoveResource(remove);
        resource.setResource(rp);
        sendEvent(resource);
    }

    /**
     * Sends an object to the venue
     * 
     * @param event
     */
    private void sendEvent(Object event) {
        try {
            session.sendObjectToVenue(event);
        } catch (CollaborationException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    private void fireListeners(DisplayData data, RemoteDisplayChangeType type) {
        RemoteDisplay rd = null;
        if (data != null) {
            rd = new RemoteDisplay(data.displayId, data.display);
        }
        for (IRemoteDisplayChangedListener listener : listeners) {
            listener.remoteDisplayChanged(rd, type);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer#
     * addRemoteDisplayChangedListener
     * (com.raytheon.uf.viz.collaboration.display.
     * IRemoteDisplayContainer.IRemoteDisplayChangedListener)
     */
    @Override
    public void addRemoteDisplayChangedListener(
            IRemoteDisplayChangedListener listener) {
        listeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer#
     * removeRemoteDisplayChangedListener
     * (com.raytheon.uf.viz.collaboration.display
     * .IRemoteDisplayContainer.IRemoteDisplayChangedListener)
     */
    @Override
    public void removeRemoteDisplayChangedListener(
            IRemoteDisplayChangedListener listener) {
        listeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer#
     * getActiveDisplay()
     */
    @Override
    public RemoteDisplay getActiveDisplay() {
        if (activeSharedEditor == null) {
            return null;
        }
        DisplayData data = displayData.get(activeSharedEditor
                .getActiveDisplayPane().getRenderableDisplay());
        return new RemoteDisplay(data.displayId, data.display);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer#
     * getActiveDisplayEditor()
     */
    @Override
    public IEditorPart getActiveDisplayEditor() {
        return activeSharedEditor;
    }

    public void addListener(ISharedEditorsManagerListener listener) {
        sharedEditorsManagerListenerHandler.addListener(listener);
    }

    public void removeListener(ISharedEditorsManagerListener listener) {
        sharedEditorsManagerListenerHandler.removeListener(listener);
    }
}
