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
package com.raytheon.viz.ui.actions;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.internal.EditorAreaHelper;
import org.eclipse.ui.internal.EditorReference;
import org.eclipse.ui.internal.WorkbenchPage;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Handles loading of bundles or procedures
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LoadSerializedXml extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LoadSerializedXml.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setText("Load Displays...");
        fd.setFilterExtensions(new String[] { "*.xml" });
        fd.setFilterPath(System.getProperty("user.home"));
        String retVal = fd.open();

        if (retVal == null) {
            return null;
        }

        String fileName = fd.getFilterPath() + File.separator
                + fd.getFileName();

        Object obj = deserialize(new File(fileName));
        try {
            if (obj != null) {
                if (obj instanceof Procedure) {
                    loadProcedureToScreen((Procedure) obj, false);
                } else if (obj instanceof Bundle) {
                    loadBundle((Bundle) obj);
                }
            }
        } catch (VizException e) {
            String errMsg = "Error occurred during load";
            statusHandler.handle(Priority.CRITICAL, errMsg, e);
        }

        return null;
    }

    public static Object deserialize(File fileName) {
        Object obj = null;
        try {
            obj = SerializationUtil.jaxbUnmarshalFromXmlFile(fileName);
        } catch (Exception e) {
            String errMsg = "Error occurred during xml deserialization";
            statusHandler.handle(Priority.CRITICAL, errMsg, e);
        }
        return obj;
    }

    private void loadBundle(Bundle bundle) throws VizException {
        IRenderableDisplay renderableDisplay = bundle.getDisplays()[0];
        IDescriptor bundleDescriptor = renderableDisplay.getDescriptor();
        String bundleEditorId = DescriptorMap.getEditorId(bundleDescriptor
                .getClass().getName());
        synchronizeDisplays(bundle);
        AbstractEditor editor = UiUtil.createOrOpenEditor(bundleEditorId,
                bundle.getDisplays());

        loadTo(editor, bundle);
    }

    public static void loadProcedureToScreen(Procedure procedure,
            boolean ignorePerspective) throws VizException {
        IWorkbenchWindow windowToLoadTo = VizWorkbenchManager.getInstance()
                .getCurrentWindow();
        String perspective = null;
        try {
            perspective = procedure.getPerspective();
        } catch (Throwable e) {
            e.printStackTrace();
        }
        IWorkbenchPage page = null;
        if (perspective != null && !ignorePerspective) {
            try {
                page = PlatformUI.getWorkbench().showPerspective(perspective,
                        PlatformUI.getWorkbench().getActiveWorkbenchWindow());
            } catch (WorkbenchException e) {
                throw new VizException("Opening perspective failed", e);
            }
        } else {
            page = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage();
        }

        // close existing containers
        for (IEditorReference ref : page.getEditorReferences()) {
            IEditorPart part = ref.getEditor(false);
            if (part != null) {
                page.closeEditor(part, false);
            }
        }

        if (procedure.getLayout() != null) {
            EditorAreaHelper editorArea = ((WorkbenchPage) page)
                    .getEditorPresentation();
            editorArea.restoreState(procedure.getLayout());
        }

        windowToLoadTo = page.getWorkbenchWindow();

        Bundle[] bundles = procedure.getBundles();
        for (Bundle b : bundles) {
            // If an editor is specified, or no view part is specified,
            // assume an editor part
            synchronizeDisplays(b);
            if (b.getView() == null) {
                String editorName = b.getEditor();
                AbstractEditor openedEditor = UiUtil.createEditor(editorName,
                        b.getDisplays());

                if (b.getLayoutId() != null) {
                    EditorAreaHelper editArea = ((WorkbenchPage) page)
                            .getEditorPresentation();
                    for (IEditorReference ref : editArea.getEditors()) {
                        if (ref.getEditor(false) == openedEditor) {
                            page.hideEditor(ref);
                            editArea.addEditor((EditorReference) ref,
                                    b.getLayoutId(), false);
                            page.activate(ref.getPart(false));
                        }
                    }
                }

                loadTo(openedEditor, b);
            } else {
                // There is a view part specified
                IViewPart part = UiUtil.findView(windowToLoadTo, b.getView(),
                        false);

                if (part != null && part instanceof IDisplayPaneContainer) {
                    loadTo((IDisplayPaneContainer) part, b);
                }
            }

        }
    }

    private static void synchronizeDisplays(Bundle b) {
        IDescriptor firstDesc = null;
        for (AbstractRenderableDisplay d : b.getDisplays()) {
            if (firstDesc == null) {
                firstDesc = d.getDescriptor();
            } else {
                d.getDescriptor().synchronizeTimeMatching(firstDesc);
            }
        }
    }

    /**
     * Load a bundle to a container
     * 
     * @param editor
     *            the container to load to
     * @param b
     *            the bundle
     * @throws VizException
     */
    public static void loadTo(final IDisplayPaneContainer container,
            final Bundle b) throws VizException {
        final int containerSize = container.getDisplayPanes().length;
        final boolean multiEditor = container instanceof IMultiPaneEditor;

        if (multiEditor) {
            if (container.getDisplayPanes().length > b.getDisplays().length) {
                VizApp.runSync(new Runnable() {
                    @Override
                    public void run() {
                        while (container.getDisplayPanes().length > b
                                .getDisplays().length) {
                            ((IMultiPaneEditor) container).removePane(container
                                    .getDisplayPanes()[container
                                    .getDisplayPanes().length - 1]);
                        }
                    }
                });
            }

        }
        List<AbstractRenderableDisplay> orderedDisplays = Arrays.asList(b
                .getDisplays());
        IDescriptor firstDesc = orderedDisplays.get(0).getDescriptor();
        if (firstDesc != null && firstDesc.getTimeMatcher() != null) {
            orderedDisplays = firstDesc.getTimeMatcher().getDisplayLoadOrder(
                    orderedDisplays);
        }
        for (AbstractRenderableDisplay d : orderedDisplays) {
            d.getDescriptor().synchronizeTimeMatching(firstDesc);
            ResourceList rl = d.getDescriptor().getResourceList();
            rl.instantiateResources(d.getDescriptor(), true);
        }

        final VizException[] errors = new VizException[1];

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                int i = 0;
                for (AbstractRenderableDisplay d : b.getDisplays()) {
                    if (i >= containerSize && multiEditor) {
                        ((IMultiPaneEditor) container).addPane(d);
                    } else if (i >= containerSize) {
                        errors[0] = new VizException(
                                "Unable to add panes to non IMultiPaneEditor");
                        return;
                    } else {
                        IRenderableDisplay oldDisplay = container
                                .getDisplayPanes()[i].getRenderableDisplay();
                        if (oldDisplay != null && oldDisplay != d) {
                            oldDisplay.dispose();
                        }
                        container.getDisplayPanes()[i].setRenderableDisplay(d);
                        container.getDisplayPanes()[i].resize();
                        container.getDisplayPanes()[i].refresh();
                    }
                    i++;
                }

                if (b.getLoopProperties() != null) {
                    container.setLoopProperties(b.getLoopProperties());
                }

                // if loading to an editor, update the globals
                if (container instanceof IEditorPart) {
                    VizGlobalsManager.getCurrentInstance().updateUI(container);
                }
            }
        });

        if (errors[0] != null) {
            throw errors[0];
        }
    }

    /**
     * Load a bundle from a file into a container
     * 
     * @param editor
     *            the container to load to
     * @param f
     *            the file containing the bundle
     * @param descriptor
     *            Optional: A descriptor that should be used for time matching
     * @throws VizException
     */
    public static void loadTo(File f, Map<String, String> variables)
            throws VizException {
        Bundle b = Bundle.unmarshalBundle(f, variables);

        IRenderableDisplay renderableDisplay = b.getDisplays()[0];
        IDescriptor bundleDescriptor = renderableDisplay.getDescriptor();
        String bundleEditorId = DescriptorMap.getEditorId(bundleDescriptor
                .getClass().getName());
        AbstractEditor editor = UiUtil.createOrOpenEditor(bundleEditorId,
                b.getDisplays());

        loadTo(editor, b);
    }

    public static void loadTo(IDisplayPane pane, File f,
            Map<String, String> variables) throws VizException {

        Bundle b = Bundle.unmarshalBundle(f, variables);

        for (AbstractRenderableDisplay d : b.getDisplays()) {
            ResourceList rl = d.getDescriptor().getResourceList();
            rl.instantiateResources(d.getDescriptor(), true);

            pane.setRenderableDisplay(d);
            pane.resize();
            pane.refresh();
        }
    }
}
