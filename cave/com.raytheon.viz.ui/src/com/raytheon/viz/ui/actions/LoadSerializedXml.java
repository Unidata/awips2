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
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
        AbstractEditor editor = UiUtil.createOrOpenEditor(bundleEditorId,
                bundle.getDisplays());

        BundleLoader.loadTo(editor, bundle);
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

                BundleLoader.loadTo(openedEditor, b);
            } else {
                // There is a view part specified
                IViewPart part = UiUtil.findView(windowToLoadTo, b.getView(),
                        false);

                if (part != null && part instanceof IDisplayPaneContainer) {
                    BundleLoader.loadTo((IDisplayPaneContainer) part, b);
                }
            }

        }
    }

    /**
     * Use {@link BundleLoader} instead
     * 
     * @param editor
     *            the container to load to
     * @param b
     *            the bundle
     * @throws VizException
     */
    @Deprecated
    public static void loadTo(final IDisplayPaneContainer container, Bundle b)
            throws VizException {
        new BundleLoader(container, b).run();
    }

}
