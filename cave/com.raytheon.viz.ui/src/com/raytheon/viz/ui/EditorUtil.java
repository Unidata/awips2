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
package com.raytheon.viz.ui;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;

/**
 * Utility functions for editors, need to migrate VizApp editor functions to
 * here. Once done, move VizWorkbenchManager to this project/package
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class EditorUtil {

    private static VizWorkbenchManager vizManager = VizWorkbenchManager
            .getInstance();

    /**
     * Get the active editor from the active window
     * 
     * This method gets the currently active editor regardless of the thread it
     * is called from.
     * 
     * @return the active editor on the active window or null if no editor on
     *         the window
     */
    public static IEditorPart getActiveEditor() {
        return getActiveEditor(vizManager.getCurrentWindow());
    }

    /**
     * Gets the active editor on the window.
     * 
     * @param window
     * @return the active editor or null if no editor
     */
    public static IEditorPart getActiveEditor(IWorkbenchWindow window) {
        return vizManager.getActiveEditor(window);
    }

    /**
     * Get the current editor.
     * 
     * This method gets the currently active IDisplayPaneContainer editor
     * regardless of the thread it is called from.
     * 
     * @return The current editor as an IDisplayPaneContainer or null if current
     *         editor is not an instance of IDisplayPaneContainer or no editors
     *         opened
     */
    public static IDisplayPaneContainer getActiveVizContainer() {
        return getActiveVizContainer(vizManager.getCurrentWindow());
    }

    /**
     * Get the active IDisplayPaneContainer on the passed in window if there is
     * an active editor and it is an IDisplayPaneContainer
     * 
     * @param window
     * @return active editor on window if IDisplayPaneContainer or null if not
     *         instanceof IDisplayPaneContainer or no editors
     */
    public static IDisplayPaneContainer getActiveVizContainer(
            IWorkbenchWindow window) {
        return getActiveEditorAs(window, IDisplayPaneContainer.class);
    }

    /**
     * Get the active editor as the type requested on the currently activated
     * window
     * 
     * @param clazz
     * @return the active editor as the type requested or null if active editor
     *         is not instanceof clazz or no editor
     */
    public static <T extends Object> T getActiveEditorAs(Class<T> clazz) {
        return getActiveEditorAs(vizManager.getCurrentWindow(), clazz);
    }

    /**
     * Get the active editor as the type requested on the specified window
     * 
     * @param window
     * @param clazz
     * @return the active editor as the type requested or null if active editor
     *         is not instanceof clazz or no editor
     */
    @SuppressWarnings("unchecked")
    public static <T extends Object> T getActiveEditorAs(
            IWorkbenchWindow window, Class<T> clazz) {
        IEditorPart part = getActiveEditor(window);
        if (part != null && clazz.isInstance(part)) {
            return (T) part;
        }
        return null;
    }

    /**
     * 
     * @param editorId
     * @return
     */
    public static IEditorPart findEditor(String editorId) {
        // Search to see if editor is available
        IEditorReference[] references = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage()
                .getEditorReferences();
        for (IEditorReference ref : references) {
            if (editorId.equals(ref.getId())) {
                return ref.getEditor(false);
            }
        }
        return null;
    }
}
