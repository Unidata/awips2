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
package com.raytheon.uf.viz.core.globals;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;

/**
 * Single frame count manager, updates the gui via eclipse command
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2009            mschenke    Initial creation
 * Jul 16, 2013 2158       bsteffen    Allow VizGlobalsManager to work without
 *                                     accessing UI thread.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VizGlobalsManager {

    private static Map<IWorkbenchWindow, VizGlobalsManager> instanceMap = new HashMap<IWorkbenchWindow, VizGlobalsManager>();

    private Map<String, Object> globals;

    private IWorkbenchWindow window;

    private static Map<String, List<IGlobalChangedListener>> listeners = new HashMap<String, List<IGlobalChangedListener>>();

    private static WorkbenchWindowListener windowListener = new WorkbenchWindowListener();

    private VizGlobalsManager(IWorkbenchWindow window) {
        globals = new HashMap<String, Object>();
        globals.put(VizConstants.FRAMES_ID, new Integer(12));
        globals.put(VizConstants.SCALE_ID, "CONUS");
        globals.put(VizConstants.DENSITY_ID, new Double(1.0));
        globals.put(VizConstants.MAGNIFICATION_ID, new Double(1.0));
        this.window = window;
    }

    public static VizGlobalsManager getCurrentInstance() {
        IWorkbenchWindow window = null;
        if (PlatformUI.isWorkbenchRunning()) {
            // This returns null when used off the UI thread.
            window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
            if (window == null) {
                window = windowListener.getActiveWindow();
            }
        }
        return getInstance(window);
    }

    public static synchronized VizGlobalsManager getInstance(
            IWorkbenchWindow window) {
        VizGlobalsManager instance = instanceMap.get(window);
        if (instance == null) {
            instance = new VizGlobalsManager(window);
            instanceMap.put(window, instance);
        }
        return instance;
    }

    /**
     * Must be called once during workbench startup to enable using globals for
     * the active window off the UI thread.
     * 
     * @param workbench
     */
    public static void startForWorkbench(IWorkbench workbench) {
        workbench.addWindowListener(windowListener);
        IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
        if (window != null) {
            windowListener.windowOpened(window);
        }
    }

    public synchronized Map<String, Object> cloneGlobals() {
        return new HashMap<String, Object>(globals);
    }

    public synchronized Object getPropery(String key) {
        return globals.get(key);
    }

    public static void addListener(String key, IGlobalChangedListener listener) {
        List<IGlobalChangedListener> list = listeners.get(key);
        if (list == null) {
            list = new ArrayList<IGlobalChangedListener>();
            listeners.put(key, list);
        }
        list.add(listener);
    }

    public static void removeListener(String key,
            IGlobalChangedListener listener) {
        List<IGlobalChangedListener> list = listeners.get(key);
        if (list != null) {
            list.remove(listener);
        }
    }

    public synchronized void updateChanges(Map<String, Object> globalMap) {
        for (String key : globalMap.keySet()) {
            Object newValue = globalMap.get(key);
            updateChange(key, newValue);
        }
    }

    public synchronized void updateChange(String key, Object newValue) {
        Object oldValue = globals.get(key);
        if (newValue != null && newValue.equals(oldValue) == false) {
            globals.put(key, newValue);
            fireListeners(key, newValue);
        }
    }

    private void fireListeners(String key, Object value) {
        List<IGlobalChangedListener> list = listeners.get(key);
        if (list != null) {
            for (IGlobalChangedListener listener : list) {
                listener.updateValue(window, value);
            }
        }
    }

    /**
     * Update the globals UI with the editor's active display pane
     * 
     * @param editor
     */
    public void updateUI(IDisplayPaneContainer editor) {
        if (editor != null && editor.getActiveDisplayPane() != null) {
            updateUI(editor, editor.getActiveDisplayPane()
                    .getRenderableDisplay());
        }
    }

    /**
     * Update the UI with the display for the editor
     * 
     * @param editor
     * @param display
     */
    public void updateUI(IDisplayPaneContainer editor,
            IRenderableDisplay display) {
        if (window != null) {
            // Check the active editor on the window
            IWorkbenchPage page = window.getActivePage();
            if (page != null && page.getActiveEditor() != editor) {
                return;
            }
        }
        if (editor != null && display != null) {
            Map<String, Object> globals = display.getGlobalsMap();
            LoopProperties props = editor.getLoopProperties();
            Boolean looping = false;
            if (props != null) {
                looping = props.isLooping();
            }
            globals.put(VizConstants.LOOPING_ID, looping);
            updateChanges(globals);
        }
    }

    /**
     * Listener for tracking the active window when not on the UI thread.
     */
    private static class WorkbenchWindowListener implements IWindowListener {

        private IWorkbenchWindow activeWindow = null;

        public IWorkbenchWindow getActiveWindow() {
            return activeWindow;
        }

        @Override
        public void windowActivated(IWorkbenchWindow window) {
            activeWindow = window;
        }

        @Override
        public void windowDeactivated(IWorkbenchWindow window) {
            ;
        }

        @Override
        public void windowClosed(IWorkbenchWindow window) {
            if (window == activeWindow) {
                activeWindow = null;
            }
        }

        @Override
        public void windowOpened(IWorkbenchWindow window) {
            activeWindow = window;
        }

    }
}
