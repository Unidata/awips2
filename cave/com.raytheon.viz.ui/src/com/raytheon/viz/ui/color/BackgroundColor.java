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
package com.raytheon.viz.ui.color;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;

public class BackgroundColor {

    private static Map<Object, BackgroundColor> instanceMap = new HashMap<Object, BackgroundColor>();

    public static synchronized BackgroundColor getActivePerspectiveInstance() {
        final IWorkbenchWindow[] currentWindow = new IWorkbenchWindow[1];
        currentWindow[0] = VizWorkbenchManager.getInstance().getCurrentWindow();
        if (currentWindow[0] == null) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    currentWindow[0] = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow();
                }

            });
        }
        for (IWorkbenchPage page : currentWindow[0].getPages()) {
            if (page != null) {
                return getInstance(page.getPerspective());
            }
        }

        UFStatus.getHandler()
                .handle(Priority.INFO,
                        "Could not get the active perspective background color instance, returning default instance");
        return getInstance(null);
    }

    /**
     * Get an instance of the background color object for the specified key
     * 
     * @param bgColorKey
     * @return
     */
    public static synchronized BackgroundColor getInstance(Object bgColorKey) {
        BackgroundColor instance = instanceMap.get(bgColorKey);
        if (instance == null) {
            instance = new BackgroundColor();
            instanceMap.put(bgColorKey, instance);
        }
        return instance;
    }

    private Map<BGColorMode, RGB> colorMap = new HashMap<BGColorMode, RGB>();

    private Map<BGColorMode, Set<IBackgroundColorChangedListener>> listeners = new HashMap<BGColorMode, Set<IBackgroundColorChangedListener>>();

    private BackgroundColor() {
        colorMap.put(BGColorMode.GLOBAL, new RGB(0, 0, 0));
        colorMap.put(BGColorMode.EDITOR, new RGB(0, 0, 0));
    }

    public synchronized RGB getColor(BGColorMode mode) {
        return colorMap.get(mode);
    }

    public synchronized void setColor(BGColorMode mode, RGB color) {
        colorMap.put(mode, color);
        Set<IBackgroundColorChangedListener> set = listeners.get(mode);
        if (set != null) {
            for (IBackgroundColorChangedListener listener : set) {
                if (listener != null) {
                    listener.setColor(mode, color);
                }
            }
        }
        if (mode == BGColorMode.GLOBAL) {
            setColor(BGColorMode.EDITOR, color);
        }
    }

    public synchronized void addListener(BGColorMode mode,
            IBackgroundColorChangedListener listener) {
        Set<IBackgroundColorChangedListener> set = listeners.get(mode);
        if (set == null) {
            set = new HashSet<IBackgroundColorChangedListener>();
            listeners.put(mode, set);
        }
        set.add(listener);
    }

    public synchronized void removeListener(BGColorMode mode,
            IBackgroundColorChangedListener listener) {
        Set<IBackgroundColorChangedListener> set = listeners.get(mode);
        if (set != null) {
            set.remove(listener);
        }
    }
}
