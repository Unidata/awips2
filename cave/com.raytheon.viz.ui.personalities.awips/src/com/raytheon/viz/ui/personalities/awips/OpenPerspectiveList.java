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
package com.raytheon.viz.ui.personalities.awips;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener3;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;

/**
 * Listener for managing list of open perspectives
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class OpenPerspectiveList {

    private static class OpenPerspectiveListener implements
            IPerspectiveListener3 {

        private List<IPerspectiveDescriptor> openPerspectives;

        public OpenPerspectiveListener(
                List<IPerspectiveDescriptor> openPerspectives) {
            this.openPerspectives = openPerspectives;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.IPerspectiveListener3#perspectiveOpened(org.eclipse.ui
         * .IWorkbenchPage, org.eclipse.ui.IPerspectiveDescriptor)
         */
        @Override
        public void perspectiveOpened(IWorkbenchPage page,
                IPerspectiveDescriptor perspective) {
            openPerspectives.add(perspective);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.IPerspectiveListener3#perspectiveClosed(org.eclipse.ui
         * .IWorkbenchPage, org.eclipse.ui.IPerspectiveDescriptor)
         */
        @Override
        public void perspectiveClosed(IWorkbenchPage page,
                IPerspectiveDescriptor perspective) {
            openPerspectives.remove(perspective);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.IPerspectiveListener3#perspectiveDeactivated(org.eclipse
         * .ui.IWorkbenchPage, org.eclipse.ui.IPerspectiveDescriptor)
         */
        @Override
        public void perspectiveDeactivated(IWorkbenchPage page,
                IPerspectiveDescriptor perspective) {
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.IPerspectiveListener3#perspectiveSavedAs(org.eclipse
         * .ui .IWorkbenchPage, org.eclipse.ui.IPerspectiveDescriptor,
         * org.eclipse.ui.IPerspectiveDescriptor)
         */
        @Override
        public void perspectiveSavedAs(IWorkbenchPage page,
                IPerspectiveDescriptor oldPerspective,
                IPerspectiveDescriptor newPerspective) {
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.IPerspectiveListener2#perspectiveChanged(org.eclipse
         * .ui .IWorkbenchPage, org.eclipse.ui.IPerspectiveDescriptor,
         * org.eclipse.ui.IWorkbenchPartReference, java.lang.String)
         */
        @Override
        public void perspectiveChanged(IWorkbenchPage page,
                IPerspectiveDescriptor perspective,
                IWorkbenchPartReference partRef, String changeId) {
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.IPerspectiveListener#perspectiveActivated(org.eclipse.
         * ui.IWorkbenchPage, org.eclipse.ui.IPerspectiveDescriptor)
         */
        @Override
        public void perspectiveActivated(IWorkbenchPage page,
                IPerspectiveDescriptor perspective) {
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.IPerspectiveListener#perspectiveChanged(org.eclipse.ui
         * .IWorkbenchPage, org.eclipse.ui.IPerspectiveDescriptor,
         * java.lang.String)
         */
        @Override
        public void perspectiveChanged(IWorkbenchPage page,
                IPerspectiveDescriptor perspective, String changeId) {
        }
    }

    private static Map<IWorkbenchWindow, OpenPerspectiveList> instanceMap = new HashMap<IWorkbenchWindow, OpenPerspectiveList>();

    private List<IPerspectiveDescriptor> openPerspectives;

    public static synchronized OpenPerspectiveList getInstance(
            IWorkbenchWindow window) {
        OpenPerspectiveList instance = instanceMap.get(window);
        if (instance == null) {
            instance = new OpenPerspectiveList();
            window.addPerspectiveListener(new OpenPerspectiveListener(
                    instance.openPerspectives));
            instanceMap.put(window, instance);
        }
        return instance;
    }

    private OpenPerspectiveList() {
        openPerspectives = new ArrayList<IPerspectiveDescriptor>();
    }

    public List<IPerspectiveDescriptor> getOpenedPerspectives() {
        return new ArrayList<IPerspectiveDescriptor>(openPerspectives);
    }

}
