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
package com.raytheon.uf.viz.collaboration.display;

import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;

/**
 * Interface for object that contains remote displays
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IRemoteDisplayContainer {

    public static class RemoteDisplay {
        private final int displayId;

        private final IRenderableDisplay display;

        /**
         * @param displayId
         * @param display
         */
        public RemoteDisplay(int displayId, IRenderableDisplay display) {
            this.displayId = displayId;
            this.display = display;
        }

        /**
         * @return the displayId
         */
        public int getDisplayId() {
            return displayId;
        }

        /**
         * @return the display
         */
        public IRenderableDisplay getDisplay() {
            return display;
        }

    }

    public static enum RemoteDisplayChangeType {
        CREATED, DISPOSED, ACTIVATED;
    }

    public static interface IRemoteDisplayChangedListener {
        public void remoteDisplayChanged(RemoteDisplay remoteDisplay,
                RemoteDisplayChangeType changeType);
    }

    public void addRemoteDisplayChangedListener(
            IRemoteDisplayChangedListener listener);

    public void removeRemoteDisplayChangedListener(
            IRemoteDisplayChangedListener listener);

    public RemoteDisplay getActiveDisplay();

    public IEditorPart getActiveDisplayEditor();

    public void disposeContainer();
}
