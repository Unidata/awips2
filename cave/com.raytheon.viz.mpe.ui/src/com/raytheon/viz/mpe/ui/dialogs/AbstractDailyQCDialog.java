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
package com.raytheon.viz.mpe.ui.dialogs;

import java.util.Set;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;

/**
 * Abstraction of the MPE Daily QC dialogs.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2017  6164       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class AbstractDailyQCDialog extends AbstractMPEDialog {

    private Set<DisplayMode> cachedDisplayMode;

    public AbstractDailyQCDialog(Shell parent) {
        super(parent);

        cachedDisplayMode = MPEDisplayManager.getCurrent().getDisplayMode();
    }

    /**
     * Add a {@link DisposeListener} to the specified {@link Shell}.
     * 
     * @param shell
     *            the specified {@link Shell}.
     */
    protected void listenToRevertDisplay(final Shell shell) {
        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                revertDisplayModeToPrevious();
            }
        });
    }

    /**
     * Reverts the current {@link DisplayMode}s to the previous selections
     * before a {@link AbstractDailyQCDialog} was opened.
     */
    protected void revertDisplayModeToPrevious() {
        /*
         * Toggle the currently display modes OFF.
         */
        for (DisplayMode currentDisplayMode : MPEDisplayManager.getCurrent()
                .getDisplayMode()) {
            MPEDisplayManager.getCurrent()
                    .toggleDisplayMode(currentDisplayMode);
        }
        /*
         * Toggle the previous display modes back ON.
         */
        for (DisplayMode previousDisplayMode : cachedDisplayMode) {
            MPEDisplayManager.getCurrent()
                    .toggleDisplayMode(previousDisplayMode);
        }
    }
}