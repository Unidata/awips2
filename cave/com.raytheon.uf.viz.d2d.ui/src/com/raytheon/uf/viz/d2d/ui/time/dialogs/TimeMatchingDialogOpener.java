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
package com.raytheon.uf.viz.d2d.ui.time.dialogs;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.d2d.core.time.TimeMatchingConfiguration;

/**
 * 
 * Class opens and waits on time matching dialog for configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class TimeMatchingDialogOpener {

    public static TimeMatchingConfiguration runDialog(
            Class<? extends AbstractTimeMatchingDialog> dialogClass,
            LoadProperties loadProps, D2DTimeMatcher timeMatcher,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        return runDialog(null, dialogClass, loadProps, timeMatcher,
                availableTimes, descriptor);
    }

    public static TimeMatchingConfiguration runDialog(String title,
            Class<? extends AbstractTimeMatchingDialog> dialogClass,
            LoadProperties loadProps, D2DTimeMatcher timeMatcher,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        final AbstractTimeMatchingDialog dialog = TimeMatchingDialogFactory
                .constructDialog(dialogClass, loadProps, timeMatcher,
                        availableTimes, descriptor);
        if (title != null) {
            dialog.setText(title);
        }

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                dialog.open();
            }
        });

        return dialog.getConfig();
    }
}
