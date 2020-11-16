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
package com.raytheon.viz.gfe;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IEvaluationService;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventHandler;

/**
 * Property tester for GFE quick set buttons
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 12, 2010           randerso  Initial creation
 * Jan 15, 2016  5193     bsteffen  Handle uninitialized pref store.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class QuickSetPropertyTester extends PropertyTester {
    private static final String PREFERENCE_KEY = "QuickSetButtons";

    private static final int MAX_BUTTONS = 7;

    private static final int DEFAULT_BUTTONS = 4;

    @Override
    public boolean test(Object receiver, String property, Object[] args,
            Object expectedValue) {
        if (!Activator.getDefault()
                .checkPreferenceStore(new RefreshPropertyHandler())) {
            return false;
        }
        if (PREFERENCE_KEY.equalsIgnoreCase(property)) {
            return ((Integer) expectedValue).intValue() <= getQuickSetButtons();
        }
        return false;
    }

    private int getQuickSetButtons() {
        int quickSetButtons = GFEPreference.getInt(PREFERENCE_KEY,
                DEFAULT_BUTTONS);
        if (quickSetButtons < 1) {
            quickSetButtons = 0;
        } else if (quickSetButtons > MAX_BUTTONS) {
            quickSetButtons = MAX_BUTTONS;
        }
        return quickSetButtons;
    }

    private static class RefreshPropertyHandler implements EventHandler {

        @Override
        public void handleEvent(Event event) {
            IEvaluationService service = PlatformUI.getWorkbench()
                    .getService(IEvaluationService.class);
            service.requestEvaluation("com.raytheon.viz.gfe.QuickSetButtons");
        }

    }
}
