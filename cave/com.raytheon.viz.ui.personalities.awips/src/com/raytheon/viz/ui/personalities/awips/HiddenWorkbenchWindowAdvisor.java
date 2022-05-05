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

import org.eclipse.e4.ui.model.application.ui.basic.MWindow;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;

/**
 * Workbench window advisor to use which allows for the workbench to be started
 * but not visible. This is used by {@link HiddenWorkbenchAdvisor} for
 * {@link AbstractAWIPSComponent}s that need the workbench to start but do not
 * utilize it
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 09, 2010           mschenke  Initial creation
 * Dec 09, 2019  7991     randerso  Fix blank CAVE window when running
 *                                  standalone TextWS
 *
 * </pre>
 *
 * @author mschenke
 */

public class HiddenWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor {

    /**
     * @param configurer
     */
    public HiddenWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer) {
        super(configurer);
    }

    @Override
    public void postWindowOpen() {
        getWindowConfigurer().getWindow().getService(MWindow.class)
                .setVisible(false);
    }

}
