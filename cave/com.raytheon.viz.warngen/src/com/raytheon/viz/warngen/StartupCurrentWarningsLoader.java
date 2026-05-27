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
package com.raytheon.viz.warngen;

import java.util.Arrays;

import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IStartup;

import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.warngen.util.CurrentWarnings;

/**
 * Class for loading current warnings on CAVE startup, to speed up WarnGen
 * startup.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 22, 2021 8258       mapeters    Initial creation
 * Feb 21, 2024 2036872    aqlockleigh Don't fetch the CurrentWarnings 
 *                                     if we're in a standalone component.
 *
 * </pre>
 *
 * @author mapeters
 */
public class StartupCurrentWarningsLoader implements IStartup {

    private static final String COMPONENT_FLAG = "-component";

    @Override
    public void earlyStartup() {
        String[] args = Platform.getApplicationArgs();

        boolean hasCompFlag = Arrays.stream(args)
                .anyMatch(x -> COMPONENT_FLAG.equals(x));

        if (!hasCompFlag) {
            // DR 2036872: Only load the warnings if we're not running as a
            // standalone component (e.g. TextWS).
            Runnable runnable = () -> CurrentWarnings.getInstance(
                    LocalizationManager.getInstance().getCurrentSite());
            new Thread(runnable, getClass().getSimpleName()).start();
        }
    }
}