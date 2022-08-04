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
 *
 * </pre>
 *
 * @author mapeters
 */
public class StartupCurrentWarningsLoader implements IStartup {

    @Override
    public void earlyStartup() {
        Runnable runnable = () -> CurrentWarnings.getInstance(
                LocalizationManager.getInstance().getCurrentSite());
        new Thread(runnable, getClass().getSimpleName()).start();
    }
}