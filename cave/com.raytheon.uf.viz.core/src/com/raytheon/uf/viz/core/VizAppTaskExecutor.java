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
package com.raytheon.uf.viz.core;

/**
 * Implementation of {@link IVizAppTaskExecutor} that uses VizApp.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 02, 2013 1449       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class VizAppTaskExecutor implements IGuiThreadTaskExecutor {

    /**
     * {@inheritDoc}
     */
    @Override
    public void runAsync(Runnable aTask) {
        VizApp.runAsync(aTask);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void runSync(Runnable task) {
        VizApp.runSync(task);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void runSyncIfWorkbench(Runnable task) {
        VizApp.runSyncIfWorkbench(task);
    }

}
