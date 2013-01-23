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
 * Executor service that runs tasks asynchronously or synchronously on the GUI
 * thread.
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

public interface IGuiThreadTaskExecutor {

    /**
     * Run a task asynchronously on the UI thread
     * 
     * @param aTask
     *            the task to run
     */
    void runAsync(Runnable aTask);

    /**
     * Run a task synchronously on the UI thread
     * 
     * @param task
     *            the task to run
     */
    void runSync(Runnable task);

    /**
     * Run a task synchronously on the UI thread if a workbench is running,
     * otherwise just runs the task
     * 
     * @param task
     *            the task to run
     */
    void runSyncIfWorkbench(Runnable task);
}
