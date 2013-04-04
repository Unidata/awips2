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
package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Loads a procedure to the display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class ProcedureLoadJob {

    private static final ProcedureLoadJob instance = new ProcedureLoadJob();

    private LinkedHashMap<AbstractEditor, Bundle> bundlesToLoad = new LinkedHashMap<AbstractEditor, Bundle>();

    private Job j = new Job("Loading bundle") {
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            while (bundlesToLoad.size() > 0) {
                Map.Entry<AbstractEditor, Bundle> entry = null;
                synchronized (bundlesToLoad) {
                    Iterator<Map.Entry<AbstractEditor, Bundle>> iter = bundlesToLoad
                            .entrySet().iterator();
                    if (iter.hasNext()) {
                        entry = iter.next();
                        iter.remove();
                    }
                }

                if (entry != null) {
                    AbstractEditor editor = entry.getKey();
                    Bundle b = entry.getValue();

                    new BundleLoader(editor, b).run();
                }
            }

            return Status.OK_STATUS;
        }

    };

    public static ProcedureLoadJob getInstance() {
        return instance;
    }

    /**
     * Puts the bundle on the queue to be loaded. And editor can only have one
     * Bundle waiting to be loaded.
     * 
     * @param b
     * @param e
     */
    public void enqueue(Bundle b, AbstractEditor e) {
        synchronized (bundlesToLoad) {
            bundlesToLoad.put(e, b);
        }

        if (j.getState() != Job.RUNNING) {
            j.schedule();
        }
    }
}
