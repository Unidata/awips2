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
package com.raytheon.viz.aviation.monitor;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.viz.core.VizApp;

/**
 * Job that executes python monitoring code. The PythonCachedMonitorJob should
 * be used in production, this should only be used for development.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@Deprecated
public class TestMonitorJob extends Job {

    private MonitorRequest request;

    public TestMonitorJob(String name, MonitorRequest req) {
        super(name);
        this.request = req;
    }

    private PythonScript initializePython() throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        File runner = pathMgr.getStaticFile("aviation/python/MonitorEntry.py");
        String filePath = runner.getPath();
        String includePath = PyUtil.buildJepIncludePath(runner.getParentFile()
                .getPath(), AvnPyUtil.getLoggingHandlerDir(), AvnPyUtil
                .getPointDataDir(), AvnPyUtil.getCommonPythonDir());
        PythonScript python = new PythonScript(filePath, includePath,
                TestMonitorJob.class.getClassLoader());
        return python;
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        PythonScript python = null;
        Map<String, Object> args = new HashMap<String, Object>();
        args.put("request", request);
        try {
            python = initializePython();
            long t0 = System.currentTimeMillis();
            final Map<?, ?> result = (Map<?, ?>) python
                    .execute("monitor", args);
            long t1 = System.currentTimeMillis();
            System.out.println("Python monitor time: " + (t1 - t0));
            // System.out.println(result);
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    request.getListener().requestComplete(result);
                }
            });
        } catch (JepException e) {
            // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
            // StatusConstants.CATEGORY_AVNFPS, null,
            // "Error monitoring data", e);
            // TODO go back to UFStatus once more monitors are hooked up
            e.printStackTrace();

        } finally {
            if (python != null) {
                python.dispose();
            }
        }

        return Status.OK_STATUS;
    }

}
