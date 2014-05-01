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
package com.raytheon.viz.aviation.guidance;

import java.io.File;
import java.util.Map;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.jobs.IRequestCompleteListener;
import com.raytheon.viz.aviation.activator.Activator;

/**
 * Only for testing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@Deprecated
public class TestJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TestJob.class);

    private GuidanceRequest request;

    public TestJob(String name, GuidanceRequest req) {
        super(name);
        request = req;
    }

    private PythonScript initializePython() throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext baseCommonCtx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File pointdata = pathMgr.getStaticFile(
                "pointdata/PointDataContainer.py").getParentFile();
        File status = pathMgr.getStaticFile("python/UFStatusHandler.py")
                .getParentFile();
        File runner = pathMgr.getStaticFile("aviation/python/GuidanceEntry.py");
        File commonPython = pathMgr.getFile(baseCommonCtx, "python");
        String filePath = runner.getPath();
        String includePath = PyUtil.buildJepIncludePath(runner.getParentFile()
                .getPath(), pointdata.getPath(), status.getPath(), commonPython
                .getPath());
        PythonScript python = new PythonScript(filePath, includePath,
                PythonGuidanceJob.class.getClassLoader());
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
        try {
            python = initializePython();
        } catch (JepException e) {
            return new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                    "Error initializing guidance python", e);
        }

        final IRequestCompleteListener<String[]> listener = request
                .getListener();
        Map<String, Object> args = request.getPythonArguments();
        String methodName = request.getGuidanceType().getPythonMethod();
        try {
            long t0 = System.currentTimeMillis();
            final String[] result = (String[]) python.execute(methodName, args);
            long t1 = System.currentTimeMillis();
            System.out.println("Python guidance time: " + (t1 - t0));
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    listener.requestComplete(result);
                }
            });
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM, "Error generating guidance",
                    e);

        } finally {
            if (python != null) {
                python.dispose();
            }
        }

        return Status.OK_STATUS;
    }

}
