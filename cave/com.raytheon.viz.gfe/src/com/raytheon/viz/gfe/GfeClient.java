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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Timer;
import java.util.TimerTask;

import jep.Jep;
import jep.JepException;

import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent;

/**
 * GFE Client application component
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2010            mschenke     Initial creation
 * Aug 20, 2012  #1081     dgilling     Don't pass -server and -site args
 *                                      to python script.
 * Dec 04, 2013  #2588     dgilling     Add thread to force shutdown.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GfeClient extends AbstractCAVEComponent {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeClient.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#startInternal
     * (java.lang.String)
     */
    @Override
    protected void startInternal(String componentName) throws Exception {
        long t0 = System.currentTimeMillis();

        String[] args = Platform.getApplicationArgs();
        int gfeClientArgStartIndex = -1;

        for (int i = 0; i < args.length; ++i) {
            String arg = args[i];
            if (componentName.equals(arg)) {
                gfeClientArgStartIndex = i + 1;
                break;
            }
        }

        if (gfeClientArgStartIndex == -1
                || gfeClientArgStartIndex == args.length) {
            System.err.println("No python script specified to run - exiting");
            return;
        }

        System.out.println("Running script: " + args[gfeClientArgStartIndex]);

        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext baseContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);

        String pyVizDir = pathMgr.getFile(baseContext, "pyViz").getPath();

        String pyInclude = System.getProperty("python_include");
        if (pyInclude == null) {
            pyInclude = "";
        }

        String utilityDir = pathMgr.getFile(baseContext,
                com.raytheon.uf.common.util.FileUtil.join("gfe", "utility"))
                .getPath();

        boolean includeUser = (!VizApp.getWsId().getUserName().equals("SITE"));

        String includePath = PyUtil.buildJepIncludePath(true, pyInclude,
                utilityDir, GfePyIncludeUtil.getCommonPythonIncludePath(),
                GfePyIncludeUtil.getCommonGfeIncludePath(),
                GfePyIncludeUtil.getConfigIncludePath(includeUser), pyVizDir,
                GfePyIncludeUtil.getUtilitiesIncludePath(includeUser),
                GfePyIncludeUtil.getIToolIncludePath(),
                GfePyIncludeUtil.getVtecIncludePath(),
                GfePyIncludeUtil.getHeadlineIncludePath(),
                GfePyIncludeUtil.getAutotestIncludePath(),
                GfePyIncludeUtil.getTextUtilitiesIncludePath(includeUser),
                GfePyIncludeUtil.getTextProductsIncludePath(includeUser),
                GfePyIncludeUtil.getTextProductsTemplatesIncludePath(),
                GfePyIncludeUtil.getCombinationsIncludePath(includeUser),
                GfePyIncludeUtil.getTestsIncludePath(),
                GfePyIncludeUtil.getProceduresIncludePath(includeUser));

        Jep jep = null;
        try {
            jep = new Jep(false, includePath, GfeClient.class.getClassLoader());
            jep.eval("import JavaImporter");
            jep.eval("import sys");
            jep.eval("sys.argv = []");
            boolean skipNextArg = false;
            Collection<String> ignoredParams = getIgnoredParameters();
            for (int i = gfeClientArgStartIndex; i < args.length; i++) {
                if (ignoredParams.contains(args[i])) {
                    skipNextArg = true;
                } else if (skipNextArg) {
                    skipNextArg = false;
                } else {
                    jep.eval("sys.argv.append('"
                            + args[i].replaceAll("'", "\\\\'") + "')");
                }
            }
            jep.runScript(args[gfeClientArgStartIndex]);
            // jep.eval("main()");
        } catch (JepException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
        if (jep != null) {
            jep.close();
        }
        long t1 = System.currentTimeMillis();
        System.out.println("Entire execution to run python script: "
                + (t1 - t0));

        // operationally, we've found situations where gfeclient jobs seem to
        // hang around running even though all non-daemon threads have completed
        // their work. So, in attempt to prevent those cases from hanging around
        // as "zombie" processes let's set a timer to kill the JVM if things
        // haven't exited by themselves.
        Timer shutdownTimer = new Timer("gfe-client-shutdown", true);
        TimerTask shutdownTask = new TimerTask() {

            @Override
            public void run() {
                statusHandler
                        .warn("GFEClient should have already exited, but it hasn't. Manually exiting.");
                System.exit(0);
            }
        };
        shutdownTimer.schedule(shutdownTask, 45 * TimeUtil.MILLIS_PER_SECOND);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.personalities.awips.AbstractCAVEComponent#getRuntimeModes
     * ()
     */
    @Override
    protected int getRuntimeModes() {
        return (NON_UI | ALERT_VIZ);
    }

    private Collection<String> getIgnoredParameters() {
        return new HashSet<String>(Arrays.asList("-site", "-server", "-mode",
                "-time"));
    }
}
