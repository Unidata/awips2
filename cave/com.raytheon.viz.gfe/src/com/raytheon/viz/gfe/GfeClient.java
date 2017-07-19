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

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Timer;
import java.util.TimerTask;

import jep.Jep;
import jep.JepConfig;
import jep.JepException;
import jep.NamingConventionClassEnquirer;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.python.GfeCavePyIncludeUtil;
import com.raytheon.viz.ui.personalities.awips.AbstractAWIPSComponent;

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
 * Sep 11, 2013  #2033     dgilling     Update path to utilityDir and pyVizDir,
 *                                      now that they're no longer in 
 *                                      localization store.
 * Dec 04, 2013  #2588     dgilling     Add thread to force shutdown.
 * Mar 25, 2014  #2963     randerso     Removed obsolete python_include support 
 *                                      which was adding an empty string into the 
 *                                      python path causing python to look in user's
 *                                      current default directory for modules.
 * Aug 22, 2014  3500      bclement     override postStartupActions()
 * Aug 29, 2014  3500      bclement     removed override of postStartupActions() 
 *                                      since ProcedureXMLManager startup was moved to the CAVE subclass
 * Apr 26, 2015  4259      njensen      Updated for new JEP API
 * May 20, 2015  4509      njensen      Added time and dataaccess to include path
 * Apr 28, 2016  5236      njensen      Use Jep redirectOutput for python prints
 * Jul 19, 2017  ----      mjames@ucar  Add common python include path.
 * 
 * </pre>
 * 
 * @author mschenke
 */

public class GfeClient extends AbstractAWIPSComponent {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeClient.class);

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

        if ((gfeClientArgStartIndex == -1)
                || (gfeClientArgStartIndex == args.length)) {
            System.err.println("No python script specified to run - exiting");
            return;
        }

        System.out.println("Running script: " + args[gfeClientArgStartIndex]);

        String pyVizDir = new File(FileLocator.resolve(
                FileLocator.find(Activator.getDefault().getBundle(), new Path(
                        FileUtil.join("python", "pyViz")), null)).getPath())
                .getPath();

        String utilityDir = new File(FileLocator.resolve(
                FileLocator.find(Activator.getDefault().getBundle(), new Path(
                        FileUtil.join("python", "utility")), null)).getPath())
                .getPath();

        boolean includeUser = (!VizApp.getWsId().getUserName().equals("SITE"));

        String includePath = PyUtil.buildJepIncludePath(true, utilityDir,
                PythonIncludePathUtil.getCommonPythonIncludePath(),
                PythonIncludePathUtil.getCommonPythonIncludePath("time",
                        "dataaccess"), GfePyIncludeUtil
                        .getCommonGfeIncludePath(), GfePyIncludeUtil
                        .getConfigIncludePath(includeUser), pyVizDir,
                GfePyIncludeUtil.getUtilitiesIncludePath(includeUser),
                GfePyIncludeUtil.getIToolIncludePath(), GfePyIncludeUtil
                        .getVtecIncludePath(), GfePyIncludeUtil
                        .getHeadlineIncludePath(), GfeCavePyIncludeUtil
                        .getAutotestIncludePath(), GfePyIncludeUtil
                        .getTextUtilitiesIncludePath(includeUser),
                GfePyIncludeUtil.getTextProductsIncludePath(includeUser),
                GfePyIncludeUtil.getCombinationsIncludePath(includeUser),
                GfeCavePyIncludeUtil.getTestsIncludePath(), GfePyIncludeUtil
                        .getProceduresIncludePath(includeUser));

        Jep jep = null;
        try {
            JepConfig config = new JepConfig().setInteractive(false)
                    .setIncludePath(includePath)
                    .setClassLoader(GfeClient.class.getClassLoader())
                    .setClassEnquirer(new NamingConventionClassEnquirer())
                    .setRedirectOutputStreams(true);
            jep = new Jep(config);
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

    @Override
    protected int getRuntimeModes() {
        return (NON_UI | ALERT_VIZ);
    }

    private Collection<String> getIgnoredParameters() {
        return new HashSet<>(
                Arrays.asList("-site", "-server", "-mode", "-time"));
    }

}
