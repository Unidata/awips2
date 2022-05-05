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
package com.raytheon.viz.gfe.gfeclient;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonEval;
import com.raytheon.uf.common.python.PythonIncludePathUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManagerFactory;
import com.raytheon.viz.gfe.python.GfeCavePyIncludeUtil;
import com.raytheon.viz.ui.personalities.awips.AbstractAWIPSComponent;

import jep.JepConfig;
import jep.JepException;
import jep.NamingConventionClassEnquirer;

/**
 * GFE Client application component
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 25, 2010           mschenke  Initial creation
 * Aug 20, 2012  1081     dgilling  Don't pass -server and -site args to python
 *                                  script.
 * Sep 11, 2013  2033     dgilling  Update path to utilityDir and pyVizDir, now
 *                                  that they're no longer in localization
 *                                  store.
 * Dec 04, 2013  2588     dgilling  Add thread to force shutdown.
 * Mar 25, 2014  2963     randerso  Removed obsolete python_include support
 *                                  which was adding an empty string into the
 *                                  python path causing python to look in user's
 *                                  current default directory for modules.
 * Aug 22, 2014  3500     bclement  override postStartupActions()
 * Aug 29, 2014  3500     bclement  removed override of postStartupActions()
 *                                  since ProcedureXMLManager startup was moved
 *                                  to the CAVE subclass
 * Apr 26, 2015  4259     njensen   Updated for new JEP API
 * May 20, 2015  4509     njensen   Added time and dataaccess to include path
 * Apr 28, 2016  5236     njensen   Use Jep redirectOutput for python prints
 * Jan 24, 2017  6092     randerso  Change to use PythonEval in place of
 *                                  directly using Jep. Code cleanup.
 * Feb 20, 2018  6602     dgilling  Update for new text utilities path.
 * Jan 07, 2019  21019    ryu       Fix issue of gfeclient hanging on exit.
 *
 * </pre>
 *
 * @author mschenke
 */

public class GfeClient extends AbstractAWIPSComponent {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GfeClient.class);

    private static final Collection<String> IGNORED_PARAMETERS = new HashSet<>(
            Arrays.asList("-site", "-server", "-mode", "-time"));

    /**
     * @param componentName
     * @throws Exception
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

        if ((gfeClientArgStartIndex == -1)
                || (gfeClientArgStartIndex == args.length)) {
            statusHandler.error("No python script specified to run - exiting");
            return;
        }

        statusHandler.info("Running script: " + args[gfeClientArgStartIndex]);

        String pyVizDir = new File(
                FileLocator
                        .resolve(FileLocator.find(
                                Activator.getDefault().getBundle(),
                                new Path(FileUtil.join("python", "pyViz")),
                                null))
                        .getPath()).getPath();

        String utilityDir = new File(
                FileLocator
                        .resolve(FileLocator.find(
                                Activator.getDefault().getBundle(),
                                new Path(FileUtil.join("python", "utility")),
                                null))
                        .getPath()).getPath();

        boolean includeUser = (!"SITE".equals(VizApp.getWsId().getUserName()));

        String includePath = PyUtil.buildJepIncludePath(true, utilityDir,
                PythonIncludePathUtil.getCommonPythonIncludePath("time",
                        "dataaccess"),
                GfePyIncludeUtil.getCommonGfeIncludePath(),
                GfePyIncludeUtil.getConfigIncludePath(includeUser), pyVizDir,
                GfePyIncludeUtil.getUtilitiesIncludePath(includeUser),
                GfePyIncludeUtil.getIToolIncludePath(),
                GfePyIncludeUtil.getVtecIncludePath(),
                GfeCavePyIncludeUtil.getAutotestIncludePath(),
                GfePyIncludeUtil.getTextUtilitiesIncludePath(includeUser),
                GfePyIncludeUtil.getTextProductsIncludePath(includeUser),
                GfePyIncludeUtil.getCombinationsIncludePath(includeUser),
                GfeCavePyIncludeUtil.getTestsIncludePath(),
                GfePyIncludeUtil.getProceduresIncludePath(includeUser));

        try (PythonEval pythonEval = new PythonEval(new JepConfig()
                .setInteractive(false).setIncludePath(includePath)
                .setClassLoader(GfeClient.class.getClassLoader())
                .setClassEnquirer(new NamingConventionClassEnquirer())
                .setRedirectOutputStreams(true))) {

            pythonEval.eval("import sys");
            pythonEval.eval("sys.argv = []");
            boolean skipNextArg = false;
            for (int i = gfeClientArgStartIndex; i < args.length; i++) {
                if (IGNORED_PARAMETERS.contains(args[i])) {
                    skipNextArg = true;
                } else if (skipNextArg) {
                    skipNextArg = false;
                } else {
                    pythonEval.eval("sys.argv.append('"
                            + args[i].replaceAll("'", "\\\\'") + "')");
                }
            }
            pythonEval.runScript(args[gfeClientArgStartIndex]);
        } catch (JepException e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }

        long t1 = System.currentTimeMillis();
        statusHandler
                .info("Entire execution to run python script: " + (t1 - t0));

        DataManagerFactory.disposeAll();
    }

    @Override
    protected int getRuntimeModes() {
        return (NON_UI | ALERT_VIZ);
    }

}
