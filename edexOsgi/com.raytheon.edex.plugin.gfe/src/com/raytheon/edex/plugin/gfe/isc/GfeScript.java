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
package com.raytheon.edex.plugin.gfe.isc;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;

/**
 * Thread object which contains a script to execute.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2009 #2960       bphillip    Initial creation
 * Mar 04, 2013 #1447       dgilling    Add VTEC scripts to include path.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GfeScript extends Thread {

    /** The site this script is to be run for */
    private String site;

    /** The name of the script to execute */
    private String scriptName;

    /** The method name to execute. Defaults to "main" */
    private String methodName = "main";

    private String pythonIncludePath;

    /** The PythonScript instance */
    private PythonScript script;

    /** The return value returned from the last execution */
    private String retVal;

    private boolean running = false;

    private BlockingQueue<Map<String, Object>> cmdQueue = new LinkedBlockingQueue<Map<String, Object>>();

    /**
     * Creates a new GfeScript for a specific script
     * 
     * @param scriptName
     *            The name of the script
     */
    public GfeScript(String scriptName, String site) {
        setDaemon(true);
        this.scriptName = scriptName;
        this.site = site;
    }

    @Override
    public void run() {

        /*
         * Loads a script from the localization directory
         */
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext cx = pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        String scriptFile = pathMgr
                .getLocalizationFile(cx,
                        "gfe/isc" + File.separator + scriptName + ".py")
                .getFile().getPath();

        // Instantiates the PythonScript if necessary
        try {
            if (pythonIncludePath == null) {
                pythonIncludePath = PyUtil.buildJepIncludePath(
                        GfePyIncludeUtil.getCommonPythonIncludePath(),
                        GfePyIncludeUtil.getVtecIncludePath(site),
                        GfePyIncludeUtil.getIscScriptsIncludePath(),
                        GfePyIncludeUtil.getGfeConfigIncludePath(site));
            }
            script = new PythonScript(scriptFile, pythonIncludePath);
        } catch (JepException e1) {
            retVal = e1.getMessage();
            return;
        }

        /*
         * Infinite loop which waits until notified to execute the script
         */
        try {
            while (true) {
                try {
                    this.running = false;
                    script.execute(methodName, getNextCommand());
                } catch (InterruptedException e) {
                    retVal = e.getLocalizedMessage();
                    continue;
                } catch (JepException e) {
                    retVal = e.getLocalizedMessage();
                    continue;
                } catch (Throwable e) {
                    retVal = e.getLocalizedMessage();
                    continue;
                }
                retVal = "Success";
            }
        } finally {
            if (script != null) {
                script.dispose();
            }
        }
    }

    public Map<String, Object> getNextCommand() throws InterruptedException {
        Map<String, Object> retVal = cmdQueue.take();
        this.running = true;
        return retVal;
    }

    /**
     * Executes the script
     * 
     * @param args
     *            The arguments to pass to the script
     */
    public void execute(String[] args) {
        this.running = true;
        cmdQueue.add(setArguments(args));
    }

    public void execute(String method, Map<String, Object> args) {
        this.running = true;
        this.methodName = method;
        cmdQueue.add(args);
    }

    /**
     * Gets the return value of the last execution
     * 
     * @return The return value of the las execution
     */
    public String getRetVal() {
        return retVal;
    }

    private Map<String, Object> setArguments(String[] args) {
        Map<String, Object> argMap = new HashMap<String, Object>();
        List<String> argList = new ArrayList<String>();
        argList.add(scriptName);
        argList.addAll(Arrays.asList(args));
        argMap.put("argv", argList);
        return argMap;
    }

    public boolean isRunning() {
        return running;
    }

    public String waitFor() {
        try {
            while (isRunning()) {

                Thread.sleep(200);
            }
        } catch (InterruptedException e1) {
        }
        return retVal;
    }
}
