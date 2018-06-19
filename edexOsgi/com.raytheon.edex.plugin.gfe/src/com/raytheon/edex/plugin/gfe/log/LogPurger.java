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
package com.raytheon.edex.plugin.gfe.log;

import jep.JepException;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataplugin.gfe.util.FilePurger;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonEval;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Bean for purging GFESuite logs. Should be scheduled to run from a cron.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bphillip     Initial creation
 * Sep 05, 2013  #2307     dgilling     Use better PythonScript constructor.
 * Feb 26, 2015  #4128     dgilling     Switch to IFPServer.getActiveSites().
 * Jul 15, 2016  #5747     dgilling     Refactor based on FilePurger.
 * 
 * </pre>
 * 
 * @author bphillip
 */

public class LogPurger {

    public void purge() throws JepException {
        for (String siteID : IFPServer.getActiveSites()) {
            String includePath = PyUtil.buildJepIncludePath(
                    GfePyIncludeUtil.getCommonPythonIncludePath(),
                    GfePyIncludeUtil.getIscScriptsIncludePath(),
                    GfePyIncludeUtil.getGfeConfigIncludePath(siteID));
            String logDir = StringUtils.EMPTY;
            try (PythonEval interpreter = new PythonEval(includePath,
                    getClass().getClassLoader())) {
                interpreter.eval("import siteConfig");
                logDir = interpreter.getValue("siteConfig.GFESUITE_LOGDIR")
                        .toString();
            }

            if (!StringUtils.isBlank(logDir)) {
                IFPServer server = IFPServer.getActiveServer(siteID);
                long purgeAge = server.getConfig().logFilePurgeAfter()
                        * TimeUtil.MILLIS_PER_DAY;
                new FilePurger(logDir, purgeAge).purge();
            }
        }
    }
}
