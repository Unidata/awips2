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
package com.raytheon.uf.viz.core.log;

import org.apache.log4j.RollingFileAppender;
import org.apache.log4j.helpers.LogLog;

/**
 * RollingFileAppender retrieves the value set for property EnvLogVar from the
 * system environment to define the log file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2012            bgonzale    Initial creation
 * Mar 21, 2013       1638 mschenke    Moved from alertviz to core
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class EnvConfigurableRollingFileAppender extends RollingFileAppender {

    private String envLogVar;

    /**
     * @param envLogVar
     *            the envLogVar to set
     */
    public void setEnvLogVar(String envLogVar) {
        this.envLogVar = envLogVar;
        setFileFromEnv();
    }

    /**
     * @return the envLogVar
     */
    public String getEnvLogVar() {
        return envLogVar;
    }

    private void setFileFromEnv() {
        if (envLogVar == null || envLogVar.isEmpty()) {
            LogLog.error("Appender [" + name
                    + "] requires EnvLogVar to be set.");
        } else {
            String file = System.getenv(envLogVar);

            if (file == null || file.isEmpty()) {
                LogLog.error("Appender [" + name
                        + "] needs environment variable, " + envLogVar
                        + ", to be set.");
            } else {
                setFile(file);
            }
        }
    }

}
