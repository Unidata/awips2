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
package com.raytheon.uf.common.status.logback;

import ch.qos.logback.core.rolling.FixedWindowRollingPolicy;

/**
 * Fixed window rolling policy that determines the filename pattern from an
 * environment variable.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2013 2142       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class EnvConfigurableFixedWindowRollingPolicy extends
        FixedWindowRollingPolicy {

    private String envLogVar;

    /**
     * @param envLogVar
     *            the envLogVar to set
     */
    public void setEnvLogVar(String envLogVar) {
        this.envLogVar = envLogVar;
        setFileFromEnv();
    }

    private void setFileFromEnv() {
        if (envLogVar == null || envLogVar.isEmpty()) {
            this.addWarn("EnvConfigurableFixedWindowRollingPolicy requires EnvLogVar to be set.");
        } else {
            String file = System.getenv(envLogVar);
            if (file == null || file.isEmpty()) {
                this.addWarn("Appender needs environment variable, "
                        + envLogVar + ", to be set.");
            } else {
                setFileNamePattern(file + "%i");
            }
        }
    }

}
