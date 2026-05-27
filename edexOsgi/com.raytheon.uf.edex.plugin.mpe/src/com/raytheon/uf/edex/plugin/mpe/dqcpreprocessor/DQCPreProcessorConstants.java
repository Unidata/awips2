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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

/**
 * MPE Constants specific to the DQC PreProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2018 7184       bkowal      Initial creation
 * Apr 06, 2018 7184       bkowal      Added {@link AppsDefaults#DQCPREPROCSRV_RUN_DQC_PREPROCESSOR}.
 *
 * </pre>
 *
 * @author bkowal
 */

public class DQCPreProcessorConstants {

    /*
     * in minutes
     */
    public static final int DEFAULT_TEMPERATURE_WINDOW = 60;

    /*
     * in hours.
     */
    public static final int TEMPERATURE_RANGE_DURATION = 6;

    public static class AppsDefaults {

        public static final String DQCPREPROCSRV_RUN_DQC_PREPROCESSOR = "DqcPreProcSrv.run_dqc_preprocessor";
    }

    private DQCPreProcessorConstants() {
    }
}