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

package com.raytheon.uf.edex.ohd.pproc;

import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcRunConfiguration;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.DQCPreProcessor;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.DqcPreProcFailedException;

/**
 * Service implementation for executing the Dqc Preprocessor application
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25 2008             snaples     Initial creation
 * Mar 28, 2014   2952     mpduff      Changed to use UFStatus for logging.
 * Jan 12, 2018   7184     bkowal      Updated to run the Java port of DQC PreProcessor.
 * Apr 08, 2018   7184     bkowal      The {@link DQCPreProcRunConfiguration} is now provided externally.
 * </pre>
 * 
 * @author snaples
 */
public class DqcPreProcSrv {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(DqcPreProcSrv.class);

    public DQCPreProcRunConfiguration getDefaultConfiguration() {
        return new DQCPreProcRunConfiguration();
    }

    public void runDQCPreproc(final DQCPreProcRunConfiguration runConfig)
            throws EdexException {
        try {
            new DQCPreProcessor().execute(runConfig);
        } catch (DqcPreProcFailedException e) {
            logger.error("DQC PreProcessor has failed to complete.", e);
        }
    }
}