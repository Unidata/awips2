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
package com.raytheon.uf.common.mpe.dqcpreprocessor;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Provides the run configuration for an on-demand run of the DQC PreProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2018  7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

@DynamicSerialize
public class DQCPreProcessorExecuteRequest implements IServerRequest {

    @DynamicSerializeElement
    private DQCPreProcRunConfiguration runConfig;

    public DQCPreProcessorExecuteRequest() {
    }

    public DQCPreProcessorExecuteRequest(
            final DQCPreProcRunConfiguration runConfig) {
        this.runConfig = runConfig;
    }

    public DQCPreProcRunConfiguration getRunConfig() {
        return runConfig;
    }

    public void setRunConfig(DQCPreProcRunConfiguration runConfig) {
        this.runConfig = runConfig;
    }
}