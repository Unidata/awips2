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

import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcessorExecuteRequest;
import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcessorExecuteResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Executes the {@link DQCPreProcessor} on-demand in response to a
 * {@link DQCPreProcessorExecuteRequest}.
 * 
 * TODO: presently, this class executes the DQC PreProcessor directly in the
 * request JVM (mirroring the on-demand execution of Field Generation). However,
 * as more of MPE is modernized, both this implementation and the Field
 * Generation implementation should be updated to POST to the queue that the
 * instance of each process in the ingest JVM listens to to know when to
 * execute. Then, the associated processes in the ingest JVM would post to a
 * status topic that any connected CAVE clients would listen to to know when
 * updated data was available.
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

public class DQCPreProcessorExecuteHandler
        implements IRequestHandler<DQCPreProcessorExecuteRequest> {

    @Override
    public Object handleRequest(DQCPreProcessorExecuteRequest request)
            throws Exception {
        new DQCPreProcessor().execute(request.getRunConfig());
        return new DQCPreProcessorExecuteResponse();
    }
}