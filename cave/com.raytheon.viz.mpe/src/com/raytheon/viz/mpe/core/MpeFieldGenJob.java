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

package com.raytheon.viz.mpe.core;

import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.mpe.Activator;

/**
 * Job that executes the MPE Field Gen Job
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/08/09      1674       bphillip    Initial creation
 * 08/09/12     15307      snaples     Updated job to use postStreamingByteArray.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class MpeFieldGenJob extends Job {

    private static final IUFStatusHandler handler = UFStatus.getHandler(
            MpeFieldGenJob.class, "DEFAULT");

    /** The HTTP endpoint of the field gen service */
    private static final String ENDPOINT_NAME = "/mpeFieldGenHttpService";

    /** The argument to run the MPE Field Gen with */
    private String fieldGenArg;

    /**
     * Constructs a new MpeFieldGenJob
     * 
     * @param fieldGenArg
     *            The argument to run the MPE Field Gen with
     */
    public MpeFieldGenJob(String fieldGenArg) {
        super("Running Mpe Field Gen");
        this.fieldGenArg = fieldGenArg;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        final Integer[] mpeExitValue = new Integer[1];

        String httpAddress = VizApp.getHttpServer() + ENDPOINT_NAME;
        String args = fieldGenArg;
        byte[] ba = args.getBytes();
        
        try {
            HttpClient.getInstance().postStreamingByteArray(httpAddress, ba,
                    new HttpClient.IStreamHandler() {

                        /*
                         * (non-Javadoc)
                         * 
                         * @see
                         * com.raytheon.viz.core.comm.Connector.IStreamHandler
                         * #handleStream(java.io.InputStream)
                         */
                        @Override
                        public void handleStream(InputStream is)
                                throws CommunicationException {
                            try {

                                DynamicSerializationManager dsm = DynamicSerializationManager
                                        .getManager(SerializationType.Thrift);
                                mpeExitValue[0] = (Integer) dsm.deserialize(is);
                                System.out.println("MPE FieldGen returned: "
                                        + mpeExitValue[0]);
                            } catch (SerializationException e) {
                                throw new CommunicationException(
                                        "Error deserializing", e);
                            }
                        }

                    });
        } catch (CommunicationException e) {
            return new Status(Status.ERROR, Activator.PLUGIN_ID,
                    "MPE Field Gen execution failed with exit code: "
                            + mpeExitValue[0]);
        } 

        if (mpeExitValue[0] != null && mpeExitValue[0] == 0) {
            return Status.OK_STATUS;
        }

        return new Status(Status.ERROR, Activator.PLUGIN_ID,
                "MPE Field Gen execution failed with exit code: "
                        + mpeExitValue[0]);
    }
}
