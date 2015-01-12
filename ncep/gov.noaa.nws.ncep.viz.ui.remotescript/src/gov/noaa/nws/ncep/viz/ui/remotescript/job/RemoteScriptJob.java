/*
 * gov.noaa.nws.ncep.viz.ui.remotescript.job.RemoteScriptJob
 * 
 * 26 March 2014
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.ui.remotescript.job;

import java.util.Calendar;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.remote.script.RemoteScriptRunRequest;
import com.raytheon.uf.common.remote.script.RemoteScriptRunResponse;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Remote script job class.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/14        ?           B. Yin  Initial Creation.
 * 
 * </pre>
 * 
 * @author byin
 * 
 */

public class RemoteScriptJob extends Job {
    public enum JobStatus {
        IN_PROGRESS, OK, FAIL, TIME_OUT
    }

    private IUser user;

    private String remoteScriptName;

    private String arguments;

    private LocalizationContext context;

    private Calendar timeSubmitted;

    private JobStatus status;

    private String output = "";

    private String error = "";

    public RemoteScriptJob(IUser user, String name, String arguments,
            LocalizationContext context, Calendar date, JobStatus status) {
        super(name + "NCP");
        this.user = user;
        this.remoteScriptName = name;
        this.arguments = arguments;
        this.context = context;
        this.timeSubmitted = date;
        this.status = status;
    }

    public String getRemoteScriptName() {
        return remoteScriptName;
    }

    public LocalizationContext getContext() {
        return context;
    }

    public String getJobName() {
        return remoteScriptName;
    }

    public String getOutput() {
        return output;
    }

    public Calendar getTmSubmitted() {
        return timeSubmitted;
    }

    public JobStatus getStatus() {
        return status;
    }

    public String getError() {
        return error;
    }

    /**
     * Send a request to EDEX to run the job and get the response.
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {

        RemoteScriptRunRequest runRequest = new RemoteScriptRunRequest(user
                .uniqueId().toString(), remoteScriptName, context);
        runRequest.setUser(user);

        runRequest.parseAndSetScriptArguments(arguments);

        try {
            RemoteScriptRunResponse runResponse = (RemoteScriptRunResponse) ThriftClient
                    .sendPrivilegedRequest(runRequest);
            if (runResponse != null) {
                if (runResponse.isTimedOut()) {
                    status = JobStatus.TIME_OUT;
                } else {
                    output = runResponse.getOutput();
                    if (runResponse.getExitStatus() == 0) {
                        status = JobStatus.OK;
                    } else {
                        status = JobStatus.FAIL;
                        error = runResponse.getError();
                    }
                }
            }
        } catch (Exception e) {
            status = JobStatus.FAIL;
        }

        return Status.OK_STATUS;
    }

}
