/*
 * gov.noaa.nws.ncep.viz.ui.remotescript.job.IRemoteJobObserver
 * 
 * 26 March 2014
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.ui.remotescript.job;

/**
 * Interface for remote job observer
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

public interface IRemoteJobObserver {

    public void updateRemoteJob(RemoteScriptJob job);

    public void addRemoteJob(RemoteScriptJob job);
}
