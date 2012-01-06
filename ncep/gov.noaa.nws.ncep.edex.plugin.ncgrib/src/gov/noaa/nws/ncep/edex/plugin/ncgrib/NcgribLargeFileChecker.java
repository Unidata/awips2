package gov.noaa.nws.ncep.edex.plugin.ncgrib;

import java.io.File;
import java.net.InetAddress;
import java.net.UnknownHostException;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * Implementation of a Camel Message Processor to check if the ingested ncgrib
 * file is larger than 8Mb, all other ncgrib ingest threads will block to wait for
 * this file to finish processing. Cluster locking is used. The lock is obtained
 * in this Processor. A second processor, NcgribLockRelease, is used to release
 * the lock once the file is done being processed.
 * <p>
 * This processor was put in place to ensure large ncgrib files will not throw out
 * of memory exceptions while processing.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/08/11                 Xilin Guo   Initial Creation
 * 
 * </pre>
 * 
 * @author xguo
 * @version 1
 * @see gov.noaa.nws.ncep.edex.plugin.ncgrib.NcgribLockRelease
 */
public class NcgribLargeFileChecker implements Processor {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(NcgribLargeFileChecker.class);

    /**
     * The header attribute attached to the message to denote whether this file
     * will receive single threaded handling
     */
    public static final String LARGE_FILE_HEADER = "ncgribLargeFile";

    /** The cluster task name used for cluster locking */
    public static final String CLUSTER_TASK_NAME = "NcgribIngestLargeFile";

    public static final String CLUSTER_TASK_DETAILS;

    /**
     * The default large file size is 10Mb.
     */
    private static long LARGE_FILE_SIZE = 10485760L;
    
    /**
     * The default timing out of large file is 240000 milliseconds
     */
    private static long LARGE_FILE_WAITING_TIME = 240000L;
        
    static {
        String host = null;
        try {
            // Initialize the cluster task name with the host name
            host = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            e.printStackTrace();
            host = "";
        }
        CLUSTER_TASK_DETAILS = host;
    }

    @Override
    public void process(Exchange exchange) throws Exception {
        boolean isLargeFile = false;

        File gribFile = (File) exchange.getIn().getBody();
        
        if ( gribFile.length() > LARGE_FILE_SIZE ) {
        	isLargeFile = true;
        }

        ClusterTask task = ClusterLockUtils.lookupLock(CLUSTER_TASK_NAME,
                CLUSTER_TASK_DETAILS);

        boolean waitForLargeNcgrib = task.isRunning();
        while (waitForLargeNcgrib) {
            Thread.sleep(500);
            task = ClusterLockUtils.lookupLock(CLUSTER_TASK_NAME,
                    CLUSTER_TASK_DETAILS);
            waitForLargeNcgrib = task.isRunning();
            // need to handle timing out of large ncgrib file process manually
            if (waitForLargeNcgrib
                    && (System.currentTimeMillis() - task.getLastExecution()) > LARGE_FILE_WAITING_TIME) {

            	statusHandler.handle(Priority.EVENTA,
            		"Large Ncgrib file process timed out.  Clearing lock and resuming processing");
            	ClusterLockUtils
            		.unlock(CLUSTER_TASK_NAME, CLUSTER_TASK_DETAILS);
            	waitForLargeNcgrib = false;
            }
        }

        if (isLargeFile) {
            statusHandler.handle(Priority.EVENTA,
                    "Large Ncgrib file detected.  Establishing lock.");
            while (!ClusterLockUtils
                    .lock(CLUSTER_TASK_NAME, CLUSTER_TASK_DETAILS, LARGE_FILE_WAITING_TIME, true)
                    .getLockState().equals(LockState.SUCCESSFUL)) {
                Thread.sleep(100);
            }
            statusHandler.handle(Priority.EVENTA,
                    "Large Ncgrib file lock established.");
            // Wait for other threads to complete processing before we proceed
            Thread.sleep(1000);
            exchange.getIn().setHeader(LARGE_FILE_HEADER, true);

        } else {
            exchange.getIn().setHeader(LARGE_FILE_HEADER, false);
        }
    }
}
