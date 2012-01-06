package gov.noaa.nws.ncep.edex.plugin.ncgrib;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;

/**
 * Releases lock obtained in the NcgribLargeFileChecker class if one was
 * established.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/08/11                  Xilin Guo  Initial Creation
 * 
 * </pre>
 * 
 * @author xguo
 * @version 1
 * @see gov.noaa.nws.ncep.edex.plugin.ncgrib.NcgribLargeFileChecker
 */
public class NcgribLockRelease implements Processor {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(NcgribLockRelease.class);

    @Override
    public void process(Exchange exchange) throws Exception {
        Boolean isLargeFile = (Boolean) exchange.getIn().getHeader(
                NcgribLargeFileChecker.LARGE_FILE_HEADER);
        if (isLargeFile) {
            boolean success = ClusterLockUtils.unlock(
                    NcgribLargeFileChecker.CLUSTER_TASK_NAME,
                    NcgribLargeFileChecker.CLUSTER_TASK_DETAILS);
            if (success) {
                statusHandler.handle(Priority.EVENTA,
                        "Large Ncgrib file lock released!");
            } else {
                statusHandler.handle(Priority.CRITICAL,
                        "Large Ncgrib file lock could not be released!!");
            }
        }
    }
}

