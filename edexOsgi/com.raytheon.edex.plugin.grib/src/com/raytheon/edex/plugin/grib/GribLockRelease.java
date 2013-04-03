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
package com.raytheon.edex.plugin.grib;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;

/**
 * Releases lock obtained in the GribLargeFileChecker class if one was
 * established.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/15/10     6644        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 * @see com.raytheon.edex.plugin.grib.GribLargeFileChecker
 */
public class GribLockRelease implements Processor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribLockRelease.class);

    @Override
    public void process(Exchange exchange) throws Exception {
        Boolean isLargeFile = (Boolean) exchange.getIn().getHeader(
                GribLargeFileChecker.LARGE_FILE_HEADER);
        if (isLargeFile) {
            boolean success = ClusterLockUtils.unlock(
                    GribLargeFileChecker.CLUSTER_TASK_NAME,
                    GribLargeFileChecker.CLUSTER_TASK_DETAILS);
            if (success) {
                statusHandler.handle(Priority.EVENTA,
                        "Large Grib file lock released!");
            } else {
                statusHandler.handle(Priority.CRITICAL,
                        "Large Grib file lock could not be released!!");
            }
        }
    }
}
