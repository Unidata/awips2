package com.raytheon.uf.edex.datadelivery.bandwidth.registry;

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

import java.util.Calendar;
import java.util.concurrent.atomic.AtomicInteger;

import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthBucketDao;

/**
 * Process the totals gathered by the BandwidthUtilizationListener class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 27, 2013 1736       dhladky     Initial creation
 * Jan 08, 2014 2615       bgonzale    Change calculation of bytesPerSec to make a divide by zero error less likely.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class BandwidthUtilizationProcessor implements Job {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BandwidthUtilizationProcessor.class);
    
    /** total bytes saved in map */
    private AtomicInteger totalBytes = null;
    /** long last run time */
    private long lastRun = 0l;
    /** job data map pertinent to this execution */
    private JobDataMap map = null;
    /** network of the traffic be tracked */
    private Network network = null;
    /** bucket Dao class */
    private IBandwidthBucketDao bucketDao = null;
    /** bucket size in mins */
    private int bucketSize = 0;
    
    public BandwidthUtilizationProcessor() {
        
    }
    
    @Override
    public void execute(JobExecutionContext context) throws JobExecutionException {
        
        this.map = context.getJobDetail().getJobDataMap();
        this.network = (Network) map.get("network");
        this.lastRun = map.getLongValue("lastRun");
        this.totalBytes = (AtomicInteger) map.get("totalBytes");
        this.bucketDao = (IBandwidthBucketDao) map.get("bucketDao");
        this.bucketSize = map.getInt("bucketSize");

        // do the processing
        processRecord();
    }
    
    /**
     * Gets the bytes per second for the interval run
     * @return bytesPerSec
     */
    private int getBytesPerSecondAndReset() {
        
        long now = TimeUtil.currentTimeMillis();
        int diffMilliSeconds = (int) (now - lastRun);
        int bytesPerMillSec = totalBytes.getAndSet(0) / diffMilliSeconds;
        int bytesPerSec = bytesPerMillSec / 1000;
        // reset time
        map.put("lastRun", now);
      
        return bytesPerSec;
    }
    
    /**
     * Process the record and save to DB
     */
    private synchronized void processRecord() {
        
        RegistryBandwidthService service = new RegistryBandwidthService(bucketDao, network, bucketSize);
        RegistryBandwidthRecord rbr = null;
        
        try {
            rbr = service.getCurrentRegistryBandwidthRecord();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Could not lookup previous Registry Bandwidth Record! ", e);
        }

        int nowBytes = getBytesPerSecondAndReset();
        
        if (rbr != null) {
            // get the previous bytes/per second value
            int previousBytes = rbr.getBytes();
            int averageBytes = (previousBytes + nowBytes)/2;
            rbr.setBytes(averageBytes);
            
        } else {
            // brand spanking new
            Calendar cal = TimeUtil.newGmtCalendar();
            Long timePeriodKey = service.getTimePeriodKey(cal);
            
            if (timePeriodKey != null) {
                rbr = new RegistryBandwidthRecord(timePeriodKey, nowBytes);
            }
        }

        service.addorUpdateRecord(rbr);
    }
    

}
