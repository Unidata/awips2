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

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthBucket;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthBucketDao;
/**
 * Registry Bandwidth Service.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2013  1736     dhladky      Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public class RegistryBandwidthService {
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryBandwidthService.class);
   
    public static final int BYTES_PER_KILOBYTE = 1024;
    
    private Network network;
    
    private IBandwidthBucketDao bucketDao;
    
    private int bucketSize;
            
    public RegistryBandwidthService() {
        
    }
    
    /**
     * Construct an instance
     * @param bucketDao
     * @param network
     */
    public RegistryBandwidthService(IBandwidthBucketDao bucketDao, Network network, int bucketSize) {
        this.bucketDao = bucketDao;
        this.network = network;
        this.bucketSize = bucketSize;
    }
    
    /**
     * Gives a time averaged bandwidth for the current time.
     * @return
     */
    public Integer getCurrentRegistryBandwidth() {
        
        if (network == Network.OPSNET) {
            
            RegistryBandwidthRecord rbr = getCurrentRegistryBandwidthRecord();

            if (rbr != null) {
                // convert to kb per/second
                return convertBytesToKilobytes(rbr.getBytes());
            } else {
                statusHandler
                        .handle(Priority.WARN,
                                "No active registry bandwidth information for current time.");
            }
        } 
        
        return 0;
        
    }
    
    /**
     * Gives the current full record.  Which is the previous record time wise
     * because the query is back one full bucketSize in millis.
     * @return
     */
    public RegistryBandwidthRecord getCurrentRegistryBandwidthRecord() {

        RegistryBandwidthRecord rbr = null;

        if (network == Network.OPSNET) {

            RegistryBandwidthDao rbd = new RegistryBandwidthDao();
            Calendar cal = TimeUtil.newGmtCalendar();
            Long timePeriodKey = getTimePeriodKey(cal);

            if (timePeriodKey != null) {
                try {
                    long startMillis = timePeriodKey - bucketSize;
                    long endMillis = timePeriodKey;
                    rbr = rbd.queryByTimeRange(startMillis, endMillis);
                } catch (DataAccessLayerException dale) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not lookup Registry Bandwidth Record! ",
                            dale);
                }
            }
        }

        return rbr;
    }
    
    /**
     * Gives a time averaged bandwidth utilization for the registry, time passed
     * in.
     * 
     * @param cal
     * @return
     */
    public Integer getRegistryBandwidth(long millis) {

        if (network == Network.OPSNET) {

            Calendar cal = TimeUtil.newGmtCalendar();
            cal.setTimeInMillis(millis);
            RegistryBandwidthRecord rbr = getRegistryBandwidthRecord(cal);

            if (rbr != null) {
                return convertBytesToKilobytes(rbr.getBytes());
            } else {
                // No record for this bucket,
                // try current
                return getCurrentRegistryBandwidth();
            }
        } 
            
        return 0;
    }
    
    /**
     * Retrieve a registry bandwidth record
     * @param cal
     * @return
     */
    public RegistryBandwidthRecord getRegistryBandwidthRecord(Calendar cal) {

        RegistryBandwidthDao rbd = new RegistryBandwidthDao();
        Long timePeriodKey = getTimePeriodKey(cal);
        RegistryBandwidthRecord rbr = null;

        if (timePeriodKey != null) {
            try {
                long startMillis = timePeriodKey - bucketSize/2;
                long endMillis = timePeriodKey + bucketSize/2;
                rbr = rbd.queryByTimeRange(startMillis, endMillis);
            } catch (DataAccessLayerException dale) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not lookup Registry Bandwidth Record! ", dale);
            }
        }
        
        return rbr;
    }
    
    /**
     * Retrieve a registry bandwidth record
     * @param cal
     * @return
     */
    public RegistryBandwidthRecord getRegistryBandwidthRecord(long millis) {

        Calendar cal = TimeUtil.newGmtCalendar();
        cal.setTimeInMillis(millis);
        
        return getRegistryBandwidthRecord(cal);
    }
        
    /**
     * Get time period key for some other determined time
     * @param cal
     * @return
     */
    public Long getTimePeriodKey(Calendar cal) {
        
        long millis = cal.getTimeInMillis();
        BandwidthBucket bucket = bucketDao.getBucketContainingTime(millis,
                network);

        if (bucket != null) {
            return getTimeKey(bucket.getBucketStartTime());
        }

        // in the off chance a bucket doesn't exist anywhere near this time.
        return null;
    }
    
    /**
     * conversion
     * @param bytes
     * @return
     */
    public static int convertBytesToKilobytes(int bytes) {
        return bytes / BYTES_PER_KILOBYTE;
    }
    
    /**
     * Add or update the record
     * @param rbr
     */
    public void addorUpdateRecord(RegistryBandwidthRecord rbr) {
        RegistryBandwidthDao rbd = new RegistryBandwidthDao();

        try {
            if (rbr != null) {
                rbd.addOrUpdateRecord(rbr);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Could not write Registry Bandwidth Record! ", e);
        }
    }

    /**
     * Records in the DB are kept by millis on a one day cycle
     * @param current
     * @return
     */
    private long getTimeKey(long current) {

        long value = current % TimeUtil.MILLIS_PER_DAY;

        if (value < 0) {
            return 0;
        } else {
            return value;
        }
    }

}
