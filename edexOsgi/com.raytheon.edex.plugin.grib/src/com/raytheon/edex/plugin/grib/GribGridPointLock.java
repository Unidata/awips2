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

import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.camel.Body;
import org.apache.camel.Headers;

import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Object for tracking the number of grid points currently being processed by
 * the grib decode route. Grid points is used as an approximate measure of the
 * amount of memory needed to decode the file, limiting the total number of grid
 * points keeps memory usage more consistent. Before a file can be decoded the
 * message should pass through reserve to ensure the grib decoder has enough
 * free points. After decode the points should be released.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 09, 2013  2402     bsteffen    Initial creation
 * Sep 14, 2015  4868     rjpeter     Fix comment spelling.
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GribGridPointLock {

    public static final String GRID_POINT_COUNT_HEADER = "gridPointCount";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribGridPointLock.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("");

    private final long maxConcurrentGridPoints;

    private final long fairConcurrentGridPoints;

    private final AtomicLong currentPoints = new AtomicLong();

    private final Queue<Object> waiters = new ConcurrentLinkedQueue<Object>();

    public GribGridPointLock(long maxConcurrentGridPoints, int decodeThreadCount) {
        this.maxConcurrentGridPoints = maxConcurrentGridPoints;
        this.fairConcurrentGridPoints = maxConcurrentGridPoints
                / decodeThreadCount;
    }

    /**
     * Reserve a message for decode. This method will wait until there are
     * enough grid points available. Grid points are reserved until release is
     * called.
     * 
     * @param message
     *            the message to decode
     * @param headers
     *            used to store information used in release.
     */
    public void reserve(@Body GribDecodeMessage message,
            @Headers Map<String, Object> headers) {
        long gridPoints = message.getGridPointCount();
        if (gridPoints > maxConcurrentGridPoints) {
            statusHandler
                    .handle(Priority.EVENTA,
                            String.format(
                                    "Large grib file requires exclusive access: (%d > %d): %s",
                                    gridPoints, maxConcurrentGridPoints,
                                    message.getFileName()));
            gridPoints = maxConcurrentGridPoints;
        } else if (gridPoints > fairConcurrentGridPoints) {
            statusHandler
                    .handle(Priority.EVENTA,
                            String.format(
                                    "Large grib file is using many grid points: (%d of %d): %s",
                                    gridPoints, maxConcurrentGridPoints,
                                    message.getFileName()));
        }
        reserve(gridPoints);
        headers.put(GRID_POINT_COUNT_HEADER, gridPoints);
        headers.put("dequeueTime", System.currentTimeMillis());

    }

    /**
     * Release grid points after decode.
     * 
     * @param headers
     *            containing information set in reserve
     */
    public void release(@Headers Map<String, Object> headers) {
        Object gridPointsHeader = headers.get(GRID_POINT_COUNT_HEADER);
        if (gridPointsHeader instanceof Number) {
            long gridPoints = ((Number) gridPointsHeader).longValue();
            release(gridPoints);
        }
    }

    private void reserve(long gridPoints) {
        if (!fastReserve(gridPoints, false)) {
            ITimer timer = TimeUtil.getTimer();
            timer.start();
            Object waiter = new Object();
            synchronized (waiter) {
                waiters.offer(waiter);
                while ((waiters.peek() != waiter)
                        || !fastReserve(gridPoints, true)) {
                    try {
                        waiter.wait(15000);
                    } catch (InterruptedException e) {
                        ;
                    }
                }
            }
            waiters.remove(waiter);
            notifyNext();
            timer.stop();
            perfLog.logDuration("Grib: Time waiting to reserve grid points",
                    timer.getElapsedTime());
        }
    }

    private boolean fastReserve(long gridPoints, boolean ignoreWaiters) {
        long oldPoints = currentPoints.get();
        long newPoints = oldPoints + gridPoints;
        while ((ignoreWaiters || waiters.isEmpty())
                && (newPoints <= maxConcurrentGridPoints)) {
            if (currentPoints.compareAndSet(oldPoints, newPoints)) {
                return true;
            }
            oldPoints = this.currentPoints.get();
            newPoints = oldPoints + gridPoints;
        }
        return false;
    }

    private void release(long gridPoints) {
        currentPoints.addAndGet(-gridPoints);
        notifyNext();
    }

    private void notifyNext() {
        Object next = waiters.peek();
        if (next != null) {
            synchronized (next) {
                next.notifyAll();
            }
        }
    }

}
