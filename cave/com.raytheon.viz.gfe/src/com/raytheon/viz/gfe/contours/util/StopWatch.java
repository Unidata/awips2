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
package com.raytheon.viz.gfe.contours.util;

/**
 * This is a Java implementation of the StopWatch facility. Only
 * operations needed for GFE applications are included.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Mar2008    968        MW Fegan    Initial creation
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class StopWatch {
    /** 
     * time, in clock ticks - milliseconds, that the code timing began 
     */
    long startTime = 0; 
    /**
     * The total time of all the code timings in seconds.
     */
    double totalTime = 0.0;
    /**
     * The number of times the StopWatch has been used
     * for timing. Note that a timing is a start/stop
     * cycle.
     */
    int numTimings = 0;
    private double wallClockTime = 0;
//    private double userCPUTime   = 0;   // TODO: are these even available?
//    private double systemCPUTime = 0; // TODO: are these even available?
    /**
     * Flag to indicate if the stop watch is running.
     */
    private boolean running = false;
    /**
     * Constructor.
     */
    public StopWatch() {
        // this one can be empty
    }
    /* main operations */
    /**
     * Starts this stop watch. If already started, no action is taken. 
     */
    public void start() {
        if (running) {
            return;
        }
        this.startTime = System.currentTimeMillis();
        this.running = true;
    }
    /**
     * Stops this stop watch. If the watch is not running, the effect is
     * the same as calling {@link #reset()}.
     */
    public void stop() {
        /* if not running, reset accumulators and exit */
        if (!running) {
            reset();
            return;
        }
        this.running = false;
        /* get the current time */
        long elapsed = System.currentTimeMillis() - this.startTime;
        this.wallClockTime = (double)elapsed / (double)1000;
        this.totalTime += this.wallClockTime;
        this.numTimings++;
        
        /* zero out the start time */
        this.startTime = 0;
    }
    /**
     * Sets this stop watch's accumulator to zero. 
     */
    public void reset() {
        this.startTime = 0;
        this.numTimings = 0;
        this.wallClockTime = 0;
    }
    /**
     * Determines if this stop watch is running.
     */
    public boolean isRunning() {
        return this.running;
    }
    
    /* accessors */
    /**
     * Returns the current elapsed time (in seconds).
     */
    public double getSplit() {
        if (this.running) {
            /* elapsed time, but in msec */
            long elapsed = System.currentTimeMillis() - this.startTime;
            /* return elapsed time in sec */
            return (double)elapsed / (double)1000;
        }
        return this.wallClockTime;
    }
    /**
     * Gets the average time per timing. Returns zero if this stop watch
     * is currently running.
     */
    public double getAvgWallClockTime() {
        return (this.running)?0.0:(this.totalTime/this.numTimings);
    }
    /**
     * Gets the wall clock time for this stop watch.
     */
    public double getWallClockTime() {
        return this.wallClockTime;
    }
//    public double getUserCPUTime() {
//        return userCPUTime;
//    }
//    public double getSystemCPUTime() {
//        return systemCPUTime;
//    }
    /* operations */
    @Override
    public boolean equals(Object rhs) {
        if (!(rhs instanceof StopWatch)) {
            return false;
        }
        StopWatch sw = (StopWatch)rhs;
        return this.startTime == sw.startTime &&
               this.totalTime == sw.totalTime &&
               this.numTimings == sw.numTimings;
    }
}
