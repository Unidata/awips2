package com.raytheon.uf.common.time.util;


/**
 * 
 * Provides the basic {@link ITimer} implementation, such as the state machine
 * functionality.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
// @NotThreadSafe
abstract class AbstractTimer implements ITimer {
    private long start;

    private long stop;

    private long elapsedTime;

    /**
     * {@inheritDoc}
     */
    @Override
    public void start() {
        if (isTimerStopped()) {
            elapsedTime += (stop - start);
            stop = 0;
        } else if (isTimerStarted()) {
            throw new IllegalStateException(
                    "A timer that is running must be stopped before start() is called again!");
        }
        this.start = getCurrentTime();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void stop() {
        if (!isTimerStarted()) {
            throw new IllegalStateException(
                    "Timer must be started before it can be stopped!");
        }
        // If the timer has already been stopped, don't change the time
        if (!isTimerStopped()) {
            this.stop = getCurrentTime();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getElapsedTime() {
        long currentOrStopTime = (isTimerRunning()) ? getCurrentTime() : stop;
        return (currentOrStopTime - start) + elapsedTime;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void reset() {
        start = 0;
        stop = 0;
        elapsedTime = 0;
    }

    protected abstract long getCurrentTime();

    /**
     * Check whether the timer is actively running.
     * 
     * @return true if the timer is running
     */
    private boolean isTimerRunning() {
        return isTimerStarted() && !isTimerStopped();
    }

    /**
     * Check whether the timer was started.
     * 
     * @return true if the timer was started
     */
    private boolean isTimerStarted() {
        return start > 0;
    }

    /**
     * Check whether the timer is stopped.
     * 
     * @return true if the timer is stopped
     */
    private boolean isTimerStopped() {
        return stop > 0;
    }
}
