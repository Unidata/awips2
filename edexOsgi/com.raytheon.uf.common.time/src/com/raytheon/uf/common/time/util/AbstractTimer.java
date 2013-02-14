package com.raytheon.uf.common.time.util;

import com.raytheon.uf.common.time.domain.Durations;
import com.raytheon.uf.common.time.domain.api.IDuration;
import com.raytheon.uf.common.time.domain.api.ITimePoint;

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
 * Jan 14, 2013 1286       djohnson     Use time domain API.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
abstract class AbstractTimer implements ITimer {
    private ITimePoint start;

    private ITimePoint stop;

    private IDuration elapsedTime = Durations.ZERO;

    /**
     * {@inheritDoc}
     */
    @Override
    public void start() {
        if (isTimerStopped()) {
            elapsedTime = elapsedTime.plus(Durations.between(start, stop));
            stop = null;
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
        return getElapsed().getMillis();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IDuration getElapsed() {
        ITimePoint currentOrStopTime = (isTimerRunning()) ? getCurrentTime()
                : stop;
        if (currentOrStopTime == null && start == null) {
            return Durations.ZERO;
        }
        IDuration currentRun = Durations.between(start, currentOrStopTime);
        return currentRun.plus(elapsedTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void reset() {
        start = null;
        stop = null;
        elapsedTime = Durations.ZERO;
    }

    protected abstract ITimePoint getCurrentTime();

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
        return start != null;
    }

    /**
     * Check whether the timer is stopped.
     * 
     * @return true if the timer is stopped
     */
    private boolean isTimerStopped() {
        return stop != null;
    }
}
