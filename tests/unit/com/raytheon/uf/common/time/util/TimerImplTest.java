package com.raytheon.uf.common.time.util;


/**
 * 
 * Test {@link TimerImpl}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2012 0743       djohnson     Initial creation
 * Jun 14, 2013 2095       djohnson     Extracted contents to {@link AbstractTimerTest}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class TimerImplTest extends AbstractTimerTest<TimerImpl> {

    /**
     * {@inheritDoc}
     */
    @Override
    protected TimerImpl getTimer() {
        return new TimerImpl();
    }

}
