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

/**
 * 
 */
package com.raytheon.edex.util;

/**
 * JMX MBean interface for AbstractMessageSrv. This interface specifies the
 * common methods for JMX instrumenting classes that derive from AbstractMessageSrv.
 * <P>
 * To enable JMX instrumentation, any class that extends {@code AbstractMessageSrv}
 * must inplement an interface that extends this interface. In addition, the class
 * and interface must be defined as follows:
 * <PRE>
 *    {@code public interface MyClassMBean}
 *    {@code extends AbstractSrvMBean} {
 *    }
 *    
 *    {@code public class MyClass }
 *    {@code extends AbstractMessageSrv}
 *    {@code implements AbstractSrvMBean} {
 *    }
 * </PRE>
 * The naming convention shown here must be followed. If no methods beyond those
 * provided by this interface are required, the derived interface may be empty.
 * <P>
 * This interface provides the basic instrumentation as follows:
 * <DL>
 * <DT>{@link #getExecCount()}
 * <DD>gets the current number of times the implementing class' 
 *     {@link com.raytheon.edex.util.AbstractMessageSrv#process() process()} method
 *     has been been called.
 * <DT>{@link #clearExecCount()}
 * <DT>clears the execution counter.
 * <DT>{@link #getExecTime()}
 * <DD>gets the total time the implementing class'
 *     {@link com.raytheon.edex.util.AbstractMessageSrv#process() process()} method
 *     has spent processing messages.
 * <DT>{@link #clearExecTime()}
 * <DD>clears the execution time accumulator.
 * </DL>  
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 14Jun2006                GArmendariz Initial check in.
 * 15Aug2006    #18         MW Fegan    Added methods for execution
 *                                       time accumulator. Added
 *                                       documentation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */
public interface AbstractSrvMBean {
    /**
     * Gets the number of times {@code process()} has executed.
     * 
     * @return number of executions
     */
    public int getExecCount();
    /**
     * Clears the execution counter.
     */
    public void clearExecCount();
    /**
     * Gets the total execution time.
     * 
     * @return the total execution time.
     */
    public long getExecTime();
    /**
     * Clears the total execution time.
     *
     */
    public void clearExecTime();
}
