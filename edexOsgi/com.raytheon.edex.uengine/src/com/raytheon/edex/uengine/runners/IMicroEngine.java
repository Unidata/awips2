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
package com.raytheon.edex.uengine.runners;

import java.util.List;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;

/**
 * @deprecated MicroEngine, aka uEngine, is deprecated.  Use IServerRequest/IRequestHandler
 * framework instead.
 * 
 * The public interface for all &mu;Engine script runners. It provides the required
 * operations to run a script. These operations are {@link #initialize()},
 * {@link #setScript(String)}, {@link #execute()}, {@link #getResult()}, and
 * {@link #release()}. There are two convenience methods: {@link #executeScript(String)},
 * which combines this all functionality into a single method call, and
 * {@link #initialize(String)}, which combines providing the script and initialization
 * of the script runner into a single operation.
 * <P>
 * Normal usage:
 * <pre><code>
 *    List&lt;?&gt; result = null;
 *    try {
 *       runner.setScript(script);
 *       runner.initialize();
 *       runner.execute();
 *       result = runner.getResult();
 *    catch (MicroEngineException e) {
 *       // handle the exception
 *    } finally {
 *       runner.release();
 *    }
 *    // process results  
 * </code></pre> 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12Nov2008    1709       MW Fegan    Initial Creation.
 * 25May2011    8686       cjeanbap    Add setter method.
 * 25Jun2015    4495       njensen     Deprecated
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

@Deprecated
public interface IMicroEngine {
    /**
     * Executes a previously set &mu;Engine script. Implementations of
     * this method must save execution results so they can be retrieved
     * via {@link #getResult()}.
     * 
     * @throws MicroEngineException if any error occurs
     */
    public void execute() throws MicroEngineException;

    /**
     * Executes the specified &mu;Engine script. This is a convenience method
     * that includes initialization and release operations on the script runner
     * as well as execution the script.
     * <p> 
     * In general,
     * <pre><code>
     *    List&lt;?&gt; result = null;
     *    result = runner.executeScript(script);
     * </code></pre>
     * is equivalent to
     * <pre><code>
     *    List&lt;?&gt; result = null;
     *    runner.setScript(script);
     *    runner.initialize();
     *    runner.execute();
     *    runner.release();
     *    result = runner.getResult();
     * </code></pre>
     * 
     * @param script the &mu;Engine script to execute
     * @return the result of execution the script
     * @throws MicroEngineException if any error occurs
     */
    public List<AbstractResponseMessage> executeScript(String script)
            throws MicroEngineException;

    /**
     * Returns the results from running a &mu;Engine script. Classes that implement
     * this interface should ensure that a valid, although possibly empty, list
     * is returned. Any system resources that are utilized in running the script
     * should be obtained in this method.
     */
    public List<AbstractResponseMessage> getResult();

    /**
     * Initializes the &mu;Engine script runner. Initialization should not be
     * dependent on having set a script; it should only get the scripting engine
     * read to use.
     * 
     * @throws MicroEngineException if any error occurs
     */
    public void initialize() throws MicroEngineException;

    /**
     * Initializes the &mu;Engine script runner. This version combines
     * {@link #setScript(String) setting the script} with 
     * {@link #initialize() initializing the script runner}.
     * 
     * @param script the script to execute.
     * @throws MicroEngineException if any error occurs
     */
    public void initialize(String script) throws MicroEngineException;

    /**
     * Releases any resources utilized by the &mu;Engine script runner. Generally,
     * this method should release resources allocated in {@link #initialize()}.
     * In particular, it should not expect the client to retrieve results prior to
     * execution.
     * <P>
     * In case of failure, this method should log the failure and  fail silently.
     */
    public void release();

    /**
     * Provides the script to be executed.
     */
    public void setScript(String script);

    public void setTrigger(String triggerId);
}
