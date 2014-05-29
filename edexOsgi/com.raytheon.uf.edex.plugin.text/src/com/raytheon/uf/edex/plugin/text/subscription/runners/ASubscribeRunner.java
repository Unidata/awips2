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
package com.raytheon.uf.edex.plugin.text.subscription.runners;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;

/**
 * Abstract base class for all subscription runners. Extending classes must
 * implement the {@link #execute()} method. Default implementations of other
 * methods specified in {@link ISubscribeRunner} are provided.
 * <P>
 * Also provided are two factory methods (
 * {@link ASubscribeRunner#getInstance(String)} and
 * {@link ASubscribeRunner#getInstance(String, Message)}) may be used to
 * instantiate the known concrete implementations.
 * <P>
 * Expected usage:
 * 
 * <PRE>
 * <CODE>
 *     String type = "...";  // name of an appropriate runner action
 *     Message message = null;  // initialized later...
 *     
 *     List<Property> results = null;
 *     SubscribeAction action = SubscribeAction.translate(type);
 *     try {
 *        ISubscribeRunner runner = ASubscribeRunner.getInstance(type);
 *        runner.initialize(message);
 *        runner.execute();
 *        results = runner.getResults();
 *     } catch (EdexException e) {
 *        // handle the exception
 *     }
 * </CODE>
 * </PRE>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation
 * May 22, 2014 2536       bclement    moved from autobldsrv to edex.plugin.text
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public abstract class ASubscribeRunner implements ISubscribeRunner {
    /**
     * the logger
     */
    protected final Log logger = LogFactory.getLog(getClass());

    protected Message message = null;
    
    protected List<Property> results = null;
    /**
     * Constructor. Does not initialize the message to process. Must
     * be followed by a call to {@link #initialize(message)}.
     */
    protected ASubscribeRunner() {
        this(null);
    }
    /**
     * Constructor. This version combines object construction and
     * initialization.
     * 
     * @param message the message object to process
     */
    protected ASubscribeRunner(Message message) {
        super();
        this.initialize(message);
    }
    
    @Override
    public abstract boolean execute();

    @Override
    public void initialize(Message message) {
        this.results = new ArrayList<Property>();
        this.message = message;
    }
    @Override
    public List<Property> getResults() {
        return this.results;
    }


}
