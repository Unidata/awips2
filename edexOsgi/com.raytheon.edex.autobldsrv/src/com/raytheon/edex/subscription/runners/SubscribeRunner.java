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
package com.raytheon.edex.subscription.runners;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Factory class for creating Subscription Runner objects.
 * <P>
 * Two factory methods ({@link ASubscribeRunner#getInstance(String)}
 * and {@link ASubscribeRunner#getInstance(String, Message)}) are used to instantiate
 * the known concrete implementations.
 * <P>
 * Expected usage:
 * <PRE><CODE>
 *     String type = "...";  // name of an appropriate runner action
 *     Message message = null;  // initialized later...
 *     
 *     List<Property> results = null;
 *     try {
 *        ISubscribeRunner runner = SubscribeRunner.getInstance(type);
 *        runner.initialize(message);
 *        runner.execute();
 *        results = runner.getResults();
 *     } catch (EdexException e) {
 *        // handle the exception
 *     }
 * </CODE></PRE>
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18Nov2008    1709       MW Fegan    Initial creation
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class SubscribeRunner {
    /**
     * the logger
     */
    @SuppressWarnings("unused")
    private final Log logger = LogFactory.getLog(getClass());
    
    /*
     * The factory instance.
     */
    private static SubscribeRunner instance = null;
    /**
     * Constructor.
     */
    private SubscribeRunner() {
        super();
    }
    /**
     * 
     * @return
     */
    public static synchronized SubscribeRunner getInstance() {
        if (null == instance) {
            instance = new SubscribeRunner();
        }
        return instance;
    }
    /**
     * Factory method for obtaining an {@link ISubscribeRunner} instance for the specified
     * operation. The {@link ISubscribeRunner} instance must be initialized via the 
     * {@link ISubscribeRunner#initialize(Message) initialize(Message)} method.
     * 
     * @param oper the operation for the instance to execute
     * 
     * @return the requested class instance
     * 
     * @throws EdexException if any error occurs in creating the class instance
     */
    public static ISubscribeRunner getInstance(String oper) throws EdexException {
        return getInstance(oper, null);
    }
    /**
     * Factory method for obtaining an {@link ISubscribeRunner} instance for the specified
     * operation to process the specified message. If <code>message</code> is <code>null</code>,
     * the {@link ISubscribeRunner} instance must be initialized via the 
     * {@link ISubscribeRunner#initialize(Message) initialize(Message)} method.
     * 
     * @param message the message to process
     * 
     * @return the requested class instance
     * 
     * @throws EdexException if any error occurs in creating the class instance
     */
    public static ISubscribeRunner getInstance(String oper, Message message) throws EdexException {
        ISubscribeRunner retVal = null;
        if (Util.isEmptyString(oper)) {
            throw new EdexException("Unable to initialize ISubscribeRunner instance; null or empty operation specified - unable to continue");
        }
        SubscribeAction action = SubscribeAction.translate(oper);
        String className = action.getRunner();
        if (className == null) {
            throw new EdexException("Unable to initialize ISubscribeRunner instance; invalid operation [" + oper + "] specified - unable to continue");
        }
        
        Class<?> aClass = null;
        try {
            aClass = Class.forName(className);
            retVal = (ISubscribeRunner)aClass.newInstance();
            if (message != null) {
                retVal.initialize(message);
            }
        } catch (Exception e) {
            throw new EdexException("Unable to initialize ISubscribeRunner instance; invalid operation [" + oper + "] specified - unable to continue",e);
        }
        return retVal;
    }

    /**
     * 
     * An enumeration of Subscription Actions. This enumeration is used by the factory methods
     * ({@link ASubscribeRunner#getInstance(String, Message)} and 
     * {@link ASubscribeRunner#getInstance(String)}) to determine the correct class to
     * instantiate to create a worker.
     *  
     * @author mfegan
     * @version 1.0
     */
    public enum SubscribeAction {
        ACTION_ADD,ACTION_READ,ACTION_DELETE,ACTION_UPDATE,ACTION_QUERY;
        /**
         * A mapping of action names to SubscribeAction objects.
         */
        private static final Map<String, SubscribeAction> actions = new HashMap<String, SubscribeAction>() {
            private static final long serialVersionUID = 1L;
            {
                put("add",ACTION_ADD);
                put("read",ACTION_READ);
                put("delete",ACTION_DELETE);
                put("update",ACTION_UPDATE);
                put("query",ACTION_QUERY);
            }
        };
        /**
         * A mapping of SubscribeAction objects to implementation class names.
         */
        private static final Map<SubscribeAction, String> runners = new HashMap<SubscribeAction, String>() {
            private static final long serialVersionUID = 1L;
            {
                put(ACTION_ADD,"com.raytheon.edex.subscription.runners.SubscribeAddRunner");
                put(ACTION_READ,"com.raytheon.edex.subscription.runners.SubscribeReadRunner");
                put(ACTION_DELETE,"com.raytheon.edex.subscription.runners.SubscribeDeleteRunner");
                put(ACTION_UPDATE,"com.raytheon.edex.subscription.runners.SubscribeUpdateRunner");
                put(ACTION_QUERY,"com.raytheon.edex.subscription.runners.SubscribeQueryRunner");
            }
        };
        /**
         * Constructor.
         */
        private SubscribeAction(){}
        /**
         * Factory method that gets the SubscribeAction object for an action name.
         * 
         * @param action name of the requested action
         * 
         * @return the corresponding action
         */
        public static final SubscribeAction translate(String action) {
            return actions.get(action.toLowerCase());
        }
        /**
         * Returns the class name of the runner associated with the action.
         */
        public final String getRunner() {
            return runners.get(this);
        }
    }
}
