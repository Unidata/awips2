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
package com.raytheon.uf.common.status;

import java.util.Iterator;
import java.util.ServiceLoader;

import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * The principal mechanism for representing the outcome of an operation. <BR>
 * 
 * <BR>
 * 
 * A UFStatus can be used to report a failure, or a successful operation. The
 * final disposition of this status object is implementation specific and is
 * handled by the product's specific {@link IUFStatusHandler}. <BR>
 * 
 * <BR>
 * 
 * The UFStatus has a priority, a pluginName (if applicable), a category (user
 * specific, but check the StatusConstants for common options), a source
 * (optional), a message and a throwable (if applicable).
 * 
 * The priorities are:
 * 
 * <UL>
 * <LI><B>CRITICAL</B> (0) - Information that should be acted on immediately
 * <LI><B>SIGNIFICANT</B> (1) - May not be an "emergency" but still very
 * important
 * <LI><B>PROBLEM</B> (2) - A problem occurred
 * <LI><B>EVENTA</B> (3) - An important message, but non-crucial
 * <LI><B>EVENTB</B> (4) - Suggested reading but non-required
 * <LI><B>VERBOSE</B> (5) - Of an informational nature
 * </UL>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2008  1433       chammack    Initial creation
 * Apr 17, 2013 1786       mpduff      Allow setting of Handler Factory.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class UFStatus {

    /**
     * 
     * <UL>
     * <LI><B>CRITICAL</B> (0) - Information that should be acted on immediately
     * <LI><B>SIGNIFICANT</B> (1) - May not be an "emergency" but still very
     * important
     * <LI><B>PROBLEM</B> (2) - A problem occurred
     * <LI><B>EVENTA</B> (3) - An important message, but non-crucial
     * <LI><B>EVENTB</B> (4) - Suggested reading but non-required
     * <LI><B>VERBOSE</B> (5) - Of an informational nature
     * </UL>
     */
    public enum Priority {
        CRITICAL, SIGNIFICANT, PROBLEM, EVENTA, EVENTB, VERBOSE;

        public static final Priority FATAL = CRITICAL;

        public static final Priority ERROR = SIGNIFICANT;

        public static final Priority WARN = PROBLEM;

        public static final Priority INFO = EVENTA;

        public static final Priority DEBUG = VERBOSE;
    }

    /** The priority */
    protected final Priority priority;

    /** The exception */
    protected final Throwable exception;

    /** The message */
    protected final String message;

    /** handler factory */
    private static IUFStatusHandlerFactory handlerFactory = createHandlerFactory();

    private static final IUFStatusHandlerFactory createHandlerFactory() {
        ServiceLoader<IUFStatusHandlerFactory> loader = ServiceLoader.load(
                IUFStatusHandlerFactory.class,
                IUFStatusHandlerFactory.class.getClassLoader());
        Iterator<IUFStatusHandlerFactory> handlerIterator = loader.iterator();
        IUFStatusHandlerFactory factory = null;

        if (handlerIterator.hasNext()) {
            factory = handlerIterator.next();
        } else {
            factory = new DefaultStatusHandlerFactory();
            Exception e = new RuntimeException("No "
                    + IUFStatusHandlerFactory.class.getName()
                    + " found.\nUsing default handler.");
            factory.getInstance()
                    .handle(Priority.CRITICAL,
                            e.getLocalizedMessage()
                                    + "\nPlease ignore if you are in a unit test environment\n");
        }

        if (handlerIterator.hasNext()) {
            throw new RuntimeException("Multiple "
                    + IUFStatusHandlerFactory.class.getName()
                    + " handlers defined");
        }
        return factory;
    }

    /**
     * Constructor
     * 
     * Protected: Use
     * {@link UFStatus#handle(Priority, String, String, Throwable)} instead
     * 
     * @param priority
     * @param pluginName
     * @param message
     * 
     */
    public UFStatus(Priority priority, String message) {
        this.priority = priority;
        this.message = message;
        this.exception = null;
    }

    /**
     * Constructor
     * 
     * Protected: Use
     * {@link UFStatus#handle(com.raytheon.uf.common.status.UFStatus.Priority, String, String, Throwable)}
     * instead
     * 
     * @param priority
     * @param message
     * @param throwable
     */
    public UFStatus(Priority priority, String message, Throwable throwable) {
        this.priority = priority;
        this.exception = throwable;
        this.message = message;
    }

    /**
     * Copy constructor
     * 
     * @param status
     */
    public UFStatus(UFStatus status) {
        this.exception = status.exception;
        this.message = status.message;
        this.priority = status.priority;
    }

    /**
     * @return the priority
     */
    public Priority getPriority() {
        return priority;
    }

    /**
     * @return the exception
     */
    public Throwable getException() {
        return exception;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * Use if handler implementation recommends caching of the handler for
     * performance reasons.
     * 
     * @return
     */
    public static IUFStatusHandler getHandler() {
        return handlerFactory.getInstance();
    }

    /**
     * @return the handlerfactory
     */
    public static IUFStatusHandlerFactory getHandlerfactory() {
        return handlerFactory;
    }

    /**
     * Get a handler for the given class. '
     * 
     * @param cls
     * 
     * @return
     */
    public static IUFStatusHandler getHandler(Class<?> cls) {
        return handlerFactory.getInstance(cls);
    }

    /**
     * Get a handler for the given class and source.
     * 
     * @param cls
     * @param source
     * 
     * @return
     */
    public static IUFStatusHandler getHandler(Class<?> cls, String source) {
        return handlerFactory.getInstance(cls, source);
    }

    /**
     * Get a handler for the given pluginId and source.
     * 
     * @param pluginId
     * @param source
     * 
     * @return
     */
    public static IUFStatusHandler getHandler(String pluginId, String source) {
        return handlerFactory.getInstance(pluginId, source);
    }

    /**
     * Get a handler for the given pluginId, category, and source. This is used
     * for handlers that are initialized prior to loading the configured
     * handlers.
     * 
     * @param pluginId
     * @param source
     * 
     * @return
     */
    public static IUFStatusHandler getHandler(String pluginId, String category,
            String source) {
        return handlerFactory.getInstance(pluginId, category, source);
    }

    /**
     * Get a handler for the given class, category, and source.
     * 
     * @param cls
     * @param category
     * @param source
     * 
     * @return
     */
    public static IUFStatusHandler getHandler(Class<?> cls, String category,
            String source) {
        return handlerFactory.getInstance(cls, category, source);
    }

    /**
     * Use if handler implementation recommends caching of the handler for
     * performance reasons.
     * 
     * @param name
     *            Named handler. This is implementation specific.
     * @return
     */
    public static IUFStatusHandler getNamedHandler(String name) {
        return handlerFactory.getInstance(name);
    }

    /**
     * Use if handler implementation recommends caching of the handler for
     * performance reasons.
     * 
     * @param name
     *            Named handler. This is implementation specific.
     * @return
     */
    public static IUFStatusHandler getMonitorHandler(Class<?> cls) {
        return handlerFactory.getMonitorInstance(cls);
    }

    /**
     * Use if handler implementation recommends caching of the handler for
     * performance reasons.
     * 
     * @param name
     *            Named handler. This is implementation specific.
     * @param monitorSource
     * @return
     */
    public static IUFStatusHandler getMonitorHandler(Class<?> cls,
            String monitorSource) {
        return handlerFactory.getMonitorInstance(cls, monitorSource);
    }

    /**
     * Set the handler factory
     * 
     * @param factory
     *            the handler factory
     */
    public static void setHandlerFactory(IUFStatusHandlerFactory factory) {
        handlerFactory = factory;
    }
}
