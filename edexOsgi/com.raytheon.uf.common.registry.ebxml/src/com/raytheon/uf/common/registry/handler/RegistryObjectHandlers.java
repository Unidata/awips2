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
package com.raytheon.uf.common.registry.handler;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.registry.annotations.RegistryObject;

/**
 * Register or find {@link IRegistryObjectHandler} implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012 1169       djohnson     Initial creation
 * Oct 16, 2012 0726       djohnson     Add the ability to use a pojo instance which delegates to static methods.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class RegistryObjectHandlers {

    private static final ConcurrentMap<String, IRegistryObjectHandler<?>> handlers = new ConcurrentHashMap<String, IRegistryObjectHandler<?>>();

    // Used to simplify the Spring files
    private static final RegistryObjectHandlers INSTANCE = new RegistryObjectHandlers();

    /**
     * Prevent construction.
     */
    private RegistryObjectHandlers() {
    }

    /**
     * Calls through to
     * {@link RegistryObjectHandlers#register(Class, IRegistryObjectHandler)}.
     * Used to simplify Spring files.
     * 
     * @param <T>
     *            the registry object handler interface type
     * @param handlerInterface
     *            the handler interface
     * @param handler
     *            the handler implementation
     * @return {@link #getInstance()}
     */
    public <T extends IRegistryObjectHandler<?>> RegistryObjectHandlers registerHandler(
            Class<T> handlerInterface, T handler) {
        RegistryObjectHandlers.register(handlerInterface, handler);
        return getInstance();
    }

    /**
     * Register an {@link IRegistryObjectHandler} implementation for use with
     * the {@link RegistryObject} class. One, and only one,
     * {@link IRegistryObjectHandler} implementation can be registered for any
     * given {@link IRegistryObjectHandler} type.
     * 
     * @param <T>
     *            the registry object handler interface
     * @param handlerInterface
     *            the registry object handler interface
     * @param handler
     *            the handler
     * @throws IllegalStateException
     *             if a second handler is registered for any interface
     */
    public static <T extends IRegistryObjectHandler<?>> void register(
            Class<T> handlerInterface, T handler) throws IllegalStateException {

        if (!handlerInterface.isInterface()) {
            throw new IllegalArgumentException(
                    "Implementations must be registered under their interfaces!");
        }

        IRegistryObjectHandler<?> previous = handlers.putIfAbsent(
                handlerInterface.getName(), handler);
        if (previous != null) {
            throw new IllegalStateException(
                    String.format(
                            "Attempt to associate handler [%s] with handler interface [%s] fails, because [%s] is already associated with it!",
                            handler.getClass().getName(), handlerInterface
                                    .getName(), previous.getClass().getName()));
        }
    }

    /**
     * Clears all {@link IRegistryObjectHandler} registrations.
     */
    @VisibleForTesting
    static void clear() {
        handlers.clear();
    }

/**
     * Retrieve the registered {@link IRegistryObjectHandler implementation for the specified interface.
     * @param handlerInterface
     *  the handler interface
     * @return
     *  the implementation
     */
    public static <T> T get(Class<T> handlerInterface) {
        Object obj = handlers.get(handlerInterface.getName());

        if (obj == null) {
            throw new IllegalArgumentException(
                    "No handler registered for interface ["
                            + handlerInterface.getName() + "]!");
        }

        return handlerInterface.cast(obj);
    }

    /**
     * @return the instance
     */
    public static RegistryObjectHandlers getInstance() {
        return INSTANCE;
    }

}
