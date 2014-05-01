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

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.registry.GenericRegistry;
import com.raytheon.uf.common.util.registry.RegistryException;

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
 * Jun 03, 2013 2038       djohnson     Change to use a {@link GenericRegistry}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class RegistryObjectHandlers {

    private static IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryObjectHandlers.class);

    private static final GenericRegistry<String, IRegistryObjectHandler<?>> handlers = new GenericRegistry<String, IRegistryObjectHandler<?>>() {
    };

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
     * @throws IllegalArgumentException
     *             if the handlerInterface parameter is not an interface
     * @throws IllegalStateException
     *             on an error registering the handler
     */
    public static <T extends IRegistryObjectHandler<?>> void register(
            Class<T> handlerInterface, T handler) throws IllegalStateException {

        if (!handlerInterface.isInterface()) {
            throw new IllegalArgumentException(
                    "Implementations must be registered under their interfaces!");
        }

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.handle(Priority.DEBUG, String.format(
                    "Associating handler [%s] with handler interface [%s]",
                    handler.getClass().getName(), handlerInterface.getName()));
        }

        try {
            handlers.register(handlerInterface.getName(), handler);
        } catch (RegistryException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * Clears all {@link IRegistryObjectHandler} registrations.
     */
    @VisibleForTesting
    static void clear() {
        handlers.getRegisteredObjects().clear();
    }

/**
     * Retrieve the registered {@link IRegistryObjectHandler implementation for the specified interface.
     * @param handlerInterface
     *  the handler interface
     * @return
     *  the implementation
     * @throws IllegalArgumentException if no handler is registered for the specific interface
     */
    public static <T> T get(Class<T> handlerInterface) {
        Object obj = handlers.getRegisteredObject(handlerInterface.getName());

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
