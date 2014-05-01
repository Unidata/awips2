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
package com.raytheon.uf.edex.auth;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.RemoteServiceRequest;

/**
 * Handler of web service based requests, uses reflection to execute method on
 * handler object. This handler assumes the type registered in the
 * HandlerFactory is in fact an instance of the interface passed in. Exceptions
 * will be thrown otherwise
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RemoteServiceRequestHandler implements
        IRequestHandler<RemoteServiceRequest> {

    private static final Map<Class<?>, Class<?>> PRIMITIVES = new HashMap<Class<?>, Class<?>>();
    static {
        PRIMITIVES.put(Boolean.class, Boolean.TYPE);
        PRIMITIVES.put(Byte.class, Byte.TYPE);
        PRIMITIVES.put(Character.class, Character.TYPE);
        PRIMITIVES.put(Short.class, Short.TYPE);
        PRIMITIVES.put(Integer.class, Integer.TYPE);
        PRIMITIVES.put(Float.class, Float.TYPE);
        PRIMITIVES.put(Long.class, Long.TYPE);
        PRIMITIVES.put(Double.class, Double.TYPE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(RemoteServiceRequest request) throws Exception {
        long t0 = System.currentTimeMillis();
        try {
            String interfaze = request.getInterfaceToUse();
            Object impl = HandlerRegistry.getInstance().getRegisteredObject(
                    interfaze);
            if (impl != null) {
                // Find the interface impl claims to implement
                Class<?> clazz = null;
                for (Class<?> inter : impl.getClass().getInterfaces()) {
                    if (inter.getCanonicalName().equals(interfaze)) {
                        clazz = inter;
                        break;
                    }
                }

                // If null, does not implement interface
                if (clazz == null) {
                    throw new IllegalArgumentException("Registered object for "
                            + interfaze + " does not implement the interface");
                }

                String method = request.getMethod();
                Object[] args = request.getArguments();
                if (args == null) {
                    args = new Object[0];
                }

                // Find the method we want to implement
                Method m = null;
                for (Method meth : clazz.getMethods()) {
                    if (meth.getName().equals(method)) {
                        Class<?>[] argTypes = meth.getParameterTypes();
                        if (argTypes.length == args.length) {
                            boolean good = true;
                            for (int i = 0; i < argTypes.length; ++i) {
                                Object arg = args[i];
                                Class<?> type = argTypes[i];
                                if (arg != null
                                        && type.isInstance(arg) == false) {
                                    // Argument is not instance of type, check
                                    // primitive argument
                                    Class<?> primitiveClass = PRIMITIVES
                                            .get(arg.getClass());
                                    if (primitiveClass != type) {
                                        good = false;
                                        break;
                                    }
                                }
                            }
                            if (good) {
                                m = meth;
                                break;
                            }
                        }
                    }
                }

                // Method not found
                if (m == null) {
                    throw new NoSuchMethodException(
                            "Could not find method named '" + method
                                    + "' that supported arguments passed in");
                }

                // Execute
                return m.invoke(impl, args);
            }
            throw new ClassNotFoundException(
                    "Could not find implementation class for "
                            + request.getInterfaceToUse());
        } finally {
            System.out.println("Time to execute = "
                    + (System.currentTimeMillis() - t0) + "ms");
        }
    }
}
