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
package com.raytheon.uf.edex.registry.acp.xacml.engine.function;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.opensaml.xacml.ctx.RequestType;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLSyntaxException;

/**
 * 
 * Interface for XACML functions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public abstract class XACMLFunction {

    /** The method in this class that this function invokes */
    private Method method;

    /** The current request object */
    protected RequestType request;

    /** Gets the ID of the function */
    protected abstract String getFunctionId();

    /**
     * Gets whether this is a function that accepts varying amounts of arguments
     */
    protected boolean isVariableLengthArgs() {
        return false;
    }

    /**
     * Creates a new XACML function.
     */
    public XACMLFunction() {
        Method[] methods = this.getClass().getMethods();
        for (Method method : methods) {
            if (method.getName().equals("executeFunction")) {
                this.method = method;
                break;
            }
        }
    }

    /**
     * Evaluates the function with the provided arguments
     * 
     * @param <T>
     *            Object type
     * @param args
     *            The arguments to be passed to the function
     * @return The results of evaluating the function
     * @throws XACMLException
     *             If errors occur while evaluating the function
     */
    @SuppressWarnings("unchecked")
    public <T> T execute(Object[] args) throws XACMLException {
        if (this.method == null) {
            throw new XACMLProcessingException(
                    "Function implementation does not implement the 'executeFunction' method.  Unable to execute function.");
        }

        // Gets the parameter types from the method to be invoked
        Class<?>[] parameterTypes = method.getParameterTypes();

        /*
         * Checks to see if the method is expecting a list of items
         */
        for (int i = 0; i < parameterTypes.length; i++) {
            if (args[i] != null) {
                if (parameterTypes[i].equals(List.class)
                        && !args[i].getClass()
                                .isAssignableFrom(ArrayList.class)) {
                    List<Object> newArg = new ArrayList<Object>(1);
                    newArg.add(args[i]);
                    args[i] = newArg;
                }
            }
        }
        try {
            // If the function accepts a variable number of arguments, wrap the
            // arguments in an object array
            if (isVariableLengthArgs()) {
                return (T) this.method.invoke(this, new Object[] { args });

            }
            // Else execute the function with arguments provided
            else {
                return (T) this.method.invoke(this, args);
            }

        } catch (IllegalArgumentException e) {
            if (e.getMessage().equals("argument type mismatch")
                    || e.getMessage().contains("ClassCastException")) {
                throw new XACMLProcessingException("Error invoking function", e);
            } else {
                throw new XACMLSyntaxException("Error invoking function", e);
            }
        } catch (IllegalAccessException e) {
            throw new XACMLProcessingException("Error invoking function", e);
        } catch (InvocationTargetException e) {
            if (e.getTargetException() instanceof XACMLException) {
                throw (XACMLException) e.getTargetException();
            } else {
                throw new XACMLProcessingException("Error invoking function", e);
            }
        }
    }

    /**
     * Gets the request object
     * 
     * @return The request object
     */
    public RequestType getRequest() {
        return request;
    }

    /**
     * Sets the request object
     * 
     * @param request
     *            The request object
     */
    public void setRequest(RequestType request) {
        this.request = request;
    }

}
