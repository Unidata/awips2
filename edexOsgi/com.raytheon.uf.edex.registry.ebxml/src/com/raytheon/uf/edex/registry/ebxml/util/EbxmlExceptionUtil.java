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
package com.raytheon.uf.edex.registry.ebxml.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import org.apache.commons.beanutils.MethodUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * Utility class used to generate registry exceptions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2012 184        bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class EbxmlExceptionUtil {

    /**
     * Creates a new MsgRegistryException as well as populating it with the
     * underlying exception
     * 
     * @param msgRegistryExceptionMessage
     *            The message to attach to the exception
     * @param exceptionType
     *            The class type of the exception
     * @param code
     *            The error code, if any
     * @param message
     *            The message to attach to the underlying exception
     * @param detail
     *            More details about the exception
     * @param severity
     *            The severity of the exception
     * @return The new MsgRegistryException
     */
    public static MsgRegistryException createMsgRegistryException(
            String msgRegistryExceptionMessage, Class<?> exceptionType,
            String code, String message, String detail, String severity,
            IUFStatusHandler statusHandler) {
        return new MsgRegistryException(msgRegistryExceptionMessage,
                createRegistryException(exceptionType, code, message, detail,
                        severity, null, statusHandler));
    }

    /**
     * Creates a new MsgRegistryException as well as populating it with the
     * underlying exception
     * 
     * @param msgRegistryExceptionMessage
     *            The message to attach to the exception
     * @param exceptionType
     *            The class type of the exception
     * @param code
     *            The error code, if any
     * @param message
     *            The message to attach to the underlying exception
     * @param detail
     *            More details about the exception
     * @param severity
     *            The severity of the exception
     * @return The new MsgRegistryException
     */
    public static MsgRegistryException createMsgRegistryException(
            String msgRegistryExceptionMessage, Class<?> exceptionType,
            String code, String message, String detail, String severity,
            Exception rootException, IUFStatusHandler statusHandler) {
        return new MsgRegistryException(msgRegistryExceptionMessage,
                createRegistryException(exceptionType, code, message, detail,
                        severity, rootException, statusHandler));
    }

    /**
     * Creates a new MsgRegistryException given an existing underlying exception
     * 
     * @param message
     *            The message to attach to the exception
     * @param exception
     *            The underlying exception
     * @return The new MsgRegistryException
     */
    public static MsgRegistryException createMsgRegistryException(
            String message, RegistryExceptionType exception,
            IUFStatusHandler statusHandler) {
        return new MsgRegistryException(message, exception);
    }

    /**
     * Creates a new Exception instance
     * 
     * @param <T>
     *            A class type extending RegistryExceptionType
     * @param exceptionType
     *            The class type of the exception
     * @param code
     *            The error code, if any
     * @param message
     *            The message to attach to the underlying exception
     * @param detail
     *            More details about the exception
     * @param severity
     *            The severity of the exception
     * @return The new exception instance
     */
    public static <T extends RegistryExceptionType> T createRegistryException(
            Class<?> exceptionType, String code, String message, String detail,
            String severity, IUFStatusHandler statusHandler) {
        return createRegistryException(exceptionType, code, message, detail,
                severity, null, statusHandler);
    }

    /**
     * Creates a new Exception instance
     * 
     * @param <T>
     *            A class type extending RegistryExceptionType
     * @param exceptionType
     *            The class type of the exception
     * @param code
     *            The error code, if any
     * @param message
     *            The message to attach to the underlying exception
     * @param detail
     *            More details about the exception
     * @param severity
     *            The severity of the exception
     * @return The new exception instance
     */
    @SuppressWarnings("unchecked")
    public static <T extends RegistryExceptionType> T createRegistryException(
            Class<?> exceptionType, String code, String message, String detail,
            String severity, Exception rootException,
            IUFStatusHandler statusHandler) {
        T exception = null;
        String accessor = "create" + exceptionType.getSimpleName();

        try {
            try {
                return (T) createException(EbxmlObjectUtil.rsObjectFactory,
                        accessor, code, message, detail, severity,
                        rootException, statusHandler);
            } catch (NoSuchMethodException e) {
                // ignore
            }
            try {
                return (T) createException(EbxmlObjectUtil.spiObjectFactory,
                        accessor, code, message, detail, severity,
                        rootException, statusHandler);
            } catch (NoSuchMethodException e) {
                // ignore
            }
            try {
                return (T) createException(EbxmlObjectUtil.queryObjectFactory,
                        accessor, code, message, detail, severity,
                        rootException, statusHandler);
            } catch (NoSuchMethodException e) {
                // ignore
            }
        } catch (EbxmlRegistryException e) {
            statusHandler.error(
                    "Unexpected error encountered during error reporting", e);
        }
        return exception;
    }

    /**
     * Creates a new Exception instance
     * 
     * @param <T>
     *            A class type extending RegistryExceptionType
     * @param exceptionType
     *            The class type of the exception
     * @param code
     *            The error code, if any
     * @param message
     *            The message to attach to the underlying exception
     * @param detail
     *            More details about the exception
     * @param severity
     *            The severity of the exception
     * @return The new exception instance
     */
    private static RegistryExceptionType createException(Object factory,
            String accessor, String code, String message, String detail,
            String severity, Exception rootException,
            IUFStatusHandler statusHandler) throws NoSuchMethodException,
            EbxmlRegistryException {
        try {
            RegistryExceptionType exception = (RegistryExceptionType) MethodUtils
                    .invokeExactMethod(factory, accessor, null);
            exception.setCode(code);
            exception.setMessage(message);
            exception.setDetail(detail);
            exception.setSeverity(severity);
            exception.setDetail(detail);
            statusHandler.error("Exception Encountered: \n"
                    + exception.toString()
                    + getStackTrace(statusHandler, rootException));
            return exception;
        } catch (IllegalAccessException e) {
            throw new EbxmlRegistryException("Error creating exception", e);
        } catch (InvocationTargetException e) {
            throw new EbxmlRegistryException("Error creating exception", e);
        }
    }

    /**
     * Extracts the text of a stacktrace from an exception
     * 
     * @param handler
     *            The log handler
     * @param e
     *            The exception
     * @return The text version of the stack trace
     */
    private static String getStackTrace(IUFStatusHandler handler, Exception e) {
        if (e == null) {
            return "";
        }
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream printOut = new PrintStream(baos);
        try {
            e.printStackTrace(printOut);
            return new String(baos.toByteArray());
        } finally {
            if (baos != null) {
                try {
                    baos.close();
                } catch (IOException e1) {
                    handler.error(
                            "Error closing output stream when reporting error!",
                            e);
                }
            }
            if (printOut != null) {
                printOut.close();
            }
        }

    }
}
