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

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.AuthenticationExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.AuthorizationExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.InvalidRequestExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectExistsExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectNotFoundExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.QuotaExceededExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ReferencesExistExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.TimeoutExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnresolvedReferenceExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogingExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.FilteringExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidationExceptionType;

import org.apache.commons.lang.exception.ExceptionUtils;

import com.raytheon.uf.common.registry.constants.ErrorSeverity;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

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
 * Apr 23, 2013 1910       djohnson     Add createUnresolvedReferenceException().
 * 8/1/2013     1693       bphillip     Added methods to create exceptions
 * 10/20/2013   1682       bphillip     createMsgRegistryException changed to accept throwable
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class EbxmlExceptionUtil {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EbxmlExceptionUtil.class);

    /**
     * Creates a new MsgRegistryException object
     * 
     * @param message
     *            The message attached to the exception
     * @param detail
     *            Any additional details
     * @param exception
     *            The underlying exception that caused the MsgRegistryException
     * @return The MsgRegistryException
     */
    public static MsgRegistryException createMsgRegistryException(
            String message, String detail, RegistryExceptionType exception) {
        exception.setCode(exception.getClass().getSimpleName());
        exception.setDetail(detail);
        exception.setMessage(message);
        exception.setSeverity(ErrorSeverity.ERROR);
        MsgRegistryException msgException = new MsgRegistryException(message,
                exception);
        statusHandler.error(msgException.toString());
        return msgException;
    }

    /**
     * Creates a new MsgRegistryException object
     * 
     * @param message
     *            The message attached to the exception
     * @param detail
     *            Any additional details
     * @param exception
     *            The underlying exception that caused the MsgRegistryException
     * @return The MsgRegistryException
     */
    public static MsgRegistryException createMsgRegistryException(
            String message, String detail, Throwable exception) {
        StringBuilder builder = new StringBuilder();
        if (detail == null || detail.isEmpty()) {
            builder.append("Exception Encountered");
        } else {
            builder.append(detail);
        }

        builder.append("\n");
        Throwable[] throwables = ExceptionUtils.getThrowables(exception);
        for (int i = 0; i < throwables.length; i++) {
            if (throwables[i].getLocalizedMessage() != null) {
                if (i == 0) {
                    builder.append("\t\t  Exception: ");
                } else {
                    builder.append("\t\t  Caused By: ");
                }
                builder.append(throwables[i].getClass().getName()).append(": ")
                        .append(throwables[i].getLocalizedMessage())
                        .append("\n");
            }
        }
        MsgRegistryException retVal = createRegistryExceptionType(message,
                builder.toString());
        /*
         * Log the exception stack trace details in the server logs, but don't
         * return it to the client
         */
        statusHandler.error(message, exception);
        return retVal;
    }

    /**
     * Creates a new MsgRegistryException
     * 
     * @param message
     *            The message to attach to the exception
     * @param exception
     *            The underlying exception
     * @return The MsgRegistryException
     */
    public static MsgRegistryException createMsgRegistryException(
            String message, Throwable exception) {
        return createMsgRegistryException(message, null, exception);
    }

    /**
     * Creates a MsgRegistryException with RegistryExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with RegistryExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createRegistryExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new RegistryExceptionType());
    }

    /**
     * Creates a MsgRegistryException with AuthenticationExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with AuthenticationExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createAuthenticationExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new AuthenticationExceptionType());
    }

    /**
     * Creates a MsgRegistryException with AuthorizationExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with AuthorizationExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createAuthorizationExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new AuthorizationExceptionType());
    }

    /**
     * Creates a MsgRegistryException with CatalogingExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with CatalogingExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createCatalogingExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new CatalogingExceptionType());
    }

    /**
     * Creates a MsgRegistryException with FilteringExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with FilteringExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createFilteringExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new FilteringExceptionType());
    }

    /**
     * Creates a MsgRegistryException with InvalidRequestExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with InvalidRequestExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createInvalidRequestExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new InvalidRequestExceptionType());
    }

    /**
     * Creates a MsgRegistryException with ObjectExistsExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with ObjectExistsExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createObjectExistsExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new ObjectExistsExceptionType());
    }

    /**
     * Creates a MsgRegistryException with ObjectNotFoundExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with ObjectNotFoundExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createObjectNotFoundExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new ObjectNotFoundExceptionType());
    }

    /**
     * Creates a MsgRegistryException with QueryExceptionType as the underlying
     * exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with QueryExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createQueryExceptionType(String message,
            String detail) {
        return createMsgRegistryException(message, detail,
                new QueryExceptionType());
    }

    /**
     * Creates a MsgRegistryException with QuotaExceededExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with QuotaExceededExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createQuotaExceededExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new QuotaExceededExceptionType());
    }

    /**
     * Creates a MsgRegistryException with ReferencesExistExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with ReferencesExistExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createReferencesExistExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new ReferencesExistExceptionType());
    }

    /**
     * Creates a MsgRegistryException with TimeoutExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with TimeoutExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createTimeoutExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new TimeoutExceptionType());
    }

    /**
     * Creates a MsgRegistryException with UnresolvedReferenceExceptionType as
     * the underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with UnresolvedReferenceExceptionType as
     *         the underlying exception
     */
    public static MsgRegistryException createUnresolvedReferenceExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new UnresolvedReferenceExceptionType());
    }

    /**
     * Creates a MsgRegistryException with UnsupportedCapabilityExceptionType as
     * the underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with UnsupportedCapabilityExceptionType
     *         as the underlying exception
     */
    public static MsgRegistryException createUnsupportedCapabilityExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new UnsupportedCapabilityExceptionType());
    }

    /**
     * Creates a MsgRegistryException with ValidationExceptionType as the
     * underlying exception
     * 
     * @param message
     *            The message to attach to the MsgRegistryException
     * @param detail
     *            Additional details to add to the exception
     * @return The MsgRegistryException with ValidationExceptionType as the
     *         underlying exception
     */
    public static MsgRegistryException createValidationExceptionType(
            String message, String detail) {
        return createMsgRegistryException(message, detail,
                new ValidationExceptionType());
    }
}
