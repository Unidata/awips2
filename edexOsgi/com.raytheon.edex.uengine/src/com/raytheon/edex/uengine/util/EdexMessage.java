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

/**
 * 
 */
package com.raytheon.edex.uengine.util;

/**
 * Contains definitions of values defining the EDEX Cononical message. The basic
 * format of the cononical message is:
 * <pre>
 *    {@literal <message> }
 *       {@literal <header> }
 *          {@literal <property name="id" value="<<identifier>>" /> }
 *          {@literal <property name="time" value="<<date/time>>" /> }
 *          {@literal <property name="function" value="<<function>>" /> }
 *       {@literal </header> }
 *       {@literal <body> }
 *          {@literal <!-- contains the body of the message --> }
 *       {@literal </body> }
 *    {@literal </message> }
 * </PRE>
 * This class currently defines the valid values for the {@code function} property.
 * <pre>
 * 
 * SOFTWARE HISTORY
 *
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * 10Aug2006        Task 19         MW Fegan            Initial Creation
 * 
 * </PRE>
 *
 * @author mfegan
 *
 */
public final class EdexMessage {
    /**
     * Function property value for validating the message body.
     */
    public static final String VALIDATE = "validate";
    /**
     * Function property value for executing the message body.
     */
    public static final String EXECUTE = "execute";
    /**
     * Function property value for subscribing the message body.
     */
    public static final String SUBSCRIBE = "subscribe";
    /**
     * Function property value for unsubscribing the message body.
     */
    public static final String UNSUBSCRIBE = "unsubscribe";
    /**
     * Function property value indicating the message body is a response.
     */
    public static final String RESPONSE = "response";
}
