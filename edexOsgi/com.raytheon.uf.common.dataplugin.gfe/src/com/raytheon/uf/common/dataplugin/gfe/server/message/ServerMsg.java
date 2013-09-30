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
 * Encapsulates a messages used in ServerResponse
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/24/08     #875       bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

package com.raytheon.uf.common.dataplugin.gfe.server.message;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Encapsulates a message on the server.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/--                            Initial Creation
 * 09/30/13     #2361      njensen     Removed XML annotations
 * </pre>
 * 
 */
@DynamicSerialize
public class ServerMsg {

    /** The message */
    @DynamicSerializeElement
    private String message;

    /**
     * Creates an empty ServerMsg
     */
    public ServerMsg() {

    }

    /**
     * Creates a new ServerMsg with the given message
     * 
     * @param message
     *            The message
     */
    public ServerMsg(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String toString() {
        return this.message;
    }
}
