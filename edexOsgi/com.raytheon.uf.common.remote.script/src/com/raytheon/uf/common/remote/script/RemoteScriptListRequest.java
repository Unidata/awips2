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
package com.raytheon.uf.common.remote.script;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * This class is a request to obtain listing of remote scripts from desired
 * localization directories.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 13, 2014 2742       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

@DynamicSerialize
public class RemoteScriptListRequest extends RemoteScriptRequest {

    /**
     * The contexts to search for scripts.
     */
    @DynamicSerializeElement
    private LocalizationContext[] contexts;

    /**
     * Default constructor.
     */
    public RemoteScriptListRequest() {
    }

    /**
     * Constructor.
     * 
     * @param userId
     */
    public RemoteScriptListRequest(String userId, LocalizationContext[] contexts) {
        this();
        setUserId(userId);
        setContexts(contexts);
    }

    /**
     * Getter.
     * 
     * @return contexts
     */
    public LocalizationContext[] getContexts() {
        return contexts;
    }

    /**
     * Setter.
     * 
     * @param contexts
     *            - when null contexts is cleared.
     */
    public void setContexts(LocalizationContext[] contexts) {
        if (contexts != null) {
            this.contexts = contexts;
        } else {
            contexts = new LocalizationContext[0];
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("RemoteScriptListRequest {");
        sb.append("userId: ").append(getUserId());
        sb.append(", contexts: ").append(getContexts());
        sb.append("}");
        return sb.toString();
    }
}
