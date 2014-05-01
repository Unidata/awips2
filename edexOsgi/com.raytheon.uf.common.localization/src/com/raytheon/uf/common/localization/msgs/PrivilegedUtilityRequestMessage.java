package com.raytheon.uf.common.localization.msgs;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Message for privileged requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2010            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
@DynamicSerialize
public class PrivilegedUtilityRequestMessage extends AbstractPrivilegedRequest {

    @DynamicSerializeElement
    protected AbstractPrivilegedUtilityCommand[] commands;

    public PrivilegedUtilityRequestMessage() {

    }

    /**
     * @return the commands
     */
    public AbstractPrivilegedUtilityCommand[] getCommands() {
        return commands;
    }

    /**
     * @param commands
     *            the commands to set
     */
    public void setCommands(AbstractPrivilegedUtilityCommand[] commands) {
        this.commands = commands;
    }

}
