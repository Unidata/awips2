package com.raytheon.uf.common.localization.msgs;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Abstract command for all localization commands
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2010             rgeorge     Initial creation
 * Oct 1, 2013  2361       njensen     Removed XML annotations
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */

@DynamicSerialize
public abstract class AbstractCommand {

    @DynamicSerializeElement
    protected LocalizationContext context;

    public AbstractCommand() {
    }

    public AbstractCommand(LocalizationContext context) {
        this.context = context;
    }

    /**
     * @return the context
     */
    public LocalizationContext getContext() {
        return context;
    }

    /**
     * @param context
     *            the context to set
     */
    public void setContext(LocalizationContext context) {
        this.context = context;
    }
}
