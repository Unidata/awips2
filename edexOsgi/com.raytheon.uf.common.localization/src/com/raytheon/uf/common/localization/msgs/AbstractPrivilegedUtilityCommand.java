package com.raytheon.uf.common.localization.msgs;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Abstract class for privileged utility commands
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2010             rgeorge     Initial creation
 * Oct 1, 2013  2361       njensen     Removed XML annotations
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */

@DynamicSerialize
public class AbstractPrivilegedUtilityCommand extends AbstractCommand {

    @DynamicSerializeElement
    protected String filename;

    @DynamicSerializeElement
    protected String myContextName;

    public AbstractPrivilegedUtilityCommand() {
        super();
    }

    public AbstractPrivilegedUtilityCommand(LocalizationContext context) {
        super(context);
    }

    public AbstractPrivilegedUtilityCommand(LocalizationContext context,
            String filename) {
        super(context);
        this.filename = filename;
    }

    public String getFilename() {
        return filename;
    }

    public void setFilename(String filename) {
        this.filename = filename;
    }

    public String getMyContextName() {
        return myContextName;
    }

    public void setMyContextName(String myContextName) {
        this.myContextName = myContextName;
    }

}
