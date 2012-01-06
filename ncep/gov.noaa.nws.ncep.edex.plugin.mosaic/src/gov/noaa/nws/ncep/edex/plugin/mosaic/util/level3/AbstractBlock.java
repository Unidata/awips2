
package gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3;

import java.io.DataInputStream;
import java.io.IOException;

/**
 * AbstractBlock is a class that will allow different blocks to build upon.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class AbstractBlock {
    public AbstractBlock(DataInputStream in) throws IOException {
        init(in);
    }

    protected AbstractBlock() {

    }

    protected abstract void init(DataInputStream in) throws IOException;

}
