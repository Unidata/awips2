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

package com.raytheon.edex.uengine.jep;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.pool.impl.GenericKeyedObjectPool;

/**
 * Pool for jep instances where the key is the thread that instantiated the jep
 * instance
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 15, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class JepPool extends GenericKeyedObjectPool {

    /** The logger */
    private transient Log logger = LogFactory.getLog(getClass());

    public JepPool(JepFactory jepFactory) {
        super(jepFactory);
    }

    @Override
    public Object borrowObject(Object key) {
        Object retVal = null;
        try {
            retVal = super.borrowObject(key);
        } catch (Exception e) {
            logger.error("Unable to borrow Jep instance from pool for key: "
                    + key, e);
        }
        return retVal;
    }

    @Override
    public void returnObject(Object key, Object borrowed) {
        try {
            if (borrowed != null && key != null) {
                super.returnObject(key, borrowed);
            }
        } catch (Exception e) {
            logger.error("Unable to return Jep instance to pool for key: "
                    + key, e);
        }
    }

}
