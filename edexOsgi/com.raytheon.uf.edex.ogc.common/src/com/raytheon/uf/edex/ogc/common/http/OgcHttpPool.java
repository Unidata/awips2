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
package com.raytheon.uf.edex.ogc.common.http;

import org.apache.commons.pool.KeyedPoolableObjectFactory;
import org.apache.commons.pool.impl.GenericKeyedObjectPool;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;


/**
 * Pooling for OGC http handlers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcHttpPool extends GenericKeyedObjectPool implements
        IOgcHttpPooler {

	/** The logger */
	private transient IUFStatusHandler log = UFStatus.getHandler(getClass());

	public OgcHttpPool(KeyedPoolableObjectFactory ogcFactory) {
		super(ogcFactory);
	}

	@Override
	public Object borrowObject(Object key) {
		Object retVal = null;
		try {
			retVal = super.borrowObject(key);
		} catch (IllegalStateException e) {
			log.error(
					"Unable to borrow Ogc HTTP instance from pool for key: "
							+ key, e);
			throw new RuntimeException(e);
		} catch (Throwable e) {
			// handle when OSGi removes object but pool still has key
			returnObject(key, retVal);
			clear(key);
			retVal = borrowObject(key);

			if (retVal == null) {
				// it still didn't work, blow up
				log.error(
						"Unable to borrow Ogc HTTP instance from pool for key: "
								+ key, e);
				throw new RuntimeException(e);
			}
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
			log.error("Unable to return Ogc HTTP instance to pool for key: "
					+ key, e);
		}
	}

	public void drain() {
		clear();
	}
}
