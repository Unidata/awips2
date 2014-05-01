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

import jep.Jep;
import jep.JepException;

import org.apache.commons.pool.KeyedPoolableObjectFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;

/**
 * Pooling factory for jep instances for python uEngine scripts
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

public class JepFactory implements KeyedPoolableObjectFactory {

    private static String includePath;

    static {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        String edexPython = pathMgr.getFile(edexStaticBase, "python").getPath();
        String commonPython = pathMgr.getFile(commonStaticBase, "python")
                .getPath();
        includePath = PyUtil.buildJepIncludePath(commonPython, edexPython);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.commons.pool.PoolableObjectFactory#activateObject(java.lang
     * .Object)
     */
    @Override
    public void activateObject(Object key, Object obj) throws Exception {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.commons.pool.PoolableObjectFactory#destroyObject(java.lang
     * .Object)
     */
    @Override
    public void destroyObject(Object key, Object obj) throws Exception {
        Jep jep = (Jep) obj;
        jep.close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.commons.pool.PoolableObjectFactory#makeObject()
     */
    @Override
    public Object makeObject(Object key) throws Exception {
        Jep jep = null;
        try {
            jep = new Jep(false, includePath, JepFactory.class.getClassLoader());
            // this enables easy import of java classes in the python script
            jep.eval("import JavaImporter");
            jep.eval("import CatalogQuery");
        } catch (JepException e) {
            throw new Exception("Error instantiating jep", e);
        }
        return jep;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.commons.pool.PoolableObjectFactory#passivateObject(java.lang
     * .Object)
     */
    @Override
    public void passivateObject(Object key, Object obj) throws Exception {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.commons.pool.PoolableObjectFactory#validateObject(java.lang
     * .Object)
     */
    @Override
    public boolean validateObject(Object key, Object obj) {
        return true;
    }

}
