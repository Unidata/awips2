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
import jep.JepConfig;
import jep.JepException;
import jep.NamingConventionClassEnquirer;

import org.apache.commons.pool2.BaseKeyedPooledObjectFactory;
import org.apache.commons.pool2.PooledObject;
import org.apache.commons.pool2.impl.DefaultPooledObject;

import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonIncludePathUtil;

/**
 * Pooling factory for jep instances for python uEngine scripts
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Apr 15, 2008             njensen     Initial creation
 * Apr 26, 2015  4259       njensen     Updated for new JEP API
 * Oct 22, 2015  5004       dgilling    Use new commons-pool2 API.
 * Apr 28, 2016  5236       njensen     Use Jep redirectOutput for python prints
 * 
 * </pre>
 * 
 * @author njensen
 */

public class JepFactory extends BaseKeyedPooledObjectFactory<Long, Jep> {

    private static final String includePath = PyUtil.buildJepIncludePath(
            PythonIncludePathUtil.getCommonPythonIncludePath(),
            PythonIncludePathUtil.getEdexPythonIncludePath());

    @Override
    public Jep create(Long arg0) throws Exception {
        try {
            JepConfig config = new JepConfig().setInteractive(false)
                    .setIncludePath(includePath)
                    .setClassLoader(JepFactory.class.getClassLoader())
                    .setClassEnquirer(new NamingConventionClassEnquirer())
                    .setRedirectOutputStreams(true);
            Jep jep = new Jep(config);
            // this enables easy import of java classes in the python script
            jep.eval("import CatalogQuery");
            return jep;
        } catch (JepException e) {
            throw new Exception("Error instantiating jep", e);
        }
    }

    @Override
    public PooledObject<Jep> wrap(Jep arg0) {
        return new DefaultPooledObject<>(arg0);
    }

    @Override
    public void destroyObject(Long key, PooledObject<Jep> p) throws Exception {
        Jep jep = p.getObject();
        jep.close();
    }
}
