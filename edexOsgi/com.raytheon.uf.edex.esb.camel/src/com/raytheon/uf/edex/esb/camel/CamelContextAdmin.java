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
package com.raytheon.uf.edex.esb.camel;

import java.util.ArrayList;
import java.util.List;

import org.apache.camel.CamelContext;
import org.springframework.context.ApplicationContext;

import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.IContextAdmin;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CamelContextAdmin implements IContextAdmin {

    private static ApplicationContext springContext;

    private static List<CamelContext> camelContexts;

    private static synchronized ApplicationContext getSpringContext() {
        if (springContext == null) {
            springContext = EDEXUtil.getSpringContext();
        }
        return springContext;
    }

    private static synchronized List<CamelContext> getCamelContexts() {
        if (camelContexts == null) {
            camelContexts = new ArrayList<CamelContext>();
            String[] contextNames = getSpringContext().getBeanNamesForType(
                    CamelContext.class);
            for (String name : contextNames) {
                CamelContext cc = (CamelContext) springContext.getBean(name);
                camelContexts.add(cc);
            }
        }
        return camelContexts;
    }

    public List<String> getAllContexts() {
        List<String> result = new ArrayList<String>();
        List<CamelContext> ccs = getCamelContexts();
        for (CamelContext cc : ccs) {
            result.add(cc.getName());
        }
        return result;
    }

    public List<String> getActiveContexts() {
        List<String> result = new ArrayList<String>();
        for (CamelContext cc : getCamelContexts()) {
            if (cc.getStatus().isStarted()) {
                result.add(cc.getName());
            }
        }
        return result;
    }

    public List<String> getInactiveContexts() {
        List<String> result = new ArrayList<String>();
        for (CamelContext cc : getCamelContexts()) {
            if (cc.getStatus().isStopped()) {
                result.add(cc.getName());
            }
        }
        return result;
    }

    public void startContext(String name) throws Exception {
        CamelContext cc = (CamelContext) getSpringContext().getBean(name);
        cc.start();
    }

    public void stopContext(String name) throws Exception {
        CamelContext cc = (CamelContext) getSpringContext().getBean(name);
        cc.stop();
    }

}
