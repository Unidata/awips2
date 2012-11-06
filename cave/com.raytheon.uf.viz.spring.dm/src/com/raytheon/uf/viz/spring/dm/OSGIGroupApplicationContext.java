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
package com.raytheon.uf.viz.spring.dm;

import java.util.List;

import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.support.GenericApplicationContext;

/**
 * OSGI Group application context, constructs an {@link OSGIGroupBeanFactory}
 * from the context group's factories
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 1, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class OSGIGroupApplicationContext extends GenericApplicationContext {

    public OSGIGroupApplicationContext(
            List<OSGIXmlApplicationContext> contextGroup) {
        refresh(); // refresh first to avoid recreating bean definitions
        DefaultListableBeanFactory factory = getDefaultListableBeanFactory();
        // Register all bean definitions from other contexts into our factory
        for (OSGIXmlApplicationContext ctx : contextGroup) {
            ConfigurableListableBeanFactory ctxFactory = ctx.getBeanFactory();
            for (String beanName : ctxFactory.getBeanDefinitionNames()) {
                factory.registerBeanDefinition(beanName,
                        ctxFactory.getBeanDefinition(beanName));
            }
        }
    }

}
