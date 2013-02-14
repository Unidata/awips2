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

import org.osgi.framework.Bundle;
import org.springframework.beans.factory.xml.XmlBeanDefinitionReader;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.AbstractXmlApplicationContext;

/**
 * XML Application context for OSGI Bundle. Creates a class loader which uses
 * the bundle to resolve classes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class OSGIXmlApplicationContext extends AbstractXmlApplicationContext {

    public OSGIXmlApplicationContext(String[] configLocations, Bundle bundle) {
        this(null, configLocations, bundle);
    }

    public OSGIXmlApplicationContext(ApplicationContext parent,
            String[] configLocations, Bundle bundle) {
        super(parent);
        setClassLoader(new OSGIXmlClassLoader(bundle, getClassLoader()));
        setConfigLocations(configLocations);
        refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.springframework.context.support.AbstractXmlApplicationContext#
     * initBeanDefinitionReader
     * (org.springframework.beans.factory.xml.XmlBeanDefinitionReader)
     */
    @Override
    protected void initBeanDefinitionReader(
            XmlBeanDefinitionReader beanDefinitionReader) {
        super.initBeanDefinitionReader(beanDefinitionReader);
        beanDefinitionReader.setBeanClassLoader(getClassLoader());
    }

}
