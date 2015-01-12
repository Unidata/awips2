/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.soap;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import org.apache.cxf.jaxb.JAXBDataBinding;
import org.apache.cxf.jaxws.JaxWsServerFactoryBean;

import com.sun.xml.bind.api.JAXBRIContext;
import com.sun.xml.bind.v2.model.annotation.RuntimeAnnotationReader;

/**
 * allows jaxb context of server to be configured
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2012            bclement     Initial creation
 * May 23, 2014 3199       bclement     moved to edex.soap from edex.ogc.common
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class JaxWsConfigServerFactoryBean extends JaxWsServerFactoryBean {

    /**
     * Add classes to the servers JAXBDataBinding
     * 
     * @param classes
     * @return a Boolean (true) so it can be used as a factory in spring, which
     *         is easier to read
     * @throws JAXBException
     */
    public Boolean addContextClasses(Class<?>... classes) throws JAXBException {
        JAXBDataBinding dataBinding = (JAXBDataBinding) getServiceFactory()
                .getDataBinding();
        Class<?>[] old = dataBinding.getExtraClass();
        if (old != null && old.length > 0) {
            Set<Class<?>> all = new HashSet<Class<?>>();
            all.addAll(Arrays.asList(old));
            all.addAll(Arrays.asList(classes));
            classes = all.toArray(new Class<?>[all.size()]);
        }
        dataBinding.setExtraClass(classes);
        if (dataBinding.getContext() != null) {
            // Context was already created, we need to forcibly re-create it
            Set<Class<?>> contextClasses = new LinkedHashSet<Class<?>>();
            contextClasses.addAll(dataBinding.getContextClasses());
            dataBinding.setContext(dataBinding
                    .createJAXBContext(contextClasses));
        }
        return true;
    }

    /**
     * Convenience method for adding a single class since spring has problems
     * with arrays of Class<?> objects
     * 
     * @param clazz
     * @throws JAXBException
     */
    public Boolean addContextClass(Class<?> clazz) throws JAXBException {
        return addContextClasses(clazz);
    }

    public void setAnnotationReader(RuntimeAnnotationReader reader) {
        setContextProperty(JAXBRIContext.ANNOTATION_READER, reader);
    }

    public void setMtomEnabled(boolean enabled) {
        JAXBDataBinding dataBinding = (JAXBDataBinding) getServiceFactory()
                .getDataBinding();
        dataBinding.setMtomEnabled(enabled);
        getServiceFactory().setDataBinding(dataBinding);
        Map<String, Object> old = getProperties();
        Map<String, Object> props = new HashMap<String, Object>();
        if (old != null) {
            props.putAll(old);
        }
        props.put("mtom-enabled", enabled);
        this.setProperties(props);
    }

    public void setContextProperty(String property, Object value) {
        JAXBDataBinding dataBinding = (JAXBDataBinding) getServiceFactory()
                .getDataBinding();
        Map<String, Object> old = dataBinding.getContextProperties();
        Map<String, Object> props = new HashMap<String, Object>();
        if (old != null) {
            props.putAll(old);
        }
        props.put(property, value);
        dataBinding.setContextProperties(props);
        getServiceFactory().setDataBinding(dataBinding);
    }
    
    public void setNamespaceMap(Map<String, String> map) {
        JAXBDataBinding dataBinding = (JAXBDataBinding) getServiceFactory()
                .getDataBinding();
        dataBinding.setNamespaceMap(map);
        getServiceFactory().setDataBinding(dataBinding);
    }

}
