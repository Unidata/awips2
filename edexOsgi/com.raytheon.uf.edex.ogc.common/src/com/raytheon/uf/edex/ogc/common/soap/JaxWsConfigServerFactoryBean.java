/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.soap;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

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
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class JaxWsConfigServerFactoryBean extends JaxWsServerFactoryBean {

    public void setClasses(Class<?>[] classes) {
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
        getServiceFactory().setDataBinding(dataBinding);
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
