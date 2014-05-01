/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.util;

import java.util.Map;
import java.util.Map.Entry;

import org.springframework.beans.BeanWrapperImpl;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;

/**
 * Allows setting of additional properties on beans defined in other spring
 * files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AddonPropsPostProcessor implements BeanPostProcessor {

	private final Map<String, Map<String, Object>> propsMap;

	/**
	 * @param propsMap
	 *            a mapping of bean names to properties maps. For example:
	 * 
	 *            <pre>
	 *            { "myBean" :
	 *            	{ "myProp" : "value" }
	 *            }
	 * </pre>
	 */
	public AddonPropsPostProcessor(Map<String, Map<String, Object>> propsMap) {
		this.propsMap = propsMap;
	}

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanPostProcessor#postProcessBeforeInitialization(java.lang.Object, java.lang.String)
	 */
	@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName)
			throws BeansException {
		Map<String, Object> props = propsMap.get(beanName);
		if (props != null) {
			BeanWrapperImpl wrapper = new BeanWrapperImpl(bean);
			for (Entry<String, Object> e : props.entrySet()) {
				wrapper.setPropertyValue(e.getKey(), e.getValue());
			}
		}
		return bean;
	}

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanPostProcessor#postProcessAfterInitialization(java.lang.Object, java.lang.String)
	 */
	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName)
			throws BeansException {
		return bean;
	}

}
