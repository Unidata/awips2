/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.feature;

import com.raytheon.uf.edex.ogc.common.OgcNamespace;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class FeatureTypeConfig {

	public enum METHOD {
		JAXB
	};

	protected String name;

	protected Class<?> binding;

	protected String crs;

	protected METHOD method = METHOD.JAXB;

	protected String namespace = OgcNamespace.EDEX;

	protected String geomName = "the_geom";

	public FeatureTypeConfig(String name, Class<?> binding, String crs) {
		super();
		this.name = name;
		this.binding = binding;
		this.crs = crs;
	}

	/**
	 * 
	 */
	public FeatureTypeConfig() {
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((binding == null) ? 0 : binding.hashCode());
		result = prime * result + ((crs == null) ? 0 : crs.hashCode());
		result = prime * result
				+ ((geomName == null) ? 0 : geomName.hashCode());
		result = prime * result + ((method == null) ? 0 : method.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result
				+ ((namespace == null) ? 0 : namespace.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		FeatureTypeConfig other = (FeatureTypeConfig) obj;
		if (binding == null) {
			if (other.binding != null)
				return false;
		} else if (!binding.equals(other.binding))
			return false;
		if (crs == null) {
			if (other.crs != null)
				return false;
		} else if (!crs.equals(other.crs))
			return false;
		if (geomName == null) {
			if (other.geomName != null)
				return false;
		} else if (!geomName.equals(other.geomName))
			return false;
		if (method != other.method)
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (namespace == null) {
			if (other.namespace != null)
				return false;
		} else if (!namespace.equals(other.namespace))
			return false;
		return true;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Class<?> getBinding() {
		return binding;
	}

	public void setBinding(Class<?> binding) {
		this.binding = binding;
	}

	public String getCrs() {
		return crs;
	}

	public void setCrs(String crs) {
		this.crs = crs;
	}

	public METHOD getMethod() {
		return method;
	}

	public void setMethod(METHOD method) {
		this.method = method;
	}

	public String getNamespace() {
		return namespace;
	}

	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	public String getGeomName() {
		return geomName;
	}

	public void setGeomName(String geomName) {
		this.geomName = geomName;
	}

}
