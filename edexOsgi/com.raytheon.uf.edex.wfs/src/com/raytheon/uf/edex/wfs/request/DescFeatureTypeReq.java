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
 *
 */
package com.raytheon.uf.edex.wfs.request;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import net.opengis.wfs.v_1_1_0.DescribeFeatureTypeType;

/**
 * @author bclement
 * 
 */
public class DescFeatureTypeReq extends WfsRequest {

	protected List<QualifiedName> typenames;

	protected String outputformat = "text/xml; subtype=gml/3.1.1";

	/**
	 * @param type
	 */
	public DescFeatureTypeReq() {
		super(Type.DescribeFeature);
		this.typenames = new LinkedList<QualifiedName>();
	}

	public DescFeatureTypeReq(List<QualifiedName> typenames) {
		super(Type.DescribeFeature);
		this.typenames = typenames;
	}

	/**
	 * @param obj
	 * @return
	 */
	public DescFeatureTypeReq(DescribeFeatureTypeType req) {
		super(Type.DescribeFeature);
		setRawrequest(req);
		String outputFormat = req.getOutputFormat();
		if (outputFormat != null && !outputFormat.isEmpty()) {
			setOutputformat(outputFormat);
		}
		List<QName> typeName = req.getTypeName();
		if (typeName != null) {
			for (QName n : typeName) {
				QualifiedName qname = new QualifiedName(n.getNamespaceURI(),
						n.getLocalPart(), n.getPrefix());
				addTypename(qname);
			}
		}
	}

	public void addTypename(QualifiedName typename) {
		if (this.typenames == null) {
			this.typenames = new ArrayList<QualifiedName>();
		}
		this.typenames.add(typename);
	}

	/**
	 * @return the typenames
	 */
	public List<QualifiedName> getTypenames() {
		return typenames;
	}

	/**
	 * @param typenames
	 *            the typenames to set
	 */
	public void setTypenames(List<QualifiedName> typenames) {
		this.typenames = typenames;
	}

	/**
	 * @return the outputformat
	 */
	public String getOutputformat() {
		return outputformat;
	}

	/**
	 * @param outputformat
	 *            the outputformat to set
	 */
	public void setOutputformat(String outputformat) {
		this.outputformat = outputformat;
	}

}
