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
 * Mar 29, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.ogc.common.db;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * TODO Add Description
 * 
 * @author bclement
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class LayerHolder<L extends SimpleLayer> {
	@XmlElement
	protected L value;

	@SuppressWarnings("unchecked")
	public static Class<LayerHolder<SimpleLayer>> getDefaultClass() {
		return (Class<LayerHolder<SimpleLayer>>) new LayerHolder<SimpleLayer>()
				.getClass();
	}

	public L getValue() {
		return value;
	}

	public void setValue(L value) {
		this.value = value;
	}

}
