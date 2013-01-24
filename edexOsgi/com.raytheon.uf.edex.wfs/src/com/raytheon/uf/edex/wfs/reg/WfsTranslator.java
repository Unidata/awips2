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
 * Jul 26, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.reg;

import java.util.ArrayList;

import javax.xml.bind.JAXBElement;

import net.opengis.gml.v_3_1_1.AbstractFeatureType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public interface WfsTranslator {

	public ArrayList<JAXBElement<? extends AbstractFeatureType>> translate(
			PluginDataObject[] pdos) throws Exception;
}
