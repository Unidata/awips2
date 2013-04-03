/**********************************************************************
 *
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
 **********************************************************************/
/**
 * 
 */
package com.raytheon.uf.common.json;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

/**
 * @author bclement
 * 
 */
public interface JsonService {

	public String serialize(Object obj, boolean pretty) throws JsonException;

	public Map<String, Object> extract(Object obj) throws JsonException;

	public Object populate(Map<String, Object> map, Class<?> c)
			throws JsonException;

	public void serialize(Object obj, OutputStream out, boolean pretty)
			throws JsonException;

	public Object deserialize(String json, Class<?> c) throws JsonException;

	public Object deserialize(InputStream in, Class<?> c) throws JsonException;

}
