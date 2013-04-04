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
 * May 9, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.provider;

import java.util.ArrayList;
import java.util.List;

import net.opengis.ows.v_1_1_0.KeywordsType;
import net.opengis.ows.v_1_1_0.LanguageStringType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class WcsJaxbUtils {

	protected static final String DEFAULT_LANGUAGE = "english";

	public static List<LanguageStringType> getAsLangString(String... strs) {
		if (strs == null) {
			return null;
		}
		List<LanguageStringType> rval = new ArrayList<LanguageStringType>(
				strs.length);
		for (String str : strs) {
			LanguageStringType lst = new LanguageStringType();
			lst.setLang(DEFAULT_LANGUAGE);
			lst.setValue(str);
		}
		return rval;
	}

	public static List<KeywordsType> getKeywords(List<String> strs) {
		if (strs == null) {
			return null;
		}
		List<KeywordsType> rval = new ArrayList<KeywordsType>(strs.size());
		for (String str : strs) {
			KeywordsType kwt = new KeywordsType();
			kwt.setKeyword(getAsLangString(str));
		}
		return rval;
	}
}
