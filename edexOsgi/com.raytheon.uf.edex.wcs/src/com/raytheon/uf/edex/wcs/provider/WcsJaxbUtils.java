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
            rval.add(lst);
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
