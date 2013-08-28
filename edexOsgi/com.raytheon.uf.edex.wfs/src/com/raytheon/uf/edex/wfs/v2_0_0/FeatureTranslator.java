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
 * Apr 27, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.v2_0_0;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import net.opengis.ows.v_1_1_0.KeywordsType;
import net.opengis.ows.v_1_1_0.LanguageStringType;
import net.opengis.ows.v_1_1_0.WGS84BoundingBoxType;
import net.opengis.wfs.v_2_0_0.Abstract;
import net.opengis.wfs.v_2_0_0.FeatureTypeType;
import net.opengis.wfs.v_2_0_0.Title;

import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class FeatureTranslator {

	public List<FeatureTypeType> transform(List<WfsFeatureType> features) {
		List<FeatureTypeType> rval = new ArrayList<FeatureTypeType>(
				features.size());
		for (WfsFeatureType f : features) {
			rval.add(transform(f));
		}
		return rval;
	}

	/**
	 * @param source
	 * @return
	 */
	public FeatureTypeType transform(WfsFeatureType from) {
		FeatureTypeType fType = new FeatureTypeType();
		QName name = transform(from.getName());
		fType.setName(name);
        setTitle(fType, from);
        setAbstract(fType, from);
		fType.setKeywords(getAsKeywordList(from.getKeywords()));
        fType.setDefaultCRS(from.getDefaultSRS());
        fType.setOtherCRS(from.getOtherSRS());
		fType.setWGS84BoundingBox(getBBox(from.getBbox()));
		return fType;
	}

    protected void setTitle(FeatureTypeType fType, WfsFeatureType from) {
        if (from.getTitle() == null || from.getTitle().isEmpty()) {
            return;
        }
        Title title = new Title();
        title.setValue(from.getTitle());
        fType.setTitle(Arrays.asList(title));
    }

    protected void setAbstract(FeatureTypeType fType, WfsFeatureType from) {
        if (from.getAbs() == null || from.getAbs().isEmpty()) {
            return;
        }
        Abstract abs = new Abstract();
        abs.setValue(from.getAbs());
        fType.setAbstract(Arrays.asList(abs));
    }

	protected QName transform(QualifiedName from) {
		return new QName(from.getNamespace(), from.getName());
	}

	/**
	 * @param bbox
	 * @return
	 */
	private List<WGS84BoundingBoxType> getBBox(OgcGeoBoundingBox bbox) {
		List<WGS84BoundingBoxType> rval = new LinkedList<WGS84BoundingBoxType>();
		WGS84BoundingBoxType to = new WGS84BoundingBoxType();
		List<Double> ur = new LinkedList<Double>();
		List<Double> ll = new LinkedList<Double>();
		ur.add(bbox.getMaxx());
		ur.add(bbox.getMaxy());
		ll.add(bbox.getMinx());
		ll.add(bbox.getMiny());
		to.setUpperCorner(ur);
		to.setLowerCorner(ll);
		rval.add(to);
		return rval;
	}

	protected List<KeywordsType> getAsKeywordList(List<String> keywords) {
		List<KeywordsType> rval = new LinkedList<KeywordsType>();
		if (keywords != null && !keywords.isEmpty()) {
			KeywordsType kwType = new KeywordsType();
            kwType.setKeyword(asLangStr(keywords));
			rval.add(kwType);
		}
		return rval;
	}

    protected List<LanguageStringType> asLangStr(List<String> strs) {
        List<LanguageStringType> rval = new ArrayList<LanguageStringType>(
                strs.size());
        for (String s : strs) {
            LanguageStringType lst = new LanguageStringType();
            lst.setValue(s);
            rval.add(lst);
        }
        return rval;
    }
}
