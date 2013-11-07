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
package com.raytheon.uf.edex.wfs.v1_1_0;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import net.opengis.ows.v_1_0_0.KeywordsType;
import net.opengis.ows.v_1_0_0.WGS84BoundingBoxType;
import net.opengis.wfs.v_1_1_0.FeatureTypeType;

import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.wfs.WfsFeatureType;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * Converts feature type metadata objects to WFS 1.1.0 feature type jaxb objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2011            bclement     Initial creation
 * 
 * </pre>
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
		fType.setTitle(from.getTitle());
		fType.setAbstract(from.getAbs());
		fType.setKeywords(getAsKeywordList(from.getKeywords()));
		fType.setDefaultSRS(from.getDefaultSRS());
		fType.setOtherSRS(from.getOtherSRS());
		fType.setWGS84BoundingBox(getBBox(from.getBbox()));
		return fType;
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
			kwType.setKeyword(keywords);
			rval.add(kwType);
		}
		return rval;
	}
}
