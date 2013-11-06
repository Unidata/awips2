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
package com.raytheon.uf.edex.wfs.feature;

import java.util.Collection;
import java.util.Map;

import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.opengis.feature.Feature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.feature.type.FeatureType;
import org.opengis.feature.type.PropertyDescriptor;

import com.raytheon.uf.common.json.JsonException;
import com.raytheon.uf.common.json.JsonService;

/**
 * Converts jaxb objects to geotools feature objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FeatureAdapter {

	protected JsonService jsonService;

	public FeatureAdapter(JsonService json) {
		this.jsonService = json;
	}

	public Feature convert(Object obj, FeatureType ft) throws JsonException {
		Collection<PropertyDescriptor> descriptors = ft.getDescriptors();
		Map<String, Object> map = jsonService.extract(obj);
		SimpleFeatureBuilder builder = new SimpleFeatureBuilder(
				(SimpleFeatureType) ft);
		for (PropertyDescriptor pd : descriptors) {
			String name = pd.getName().getLocalPart();
			builder.set(name, map.get(name));
		}
		return builder.buildFeature(null);
	}
}
