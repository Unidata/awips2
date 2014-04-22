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
package com.raytheon.uf.edex.ogc.common.feature;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.geotools.data.memory.MemoryFeatureCollection;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.uf.common.http.MimeType;
import com.raytheon.uf.common.json.geo.GeoJsonUtil;
import com.raytheon.uf.common.json.geo.GeoJsonUtilSimpleImpl;
import com.raytheon.uf.common.json.geo.MixedFeatureCollection;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;

/**
 * Converts simple features to GeoJSON
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug  9, 2011            bclement    Initial creation
 * Mar 11, 2014      #2718 randerso    Changes for GeoTools 10.5
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class JsonFeatureFormatter implements SimpleFeatureFormatter {

    public static MimeType mimeType = new MimeType("application/json");

    protected GeoJsonUtil jsonUtil = new GeoJsonUtilSimpleImpl();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter#format
     * (java.util.List, java.io.OutputStream)
     */
    @Override
    public void format(List<List<SimpleFeature>> features, OutputStream out)
            throws Exception {
        MixedFeatureCollection mixed = convert(features);
        jsonUtil.serialize(mixed, out);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wms.format.SimpleFeatureFormatter#format(java.util
     * .List)
     */
    @Override
    public OgcResponse format(List<List<SimpleFeature>> features)
            throws Exception {
        MixedFeatureCollection mixed = convert(features);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        jsonUtil.serialize(mixed, baos);
        return new OgcResponse(baos.toString(), mimeType, TYPE.TEXT);
    }

    protected MixedFeatureCollection convert(List<List<SimpleFeature>> features) {
        List<MemoryFeatureCollection> colls = new ArrayList<MemoryFeatureCollection>(
                features.size());
        for (List<SimpleFeature> l : features) {
            if ((l != null) && !l.isEmpty()) {
                SimpleFeatureType t = l.get(0).getFeatureType();
                MemoryFeatureCollection c = new MemoryFeatureCollection(t);
                c.addAll(l);
                colls.add(c);
            }
        }
        return new MixedFeatureCollection(colls);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wms.format.SimpleFeatureFormatter#getMimeType()
     */
    @Override
    public MimeType getMimeType() {
        return mimeType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter#matchesFormat
     * (java.lang.String)
     */
    @Override
    public boolean matchesFormat(MimeType format) {
        return mimeType.equalsIgnoreParams(format);
    }

}
