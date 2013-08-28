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
 * Aug 8, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.format;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.List;
import java.util.Scanner;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;
import org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.TYPE;
import com.raytheon.uf.edex.ogc.common.feature.SimpleFeatureFormatter;
import com.raytheon.uf.edex.ogc.common.http.MimeType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class HtmlFeatureFormatter implements SimpleFeatureFormatter {

    public static final MimeType mimeType = new MimeType("text/html");

    private VelocityEngine _ve;

    private Template _bodyTemplate;

    private String _header;

    protected String bodyLocation = "META-INF/templates/gfi-html-body.vm";

    protected String headerLocation = "META-INF/templates/gfi-html-header.txt";

    protected Template getBodyTemplate() throws Exception {
        if (_bodyTemplate == null) {
            _ve = new VelocityEngine();
            _ve.setProperty(RuntimeConstants.RESOURCE_LOADER, "classpath");
            _ve.setProperty("classpath.resource.loader.class",
                    ClasspathResourceLoader.class.getName());
            _ve.init();
            _bodyTemplate = _ve.getTemplate(bodyLocation);
        }
        return _bodyTemplate;
    }

    protected String getHeader() throws IOException {
        if (_header == null) {
            ClassLoader loader = this.getClass().getClassLoader();
            InputStream in = loader.getResourceAsStream(headerLocation);
            if (in == null) {
                throw new IOException("Unable to find classpath resource: "
                        + headerLocation);
            }
            _header = new Scanner(in).useDelimiter("\\A").next();
        }
        return _header;
    }

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
        if (features == null || features.isEmpty()) {
            return;
        }
        Writer writer = new OutputStreamWriter(out);
        writer.write(getHeader());
        for (List<SimpleFeature> typeList : features) {
            addFeatures(typeList, writer);
        }
        writer.write("</body>\n</html>");
        writer.flush();
        writer.close();
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
        if (features == null || features.isEmpty()) {
            return new OgcResponse("", mimeType, TYPE.TEXT);
        }
        StringBuilder rval = new StringBuilder(getHeader());
        for (List<SimpleFeature> typeList : features) {
            addFeatures(typeList, rval);
        }
        rval.append("</body>\n</html>");
        return new OgcResponse(rval.toString(), mimeType, TYPE.TEXT);
    }

    protected void addFeatures(List<SimpleFeature> features,
            StringBuilder builder) throws Exception {
        StringWriter writer = new StringWriter();
        addFeatures(features, writer);
        builder.append(writer.getBuffer());
        writer.close();
    }

    protected void addFeatures(List<SimpleFeature> features, Writer writer)
            throws Exception {
        if (features == null || features.isEmpty()) {
            return;
        }
        SimpleFeature sample = features.get(0);
        SimpleFeatureType type = sample.getType();
        Template t = getBodyTemplate();
        VelocityContext vc = new VelocityContext();
        vc.put("type", type);
        vc.put("features", features);
        t.merge(vc, writer);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.wms.format.SimpleFeatureFormatter#getKey()
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
