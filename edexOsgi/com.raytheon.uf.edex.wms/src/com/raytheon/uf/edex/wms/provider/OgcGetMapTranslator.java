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
 *
 */
package com.raytheon.uf.edex.wms.provider;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;

import net.opengis.ows.v_1_0_0.BoundingBoxType;
import net.opengis.sld.v_1_1_0.ElevationType;
import net.opengis.sld.v_1_1_0.GetMapType;
import net.opengis.sld.v_1_1_0.OutputType;
import net.opengis.sld.v_1_1_0.OutputType.Size;
import net.opengis.sld.v_1_1_0.StyledLayerDescriptorElement;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.Node;
import org.dom4j.QName;
import org.dom4j.io.DocumentSource;
import org.geotools.styling.StyledLayerDescriptor;
import org.w3c.dom.Document;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcPrefix;
import com.raytheon.uf.edex.wms.GetMapRequest;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.sld.SldParser;
import com.raytheon.uf.edex.wms.sld.SldParserRegistry;

public class OgcGetMapTranslator {

	protected Log log = LogFactory.getLog(this.getClass());

	protected SldParserRegistry registry;

	protected Namespace sldNS = new Namespace(OgcPrefix.SLD, OgcNamespace.SLD);

	protected Namespace owsNS = new Namespace(OgcPrefix.OWS, OgcNamespace.OWS);

	protected Namespace wmsNS = new Namespace(OgcPrefix.WMS, OgcNamespace.WMS);

	protected QName sldName = new QName("StyledLayerDescriptor", sldNS);

	protected QName crsName = new QName("CRS", sldNS);

	protected QName bboxName = new QName("BoundingBox", sldNS);

	protected QName lcName = new QName("LowerCorner", owsNS);

	protected QName ucName = new QName("UpperCorner", owsNS);

	protected QName outputName = new QName("Output", sldNS);

	protected QName sizeName = new QName("Size", sldNS);

	protected QName widthName = new QName("Width", sldNS);

	protected QName heightName = new QName("Height", sldNS);

	protected QName formatName = new QName("Format", wmsNS);

	protected QName transparentName = new QName("Transparent", sldNS);

	protected QName execptionsName = new QName("Exceptions", sldNS);

	protected QName elevationName = new QName("Elevation", sldNS);

	protected QName timeName = new QName("Time", sldNS);

	protected QName valueName = new QName("Value", sldNS);

	public OgcGetMapTranslator(SldParserRegistry registry) {
		this.registry = registry;
	}

	public GetMapRequest translate(Element root) throws WmsException {
		GetMapRequest rval = new GetMapRequest();
		rval.setSld(parseSld(root.element(sldName)));
		rval.setBbox(parseBbox(root.element(bboxName)));
		rval.setCrs(getTextElement(root.element(crsName)));
		rval.setElevation(parseElevation(root.element(elevationName)));
		parseOutput(root.element(outputName), rval);
		rval.setTime(getTextElement(root.element(timeName)));
		rval.setExceptionFormat(root.elementTextTrim(execptionsName));
		return rval;
	}

	/**
	 * @param element
	 * @param rval
	 */
	protected void parseOutput(Element output, GetMapRequest rval) {
		if (output == null) {
			return;
		}
		Element size = output.element(sizeName);
		if (size != null) {
			try {
				String width = size.elementTextTrim(widthName);
				rval.setWidth(Integer.valueOf(width));
				String height = size.elementTextTrim(heightName);
				rval.setHeight(Integer.valueOf(height));
			} catch (NumberFormatException e) {
				// leave sizes null
			}
		}
		rval.setFormat(output.elementTextTrim(formatName));
		rval.setTransparent(getBoolElement(output.element(transparentName)));
	}

	protected Boolean getBoolElement(Element elem) {
		if (elem == null) {
			return null;
		}
		String txt = elem.getTextTrim();
		return txt != null && txt.equalsIgnoreCase("true");
	}

	protected String parseElevation(Element elev) {
		if (elev == null) {
			return null;
		}
		String rval = null;
		// TODO handle intervals
		Element val = elev.element(valueName);
		if (val != null) {
			rval = val.getTextTrim();
		}
		return rval;
	}

	/**
	 * @param element
	 * @return
	 */
	protected String getTextElement(Element elem) {
		if (elem == null) {
			return null;
		}
		return elem.getTextTrim();
	}

	/**
	 * @param element
	 * @return
	 */
	protected String parseBbox(Element bboxElem) {
		if (bboxElem == null) {
			return null;
		}
		String lcText = bboxElem.elementTextTrim(lcName);
		String ucText = bboxElem.elementTextTrim(ucName);
		String[] lc = StringUtils.split(lcText);
		String[] uc = StringUtils.split(ucText);
		List<String> bounds = Arrays.asList(lc[0], lc[1], uc[0], uc[1]);
		return StringUtils.join(bounds, ',');
	}

	protected StyledLayerDescriptor parseSld(Element e) throws WmsException {
		if (e == null) {
			return null;
		}
		String version = e.attributeValue("version");
		SldParser parser = registry.getParser(version);
		if (parser == null) {
			throw new WmsException(Code.MissingParameterValue,
					"SLD version must be specified");
		}
		Document doc;
		try {
			doc = convert(e);
			StyledLayerDescriptor rval = parser.parse(doc);
			return rval;
		} catch (Exception e1) {
			log.error("Unable to parse SLD docuement", e1);
			throw new WmsException(Code.InternalServerError);
		}
	}

	protected Document convert(Node n) throws TransformerException {
		TransformerFactory factory = TransformerFactory.newInstance();
		Transformer transformer = factory.newTransformer();
		DocumentSource source = new DocumentSource(n);
		DOMResult result = new DOMResult();
		transformer.transform(source, result);
		return (Document) result.getNode();
	}

	// below are JAXB utility methods (would need jaxb context)

	/**
	 * @param req
	 * @return
	 * @throws Throwable
	 */
	@Deprecated
	protected GetMapRequest translate(GetMapType req) throws Throwable {
		GetMapRequest rval = new GetMapRequest();
		rval.setBbox(formatBbox(req.getBoundingBox()));
		rval.setCrs(req.getCRS());
		rval.setElevation(formatElevation(req.getElevation()));
		parseOutput(req.getOutput(), rval);
		rval.setTime(formatTime(req.getTime()));
		rval.setSld(translate(req.getStyledLayerDescriptor()));
		return rval;
	}

	protected String formatBbox(BoundingBoxType bbox) {
		if (bbox == null) {
			return null;
		}
		List<Double> lc = bbox.getLowerCorner();
		List<Double> uc = bbox.getUpperCorner();

		return String.format("%f,%f,%f,%f", lc.get(0), lc.get(1), uc.get(0),
				uc.get(1));
	}

	protected String formatElevation(ElevationType elev) {
		if (elev == null) {
			return null;
		}
		List<Double> value = elev.getValue();
		if (value != null && !value.isEmpty()) {
			return StringUtils.join(value, ",");
		} else {
			return null;
		}
	}

	protected void parseOutput(OutputType out, GetMapRequest req) {
		if (out == null) {
			return;
		}
		req.setBgcolor(out.getBGcolor());
		req.setFormat(out.getFormat());
		Size size = out.getSize();
		if (size != null) {
			BigInteger width = size.getWidth();
			BigInteger height = size.getHeight();
			if (width != null) {
				req.setWidth(width.intValue());
			}
			if (height != null) {
				req.setHeight(height.intValue());
			}
		}
	}

	protected String formatTime(XMLGregorianCalendar cal) {
		if (cal == null) {
			return null;
		}
		return TimeUtil.formatCalendar(cal.toGregorianCalendar());
	}

	protected StyledLayerDescriptor translate(StyledLayerDescriptorElement sld)
			throws Throwable {
		if (sld == null) {
			return null;
		}
		// FIXME there must be a better way than this
		// String xml = manager.marshal(sld);
		// InputStream in = new StringInputStream(xml);
		// SLDParser stylereader = new SLDParser(
		// CommonFactoryFinder.getStyleFactory(null), in);
		// return stylereader.parseSLD();
		return null;
	}

	public SldParserRegistry getRegistry() {
		return registry;
	}

	public void setRegistry(SldParserRegistry registry) {
		this.registry = registry;
	}

}
