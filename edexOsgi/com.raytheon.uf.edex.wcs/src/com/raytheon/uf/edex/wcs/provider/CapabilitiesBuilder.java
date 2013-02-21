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

import static com.raytheon.uf.edex.wcs.provider.WcsJaxbUtils.getAsLangString;
import static com.raytheon.uf.edex.wcs.provider.WcsJaxbUtils.getKeywords;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.ows.v_1_1_0.AddressType;
import net.opengis.ows.v_1_1_0.CodeType;
import net.opengis.ows.v_1_1_0.ContactType;
import net.opengis.ows.v_1_1_0.KeywordsType;
import net.opengis.ows.v_1_1_0.LanguageStringType;
import net.opengis.ows.v_1_1_0.OnlineResourceType;
import net.opengis.ows.v_1_1_0.ResponsiblePartySubsetType;
import net.opengis.ows.v_1_1_0.ServiceIdentification;
import net.opengis.ows.v_1_1_0.ServiceProvider;
import net.opengis.ows.v_1_1_0.WGS84BoundingBoxType;
import net.opengis.wcs.v_1_1_2.Capabilities;
import net.opengis.wcs.v_1_1_2.Contents;
import net.opengis.wcs.v_1_1_2.CoverageSummaryType;
import net.opengis.wcs.v_1_1_2.ObjectFactory;

import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcServiceInfo;
import com.raytheon.uf.edex.wcs.provider.OgcWcsProvider.WcsOpType;
import com.raytheon.uf.edex.wcs.reg.CoverageDescription;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class CapabilitiesBuilder {

	protected String version = "1.1.2";
	protected String OWS_NS = OgcNamespace.OWS110;

	protected String WCS_NS = OgcNamespace.WCS112;

	protected String updateSequence = "1";

	protected String SERV_TYPE = "WCS";
	protected static final String wcsName = "WCS";
	protected String SERV_TITLE = "EDEX WCS";

	protected static final String svcTitle = "EDEX Coverage Server";

	protected OperationsDescriber opDesc = new OperationsDescriber();

	protected ObjectFactory wcsFactory = new ObjectFactory();

	protected List<String> formats;

	protected net.opengis.ows.v_1_1_0.ObjectFactory owsFactory = new net.opengis.ows.v_1_1_0.ObjectFactory();

	public CapabilitiesBuilder(List<String> formats) {
		this.formats = formats;
	}

	public Capabilities getCapabilities(OgcServiceInfo<WcsOpType> serviceinfo,
			List<CoverageDescription> coverages) {

		Capabilities capabilities = new Capabilities();
		capabilities.setVersion(version);
		// capabilities.setUpdateSequence(updateSequence);
		// capabilities
		// .setServiceIdentification(getServiceIdentification(serviceinfo));
		// capabilities.setServiceProvider(getServiceProvider(serviceinfo));
		capabilities.setOperationsMetadata(opDesc.getOpData(serviceinfo));
		capabilities.setContents(getContents(coverages));

		return capabilities;
	}

	protected Contents getContents(List<CoverageDescription> coverages) {
		Contents rval = new Contents();
		List<CoverageSummaryType> cstList = new ArrayList<CoverageSummaryType>();
		for (CoverageDescription coverage : coverages) {
			cstList.add(getCoverageSummaryType(coverage));
		}
		rval.setSupportedFormat(formats);
		rval.setCoverageSummary(cstList);
		return rval;
	}

	protected List<JAXBElement<WGS84BoundingBoxType>> getBboxes(
			OgcGeoBoundingBox bbox) {
		if (bbox == null) {
			return new ArrayList<JAXBElement<WGS84BoundingBoxType>>(0);
		}
		List<JAXBElement<WGS84BoundingBoxType>> rval = new ArrayList<JAXBElement<WGS84BoundingBoxType>>(
				1);
		WGS84BoundingBoxType to = owsFactory.createWGS84BoundingBoxType();
		to.setLowerCorner(Arrays.asList(bbox.getMinx(), bbox.getMiny()));
		to.setUpperCorner(Arrays.asList(bbox.getMaxx(), bbox.getMaxy()));
		to.setDimensions(BigInteger.valueOf(2));
		rval.add(owsFactory.createWGS84BoundingBox(to));

		return rval;
	}

	protected CoverageSummaryType getCoverageSummaryType(CoverageDescription cov) {
		CoverageSummaryType cst = new CoverageSummaryType();

		cst.setTitle(getAsLangString(cov.getTitle()));
		cst.setAbstract(getAsLangString(cov.getAbstractStr()));
		if (cov.getKeywords() != null) {
			cst.setKeywords(getKeywords(cov.getKeywords()));
		}

		List<JAXBElement<?>> jaxbList = new ArrayList<JAXBElement<?>>();

		jaxbList.addAll(getBboxes(cov.getCrs84Bbox()));

		if (cov.getCrs() != null && cov.getCrs().size() > 0) {
			for (String crs : cov.getCrs()) {
				jaxbList.add(owsFactory.createSupportedCRS(crs));
			}
		}
		if (cov.getIdentifier() != null) {
			jaxbList.add(wcsFactory.createIdentifier(cov.getIdentifier()));
		}
		cst.setContent(jaxbList);
		return cst;
	}

	// the following methods are for optional metadata that aren't currently
	// used

	protected ResponsiblePartySubsetType getResponsibleParty() {
		ResponsiblePartySubsetType rval = new ResponsiblePartySubsetType();
		rval.setIndividualName("Individual Name");
		rval.setPositionName("TBD");

		CodeType ct = new CodeType();
		ct.setCodeSpace("role");
		ct.setValue("TBD");
		rval.setRole(ct);

		// Contact Information
		ContactType contact = new ContactType();
		AddressType address = new AddressType();
		address.setAdministrativeArea("TBD");

		List<String> addressPointList = new ArrayList<String>();
		addressPointList.add("TBD");
		address.setDeliveryPoint(addressPointList);

		address.setCity("TBD");
		address.setCountry("TBD");

		List<String> emailList = new ArrayList<String>();
		emailList.add("TBD");
		address.setElectronicMailAddress(emailList);
		address.setPostalCode("TBD");

		contact.setAddress(address);
		contact.setContactInstructions("TBD");

		rval.setContactInfo(contact);

		return rval;
	}

	protected OnlineResourceType getOLR(OgcServiceInfo<WcsOpType> serviceinfo) {
		OnlineResourceType rval = new OnlineResourceType();

		if (serviceinfo != null && serviceinfo.getOnlineResource() != null) {
			rval.setHref(serviceinfo.getOnlineResource());
		}
		rval.setActuate("TBD");
		rval.setArcrole("TBD");
		rval.setRole("TBD");
		rval.setShow("TBD");
		rval.setTitle("TBD");
		rval.setType("TBD");
		return rval;
	}

	protected ServiceIdentification getServiceIdentification(
			OgcServiceInfo<WcsOpType> serviceinfo) {
		ServiceIdentification rval = new ServiceIdentification();

		// Abstract
		List<LanguageStringType> lstList = new ArrayList<LanguageStringType>();
		LanguageStringType lst = new LanguageStringType();
		lst.setLang(WcsJaxbUtils.DEFAULT_LANGUAGE);
		lst.setValue("TBD");
		rval.setAbstract(lstList);

		// Constraints
		List<String> constraints = new ArrayList<String>();
		constraints.add("NONE");
		rval.setAccessConstraints(constraints);

		// Fees
		rval.setFees("fees");

		// Key Words
		List<KeywordsType> keyWordTypeList = new ArrayList<KeywordsType>();
		KeywordsType kt = new KeywordsType();
		lstList = new ArrayList<LanguageStringType>();
		lst = new LanguageStringType();
		lst.setLang(WcsJaxbUtils.DEFAULT_LANGUAGE);
		lst.setValue("keyword");
		lstList.add(lst);
		kt.setKeyword(lstList);
		keyWordTypeList.add(kt);
		rval.setKeywords(keyWordTypeList);

		// Profile
		List<String> profileList = new ArrayList<String>();
		profileList.add("TBD");
		profileList.add("TBD");
		rval.setProfile(profileList);

		// Service Type
		CodeType codeType = new CodeType();
		codeType.setCodeSpace("codetype");
		rval.setServiceType(codeType);

		// Service Type Version
		List<String> serviceTypeVersion = new ArrayList<String>();
		serviceTypeVersion.add("servicetypeversion");
		rval.setServiceTypeVersion(serviceTypeVersion);

		// Title
		lstList = new ArrayList<LanguageStringType>();
		lst = new LanguageStringType();
		lst.setLang(WcsJaxbUtils.DEFAULT_LANGUAGE);
		lst.setValue(SERV_TITLE);
		lstList.add(lst);
		rval.setTitle(lstList);

		return rval;
	}

	protected ServiceProvider getServiceProvider(
			OgcServiceInfo<WcsOpType> serviceInfo) {
		ServiceProvider rval = new ServiceProvider();
		rval.setProviderName(wcsName);
		rval.setServiceContact(getResponsibleParty());
		rval.setProviderSite(getOLR(serviceInfo));
		return rval;
	}
}
