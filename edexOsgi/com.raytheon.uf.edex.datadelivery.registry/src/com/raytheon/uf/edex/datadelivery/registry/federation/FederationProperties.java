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
package com.raytheon.uf.edex.datadelivery.registry.federation;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.datatype.Duration;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FederationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonNameType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PersonType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.PostalAddressType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.TelephoneNumberType;

import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.edex.datadelivery.util.DataDeliveryIdUtil;

/**
 * 
 * Container class to hold the properties of the registry that will be joining
 * the federation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/22/2013    1707        bphillip    Initial implementation
 * Feb 11, 2014 2771        bgonzale    Removed siteIdentifier field and use Data Delivery ID instead.
 * 2/19/2014    2769        bphillip    Moved getFederationAssociation from RegistryFederationManager
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class FederationProperties {

    /** Constant used for names of registries in the registry database */
    public static final String REGISTRY_SUFFIX = " Registry";

    /**
     * A RegistryType instance MAY have an attribute named conformanceProfile
     * that declares the conformance profile that the server supports. The
     * conformance profiles choices are “RegistryLite” and “RegistryFull” as
     * defined by [regrep-rs-v4.0]
     */
    @XmlElement(required = true)
    private String conformanceProfile;

    /**
     * A RegistryType instance MUST have an attribute named specificationVersion
     * that is the version of the ebXML RegReg Specifications it implements.
     */
    @XmlElement(required = true)
    private String specificationVersion;

    /**
     * A RegistryType instance MAY have an attribute named catalogingLatency
     * that specifies the maximum latency between the time a submission is made
     * to the server and the time it gets cataloged by any cataloging services
     * defined for the objects within the submission. The default value of PT0S
     * indicates a duration of 0 seconds which implies that cataloging happens
     * immediately when request is submitted.
     */
    @XmlElement(required = true)
    private Duration catalogingLatency;

    /**
     * A RegistryType instance MAY have an attribute named
     * replicationSyncLatency that specifies the maximum latency between the
     * time when an original object changes and the time when its replica object
     * within the local server gets updated to synchronize with the new state of
     * the original object. The default value of P1D indicates a duration of
     * once a day.
     */
    @XmlElement(required = true)
    private Duration replicationSyncLatency;

    /**
     * A FederationType instance MAY specify a replicationSyncLatency attribute
     * that describes the time duration that is the amount of time within which
     * a member of this Federation MUST synchronize itself with the current
     * state of the Federation. Members of the Federation MAY use this parameter
     * to periodically synchronize the federation metadata they MUST cache
     * locally about the state of the Federation and its members. Such
     * synchronization MAY be based upon the registry event notification
     * capability.
     */
    @XmlElement(required = true)
    private Duration federationReplicationSyncLatency;

    /** Description of this site */
    @XmlElement
    private String siteDescription = "";

    /** The site primary contact's first name */
    @XmlElement(required = true)
    private String sitePrimaryContactFirstName;

    /** The site primary contact's middle name */
    @XmlElement
    private String sitePrimaryContactMiddleName = "";

    /** The site primary contact's last name */
    @XmlElement(required = true)
    private String sitePrimaryContactLastName;

    /** The site primary contact's phone area code */
    @XmlElement
    private String sitePrimaryContactPhoneAreaCode = "";

    /** The site primary contact's phone number */
    @XmlElement
    private String sitePrimaryContactPhoneNumber = "";

    /** The site's street address number */
    @XmlElement
    private String siteAddressStreetNumber = "";

    /** The site's address street name */
    @XmlElement
    private String siteAddressStreet = "";

    /** The site's address city */
    @XmlElement
    private String siteAddressCity = "";

    /** The site's address state or province */
    @XmlElement
    private String siteAddressState = "";

    /** The site's address country */
    @XmlElement
    private String siteAddressCountry = "";

    /** The site's address postal code */
    @XmlElement
    private String siteAddressPostalCode = "";

    /** The site phone's area code */
    @XmlElement
    private String sitePhoneAreaCode = "";

    /** The site's phone number */
    @XmlElement
    private String sitePhoneNumber = "";

    /**
     * Creates a new FederationProperties object
     */
    public FederationProperties() {

    }

    /**
     * Creates a RegistryType object based on the properties
     * 
     * @return A RegistryType object based on the properties
     */
    public RegistryType createRegistryObject() {
        RegistryType registryObj = new RegistryType();
        registryObj.setId(DataDeliveryIdUtil.getId() + REGISTRY_SUFFIX);
        registryObj.setLid(registryObj.getId());
        registryObj.setName(RegistryUtil
                .getInternationalString(DataDeliveryIdUtil.getId()
                        + " Registry Specification"));
        registryObj.setObjectType(RegistryObjectTypes.REGISTRY);
        registryObj.setDescription(registryObj.getName());
        registryObj.setOwner(DataDeliveryIdUtil.getId());
        registryObj.setStatus(StatusTypes.APPROVED);
        registryObj.setCatalogingLatency(catalogingLatency);
        registryObj.setConformanceProfile(conformanceProfile);
        registryObj.setOperator(RegistryUtil.DEFAULT_OWNER);
        registryObj.setReplicationSyncLatency(replicationSyncLatency);
        registryObj.setSpecificationVersion(specificationVersion);
        registryObj.setBaseURL(RegistryUtil.LOCAL_REGISTRY_ADDRESS);
        return registryObj;
    }

    /**
     * Creates a PersonType object representing the primary contact for this
     * registry based on the fields in this class
     * 
     * @return The PersonType object
     */
    public PersonType createPrimaryContactPerson() {

        TelephoneNumberType phone = new TelephoneNumberType();
        phone.setAreaCode(sitePrimaryContactPhoneAreaCode);
        phone.setNumber(sitePrimaryContactPhoneNumber);

        PersonType person = new PersonType();
        person.setId(DataDeliveryIdUtil.getId() + " Primary Contact");
        person.setLid(person.getId());
        person.setName(RegistryUtil.getInternationalString(person.getId()));
        person.setDescription(person.getName());
        person.setStatus(StatusTypes.APPROVED);
        person.setObjectType(RegistryObjectTypes.PERSON);
        person.setOwner(DataDeliveryIdUtil.getId());
        PersonNameType personName = new PersonNameType();
        personName.setFirstName(sitePrimaryContactFirstName);
        personName.setMiddleName(sitePrimaryContactMiddleName);
        personName.setLastName(sitePrimaryContactLastName);
        person.setPersonName(personName);
        person.getPostalAddress().add(getSiteAddress());
        person.getTelephoneNumber().add(phone);
        return person;
    }

    /**
     * Creates an OrganizationType object representing this registry based on
     * the fields in this class
     * 
     * @return The OrganizationType object
     */
    public OrganizationType createOrganization() {
        OrganizationType org = new OrganizationType();
        org.setPrimaryContact(DataDeliveryIdUtil.getId() + " Primary Contact");
        org.setId(DataDeliveryIdUtil.getId());
        org.setLid(org.getId());
        org.setName(RegistryUtil
                .getInternationalString("National Weather Service Office: "
                        + DataDeliveryIdUtil.getId()));
        org.setDescription(org.getName());
        org.setStatus(StatusTypes.APPROVED);
        org.setObjectType(RegistryObjectTypes.ORGANIZATION);
        org.setOwner(DataDeliveryIdUtil.getId());
        TelephoneNumberType phone = new TelephoneNumberType();
        phone.setAreaCode(sitePhoneAreaCode);
        phone.setNumber(sitePhoneNumber);
        org.getTelephoneNumber().add(phone);
        org.getPostalAddress().add(getSiteAddress());
        return org;
    }

    /**
     * Gets the site address of the organization hosting this registry based on
     * the fields in this object
     * 
     * @return The Postal address
     */
    private PostalAddressType getSiteAddress() {
        PostalAddressType siteAddress = new PostalAddressType();
        siteAddress.setStreetNumber(siteAddressStreetNumber);
        siteAddress.setStreet(siteAddressStreet);
        siteAddress.setCity(siteAddressCity);
        siteAddress.setStateOrProvince(siteAddressState);
        siteAddress.setCountry(siteAddressCountry);
        siteAddress.setPostalCode(siteAddressPostalCode);
        return siteAddress;
    }

    protected AssociationType getFederationAssociation(RegistryType registry,
            FederationType federation) {
        AssociationType association = new AssociationType();
        association.setId(registry.getId()
                + " Federation Membership Association");
        association.setLid(association.getId());
        association.setObjectType(RegistryObjectTypes.ASSOCIATION);
        association.setOwner(DataDeliveryIdUtil.getId());
        association.setType(AssociationTypes.HAS_FEDERATION_MEMBER);
        association.setStatus(StatusTypes.APPROVED);
        association.setName(RegistryUtil.getInternationalString(registry
                .getId() + " Federation Membership"));
        association.setDescription(association.getName());
        association.setTargetObject(registry.getId());
        association.setSourceObject(federation.getId());
        return association;
    }

    public String getConformanceProfile() {
        return conformanceProfile;
    }

    public void setConformanceProfile(String conformanceProfile) {
        this.conformanceProfile = conformanceProfile;
    }

    public String getSpecificationVersion() {
        return specificationVersion;
    }

    public void setSpecificationVersion(String specificationVersion) {
        this.specificationVersion = specificationVersion;
    }

    public Duration getCatalogingLatency() {
        return catalogingLatency;
    }

    public void setCatalogingLatency(Duration catalogingLatency) {
        this.catalogingLatency = catalogingLatency;
    }

    public Duration getReplicationSyncLatency() {
        return replicationSyncLatency;
    }

    public void setReplicationSyncLatency(Duration replicationSyncLatency) {
        this.replicationSyncLatency = replicationSyncLatency;
    }

    public Duration getFederationReplicationSyncLatency() {
        return federationReplicationSyncLatency;
    }

    public void setFederationReplicationSyncLatency(
            Duration federationReplicationSyncLatency) {
        this.federationReplicationSyncLatency = federationReplicationSyncLatency;
    }

    public String getSiteDescription() {
        return siteDescription;
    }

    public void setSiteDescription(String siteDescription) {
        this.siteDescription = siteDescription;
    }

    public String getSitePrimaryContactFirstName() {
        return sitePrimaryContactFirstName;
    }

    public void setSitePrimaryContactFirstName(
            String sitePrimaryContactFirstName) {
        this.sitePrimaryContactFirstName = sitePrimaryContactFirstName;
    }

    public String getSitePrimaryContactMiddleName() {
        return sitePrimaryContactMiddleName;
    }

    public void setSitePrimaryContactMiddleName(
            String sitePrimaryContactMiddleName) {
        this.sitePrimaryContactMiddleName = sitePrimaryContactMiddleName;
    }

    public String getSitePrimaryContactLastName() {
        return sitePrimaryContactLastName;
    }

    public void setSitePrimaryContactLastName(String sitePrimaryContactLastName) {
        this.sitePrimaryContactLastName = sitePrimaryContactLastName;
    }

    public String getSitePrimaryContactPhoneAreaCode() {
        return sitePrimaryContactPhoneAreaCode;
    }

    public void setSitePrimaryContactPhoneAreaCode(
            String sitePrimaryContactPhoneAreaCode) {
        this.sitePrimaryContactPhoneAreaCode = sitePrimaryContactPhoneAreaCode;
    }

    public String getSitePrimaryContactPhoneNumber() {
        return sitePrimaryContactPhoneNumber;
    }

    public void setSitePrimaryContactPhoneNumber(
            String sitePrimaryContactPhoneNumber) {
        this.sitePrimaryContactPhoneNumber = sitePrimaryContactPhoneNumber;
    }

    public String getSiteAddressStreetNumber() {
        return siteAddressStreetNumber;
    }

    public void setSiteAddressStreetNumber(String siteAddressStreetNumber) {
        this.siteAddressStreetNumber = siteAddressStreetNumber;
    }

    public String getSiteAddressStreet() {
        return siteAddressStreet;
    }

    public void setSiteAddressStreet(String siteAddressStreet) {
        this.siteAddressStreet = siteAddressStreet;
    }

    public String getSiteAddressCity() {
        return siteAddressCity;
    }

    public void setSiteAddressCity(String siteAddressCity) {
        this.siteAddressCity = siteAddressCity;
    }

    public String getSiteAddressState() {
        return siteAddressState;
    }

    public void setSiteAddressState(String siteAddressState) {
        this.siteAddressState = siteAddressState;
    }

    public String getSiteAddressCountry() {
        return siteAddressCountry;
    }

    public void setSiteAddressCountry(String siteAddressCountry) {
        this.siteAddressCountry = siteAddressCountry;
    }

    public String getSiteAddressPostalCode() {
        return siteAddressPostalCode;
    }

    public void setSiteAddressPostalCode(String siteAddressPostalCode) {
        this.siteAddressPostalCode = siteAddressPostalCode;
    }

    public String getSitePhoneAreaCode() {
        return sitePhoneAreaCode;
    }

    public void setSitePhoneAreaCode(String sitePhoneAreaCode) {
        this.sitePhoneAreaCode = sitePhoneAreaCode;
    }

    public String getSitePhoneNumber() {
        return sitePhoneNumber;
    }

    public void setSitePhoneNumber(String sitePhoneNumber) {
        this.sitePhoneNumber = sitePhoneNumber;
    }

}
