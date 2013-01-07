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

package oasis.names.tc.ebxml.regrep.xsd.rim.v4;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * This object contains factory methods for each Java content interface and Java
 * element interface generated in the oasis.names.tc.ebxml_regrep.xsd.rim._4
 * package.
 * <p>
 * An ObjectFactory allows you to programatically construct new instances of the
 * Java representation for XML content. The Java representation of XML content
 * can consist of schema derived interfaces and classes representing the binding
 * of schema type definitions, element declarations and model groups. Factory
 * methods for each of these are provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory implements ISerializableObject {

    private final static QName _IdentifiableList_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", "IdentifiableList");

    private final static QName _ObjectRefList_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", "ObjectRefList");

    private final static QName _Notification_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", "Notification");

    private final static QName _RegistryObject_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", "RegistryObject");

    private final static QName _RegistryObjectList_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", "RegistryObjectList");

    private final static QName _ObjectRef_QNAME = new QName(
            "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", "ObjectRef");

    /**
     * Create a new ObjectFactory that can be used to create new instances of
     * schema derived classes for package:
     * oasis.names.tc.ebxml_regrep.xsd.rim._4
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link SubscriptionType }
     * 
     */
    public SubscriptionType createSubscriptionType() {
        return new SubscriptionType();
    }

    /**
     * Create an instance of {@link ServiceType }
     * 
     */
    public ServiceType createServiceType() {
        return new ServiceType();
    }

    /**
     * Create an instance of {@link ExtrinsicObjectType }
     * 
     */
    public ExtrinsicObjectType createExtrinsicObjectType() {
        return new ExtrinsicObjectType();
    }

    /**
     * Create an instance of {@link CommentType }
     * 
     */
    public CommentType createCommentType() {
        return new CommentType();
    }

    /**
     * Create an instance of {@link DeliveryInfoType }
     * 
     */
    public DeliveryInfoType createDeliveryInfoType() {
        return new DeliveryInfoType();
    }

    /**
     * Create an instance of {@link DynamicObjectRefType }
     * 
     */
    public DynamicObjectRefType createDynamicObjectRefType() {
        return new DynamicObjectRefType();
    }

    /**
     * Create an instance of {@link ServiceInterfaceType }
     * 
     */
    public ServiceInterfaceType createServiceInterfaceType() {
        return new ServiceInterfaceType();
    }

    /**
     * Create an instance of {@link ExternalIdentifierType }
     * 
     */
    public ExternalIdentifierType createExternalIdentifierType() {
        return new ExternalIdentifierType();
    }

    /**
     * Create an instance of {@link LocalizedStringType }
     * 
     */
    public LocalizedStringType createLocalizedStringType() {
        return new LocalizedStringType();
    }

    /**
     * Create an instance of {@link QueryDefinitionType }
     * 
     */
    public QueryDefinitionType createQueryDefinitionType() {
        return new QueryDefinitionType();
    }

    /**
     * Create an instance of {@link IntegerValueType }
     * 
     */
    public IntegerValueType createIntegerValueType() {
        return new IntegerValueType();
    }

    /**
     * Create an instance of {@link ClassificationNodeType }
     * 
     */
    public ClassificationNodeType createClassificationNodeType() {
        return new ClassificationNodeType();
    }

    /**
     * Create an instance of {@link MapValueType }
     * 
     */
    public MapValueType createMapValueType() {
        return new MapValueType();
    }

    /**
     * Create an instance of {@link RegistryType }
     * 
     */
    public RegistryType createRegistryType() {
        return new RegistryType();
    }

    /**
     * Create an instance of {@link ObjectRefType }
     * 
     */
    public ObjectRefType createObjectRefType() {
        return new ObjectRefType();
    }

    /**
     * Create an instance of {@link InternationalStringType }
     * 
     */
    public InternationalStringType createInternationalStringType() {
        return new InternationalStringType();
    }

    /**
     * Create an instance of {@link ObjectRefListType }
     * 
     */
    public ObjectRefListType createObjectRefListType() {
        return new ObjectRefListType();
    }

    /**
     * Create an instance of {@link OrganizationType }
     * 
     */
    public OrganizationType createOrganizationType() {
        return new OrganizationType();
    }

    /**
     * Create an instance of {@link RegistryPackageType }
     * 
     */
    public RegistryPackageType createRegistryPackageType() {
        return new RegistryPackageType();
    }

    /**
     * Create an instance of {@link VocabularyTermType }
     * 
     */
    public VocabularyTermType createVocabularyTermType() {
        return new VocabularyTermType();
    }

    /**
     * Create an instance of {@link SlotValueType }
     * 
     */
    public SlotValueType createSlotValueType() {
        return new SlotValueType();
    }

    /**
     * Create an instance of {@link FederationType }
     * 
     */
    public FederationType createFederationType() {
        return new FederationType();
    }

    /**
     * Create an instance of {@link EntryType }
     * 
     */
    public EntryType createEntryType() {
        return new EntryType();
    }

    /**
     * Create an instance of {@link AssociationType }
     * 
     */
    public AssociationType createAssociationType() {
        return new AssociationType();
    }

    /**
     * Create an instance of {@link RegistryObjectListType }
     * 
     */
    public RegistryObjectListType createRegistryObjectListType() {
        return new RegistryObjectListType();
    }

    /**
     * Create an instance of {@link VersionInfoType }
     * 
     */
    public VersionInfoType createVersionInfoType() {
        return new VersionInfoType();
    }

    /**
     * Create an instance of {@link PostalAddressType }
     * 
     */
    public PostalAddressType createPostalAddressType() {
        return new PostalAddressType();
    }

    /**
     * Create an instance of {@link FloatValueType }
     * 
     */
    public FloatValueType createFloatValueType() {
        return new FloatValueType();
    }

    /**
     * Create an instance of {@link XMLQueryExpressionType }
     * 
     */
    public XMLQueryExpressionType createXMLQueryExpressionType() {
        return new XMLQueryExpressionType();
    }

    /**
     * Create an instance of {@link EmailAddressType }
     * 
     */
    public EmailAddressType createEmailAddressType() {
        return new EmailAddressType();
    }

    /**
     * Create an instance of {@link VocabularyTermValueType }
     * 
     */
    public VocabularyTermValueType createVocabularyTermValueType() {
        return new VocabularyTermValueType();
    }

    /**
     * Create an instance of {@link ServiceEndpointType }
     * 
     */
    public ServiceEndpointType createServiceEndpointType() {
        return new ServiceEndpointType();
    }

    /**
     * Create an instance of {@link SimpleLinkType }
     * 
     */
    public SimpleLinkType createSimpleLinkType() {
        return new SimpleLinkType();
    }

    /**
     * Create an instance of {@link ClassificationType }
     * 
     */
    public ClassificationType createClassificationType() {
        return new ClassificationType();
    }

    /**
     * Create an instance of {@link RoleType }
     * 
     */
    public RoleType createRoleType() {
        return new RoleType();
    }

    /**
     * Create an instance of {@link RegistryObjectType }
     * 
     */
    public RegistryObjectType createRegistryObjectType() {
        return new RegistryObjectType();
    }

    /**
     * Create an instance of {@link PersonType }
     * 
     */
    public PersonType createPersonType() {
        return new PersonType();
    }

    /**
     * Create an instance of {@link AnyValueType }
     * 
     */
    public AnyValueType createAnyValueType() {
        return new AnyValueType();
    }

    /**
     * Create an instance of {@link TelephoneNumberType }
     * 
     */
    public TelephoneNumberType createTelephoneNumberType() {
        return new TelephoneNumberType();
    }

    /**
     * Create an instance of {@link WorkflowActionType }
     * 
     */
    public WorkflowActionType createWorkflowActionType() {
        return new WorkflowActionType();
    }

    /**
     * Create an instance of {@link StringValueType }
     * 
     */
    public StringValueType createStringValueType() {
        return new StringValueType();
    }

    /**
     * Create an instance of {@link MapType }
     * 
     */
    public MapType createMapType() {
        return new MapType();
    }

    /**
     * Create an instance of {@link CollectionValueType }
     * 
     */
    public CollectionValueType createCollectionValueType() {
        return new CollectionValueType();
    }

    /**
     * Create an instance of {@link IdentifiableListType }
     * 
     */
    public IdentifiableListType createIdentifiableListType() {
        return new IdentifiableListType();
    }

    /**
     * Create an instance of {@link ActionType }
     * 
     */
    public ActionType createActionType() {
        return new ActionType();
    }

    /**
     * Create an instance of {@link InternationalStringValueType }
     * 
     */
    public InternationalStringValueType createInternationalStringValueType() {
        return new InternationalStringValueType();
    }

    /**
     * Create an instance of {@link AuditableEventType }
     * 
     */
    public AuditableEventType createAuditableEventType() {
        return new AuditableEventType();
    }

    /**
     * Create an instance of {@link DateTimeValueType }
     * 
     */
    public DateTimeValueType createDateTimeValueType() {
        return new DateTimeValueType();
    }

    /**
     * Create an instance of {@link PersonNameType }
     * 
     */
    public PersonNameType createPersonNameType() {
        return new PersonNameType();
    }

    /**
     * Create an instance of {@link QueryType }
     * 
     */
    public QueryType createQueryType() {
        return new QueryType();
    }

    /**
     * Create an instance of {@link ClassificationSchemeType }
     * 
     */
    public ClassificationSchemeType createClassificationSchemeType() {
        return new ClassificationSchemeType();
    }

    /**
     * Create an instance of {@link StringQueryExpressionType }
     * 
     */
    public StringQueryExpressionType createStringQueryExpressionType() {
        return new StringQueryExpressionType();
    }

    /**
     * Create an instance of {@link ParameterType }
     * 
     */
    public ParameterType createParameterType() {
        return new ParameterType();
    }

    /**
     * Create an instance of {@link NotificationType }
     * 
     */
    public NotificationType createNotificationType() {
        return new NotificationType();
    }

    /**
     * Create an instance of {@link BooleanValueType }
     * 
     */
    public BooleanValueType createBooleanValueType() {
        return new BooleanValueType();
    }

    /**
     * Create an instance of {@link SlotType }
     * 
     */
    public SlotType createSlotType() {
        return new SlotType();
    }

    /**
     * Create an instance of {@link ExternalLinkType }
     * 
     */
    public ExternalLinkType createExternalLinkType() {
        return new ExternalLinkType();
    }

    /**
     * Create an instance of {@link DurationValueType }
     * 
     */
    public DurationValueType createDurationValueType() {
        return new DurationValueType();
    }

    /**
     * Create an instance of {@link ServiceBindingType }
     * 
     */
    public ServiceBindingType createServiceBindingType() {
        return new ServiceBindingType();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}
     * {@link IdentifiableListType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", name = "IdentifiableList")
    public JAXBElement<IdentifiableListType> createIdentifiableList(
            IdentifiableListType value) {
        return new JAXBElement<IdentifiableListType>(_IdentifiableList_QNAME,
                IdentifiableListType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}
     * {@link ObjectRefListType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", name = "ObjectRefList")
    public JAXBElement<ObjectRefListType> createObjectRefList(
            ObjectRefListType value) {
        return new JAXBElement<ObjectRefListType>(_ObjectRefList_QNAME,
                ObjectRefListType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}
     * {@link NotificationType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", name = "Notification")
    public JAXBElement<NotificationType> createNotification(
            NotificationType value) {
        return new JAXBElement<NotificationType>(_Notification_QNAME,
                NotificationType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}
     * {@link RegistryObjectType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", name = "RegistryObject")
    public JAXBElement<RegistryObjectType> createRegistryObject(
            RegistryObjectType value) {
        return new JAXBElement<RegistryObjectType>(_RegistryObject_QNAME,
                RegistryObjectType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}
     * {@link RegistryObjectListType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", name = "RegistryObjectList")
    public JAXBElement<RegistryObjectListType> createRegistryObjectList(
            RegistryObjectListType value) {
        return new JAXBElement<RegistryObjectListType>(
                _RegistryObjectList_QNAME, RegistryObjectListType.class, null,
                value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ObjectRefType }
     * {@code >}
     * 
     */
    @XmlElementDecl(namespace = "urn:oasis:names:tc:ebxml-regrep:xsd:rim:4.0", name = "ObjectRef")
    public JAXBElement<ObjectRefType> createObjectRef(ObjectRefType value) {
        return new JAXBElement<ObjectRefType>(_ObjectRef_QNAME,
                ObjectRefType.class, null, value);
    }

}
