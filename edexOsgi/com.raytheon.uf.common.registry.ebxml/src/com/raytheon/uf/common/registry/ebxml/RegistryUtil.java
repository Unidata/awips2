package com.raytheon.uf.common.registry.ebxml;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.InternationalStringType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.LocalizedStringType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.registry.OperationStatus;
import com.raytheon.uf.common.registry.RegistryException;
import com.raytheon.uf.common.registry.RegistryQuery;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.registry.annotations.AssociationMapping;
import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.RegistryObjectAssociation;
import com.raytheon.uf.common.registry.annotations.RegistryObjectDescription;
import com.raytheon.uf.common.registry.annotations.RegistryObjectName;
import com.raytheon.uf.common.registry.annotations.RegistryObjectOwner;
import com.raytheon.uf.common.registry.annotations.RegistryObjectVersion;
import com.raytheon.uf.common.registry.annotations.SlotAttribute;
import com.raytheon.uf.common.registry.annotations.SlotAttributeConverter;
import com.raytheon.uf.common.registry.constants.Languages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryErrorMessage;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.ebxml.encoder.IRegistryEncoder;
import com.raytheon.uf.common.registry.ebxml.slots.BooleanSlotConverter;
import com.raytheon.uf.common.registry.ebxml.slots.DateSlotConverter;
import com.raytheon.uf.common.registry.ebxml.slots.DoubleSlotConverter;
import com.raytheon.uf.common.registry.ebxml.slots.EnumSlotConverter;
import com.raytheon.uf.common.registry.ebxml.slots.IntegerSlotConverter;
import com.raytheon.uf.common.registry.ebxml.slots.SlotConverter;
import com.raytheon.uf.common.registry.ebxml.slots.StringSlotConverter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.ReflectionException;
import com.raytheon.uf.common.util.ReflectionUtil;

/**
 * Utility class for common Registry activities.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 356        jspinks     Initial creation
 * May 15, 2012 455        jspinks     Modified for storing associations between registry objects.
 * Jul 10, 2012 455        djohnson    Remove synchronization from SLOT_CONVERSION, add some constants.
 * Jul 24, 2012 955        djohnson    Check for a specific object type to be specified on {@link RegistryObject}.
 * 8/3/2012     724        bphillip    Additions to support assigning ownership to registry objects
 * Aug 20, 2012 0743       djohnson    Slot converter for {@link ImmutableDate}, and enumerations.
 * Sep 07, 2012 1102       djohnson    Check in hanging around encoding strategy code that will prove useful later.
 * Oct 05, 2012 1195       djohnson    Don't persist slots for null values.
 * 4/9/2013     1802       bphillip    Pulled constants out into existing constants package that was moved into common
 * Jun 03, 2013 2038       djohnson    Allow setting the same encoder strategy.
 * Jun 24, 2013 2106       djohnson    Remove encoder strategy from instance variables.
 * Dec 04, 2013 2584       dhladky     Versions for Registry objects
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public final class RegistryUtil {

    private RegistryUtil() {
        // Prevent Instantiation
    }

    public static String LOCAL_REGISTRY_ADDRESS = null;
    
    public static final String registryObjectClassName = "registryObjectClassName";
    
    public static final String registryObjectDefaultVersion = "1.0";

    static {
        if (System.getenv("EBXML_REGISTRY_HOST") != null
                && System.getenv("EBXML_REGISTRY_WEBSERVER_PORT") != null) {
            LOCAL_REGISTRY_ADDRESS = "http://"
                    + System.getenv("EBXML_REGISTRY_HOST") + ":"
                    + System.getenv("EBXML_REGISTRY_WEBSERVER_PORT");
        }

    }

    public static final String CALLING_REGISTRY_SOAP_HEADER_NAME = "Calling_Registry";

    /**
     * The default internal owner
     */
    public static final String DEFAULT_OWNER = "NCF";

    // A private mapping of attribute types to slot types, used when storing an
    // object to the registry to map QueryableAttributes to SlotConverters.
    private static final Map<String, SlotConverter> SLOT_CONVERSION;

    static {
        Map<String, SlotConverter> map = new HashMap<String, SlotConverter>(11);
        // Load the SLOT_CONVERTER map, have to use the String equivalent of the
        // key since Class is not comparable... (which keys have to be).
        map.put(Long.class.getName(), IntegerSlotConverter.INSTANCE);
        map.put(String.class.getName(), StringSlotConverter.INSTANCE);
        map.put(Integer.class.getName(), IntegerSlotConverter.INSTANCE);
        map.put(BigInteger.class.getName(), IntegerSlotConverter.INSTANCE);
        map.put(int.class.getSimpleName(), IntegerSlotConverter.INSTANCE);

        map.put(Float.class.getName(), DoubleSlotConverter.INSTANCE);
        map.put(Double.class.getName(), DoubleSlotConverter.INSTANCE);
        map.put(float.class.getSimpleName(), DoubleSlotConverter.INSTANCE);
        map.put(double.class.getSimpleName(), DoubleSlotConverter.INSTANCE);

        map.put(boolean.class.getSimpleName(), BooleanSlotConverter.INSTANCE);
        map.put(Boolean.class.getName(), BooleanSlotConverter.INSTANCE);
        map.put(Date.class.getName(), DateSlotConverter.INSTANCE);
        map.put(ImmutableDate.class.getName(), DateSlotConverter.INSTANCE);
        SLOT_CONVERSION = Collections.unmodifiableMap(map);
    }

    /**
     * Creates a slot of the given type.
     * 
     * @param slotType
     *            The type of slot to create
     * @param slotName
     *            The name of the slot
     * @param slotValue
     *            The value of the slot
     * @return The SlotType object
     * @throws Exception
     *             If errors occur while creating the slot
     */
    public static SlotType getSlot(String slotType, String slotName,
            Object slotValue) {
        SlotConverter converter = SLOT_CONVERSION.get(slotType);
        if (converter == null) {
            throw new RegistryException("Not slot converter for type "
                    + slotType);
        }
        return converter.getSlots(slotName, slotValue).get(0);
    }

    /**
     * Generate a unique id for a registry object.
     * 
     * @return A random UUID as a String.
     * 
     */
    public static String generateRegistryObjectId() {

        return java.util.UUID.randomUUID().toString();
    }

    /**
     * Create the qualified registry object type name for a given Class.
     * 
     * @param object
     * @return A registry object type.
     */
    public static String getObjectType(Class<?> object) {
        return RegistryObjectTypes.REGISTRY_OBJECT + ":" + object.getName();
    }

    public static String getObjectTypePath(Class<?> object) {
        return RegistryObjectTypes.REGISTRY_OBJECT + object.getName();
    }

    /**
     * Translate a RegistryQuery into a QueryRequest to send to the
     * QueryManager.
     * 
     * @param registryQuery
     *            The RegistryQuery to translate.
     * 
     * @return A QueryRequest to send to the QueryManager to retrieve registry
     *         Objects.
     */
    public static <T> QueryRequest getQuery(RegistryQuery<T> registryQuery) {

        QueryRequest queryRequest = new QueryRequest();
        queryRequest.setId(RegistryUtil.generateRegistryObjectId());
        ResponseOptionType responseOption = new ResponseOptionType();
        responseOption.setReturnComposedObjects(true);
        responseOption.setReturnType(QueryReturnTypes.REGISTRY_OBJECT);

        QueryType query = new QueryType();
        query.setQueryDefinition(registryQuery.getQueryType());

        // Add the slots to the query Object.
        query.getSlot().addAll(registryQuery.getSlots());

        // Add the query to the QueryRequest
        queryRequest.setQuery(query);
        queryRequest.setResponseOption(responseOption);

        return queryRequest;
    }

    /**
     * Interrogate an Object annotated with @RegistryObject to get the registry
     * object id that should be used for the Object.
     * 
     * @param object
     *            The Object to try and generate a registry object id for.
     * 
     * @return The object id or null if no key could be generated.
     * 
     * @throws ReflectionException
     */
    public static String getRegistryObjectKey(Object object)
            throws ReflectionException {

        String registryObjectId = null;

        // Walk the Class hierarchy to find the RegistryObject annotation.
        for (Class<?> c = object.getClass(); c.getSuperclass() != null; c = c
                .getSuperclass()) {

            if (c.isAnnotationPresent(RegistryObject.class)) {

                RegistryObject ro = c.getAnnotation(RegistryObject.class);

                // Get the columnNames used to generate the ID, if present.
                String[] columnNames = ro.value();
                StringBuilder keyBuilder = new StringBuilder();

                // TODO: Check for duplication/invalid names?
                if (!CollectionUtil.isNullOrEmpty(columnNames)) {
                    for (String columnName : columnNames) {

                        Object v = ReflectionUtil.getter(object, columnName);
                        if (v != null) {
                            String key = "";
                            if (isRegistryObject(v)) {
                                key = getRegistryObjectKey(v);
                            } else {
                                key = v.toString().trim();
                            }

                            if (key.length() > 0) {
                                keyBuilder.append(key);
                                keyBuilder.append("-");
                            }
                        }
                    }

                    String t = keyBuilder.toString();
                    registryObjectId = t.substring(0, t.length() - 1);

                }

                // Break out of the for loop and return the registryObjectId.
                break;
            }

        }

        return registryObjectId;
    }

    /**
     * Get the SlotConverter needed for a Class attribute annotated with
     * <code>@SlotAttribute</code>.
     * 
     * @param The
     *            attribute Class that needs to be converted.
     * 
     * @return The SlotConverter to use for the Class provided.
     * 
     * @throws ReflectionException
     */
    public static SlotConverter getSlotConversion(Class<?> class1)
            throws ReflectionException {
        if (Enum.class.isAssignableFrom(class1)) {
            return EnumSlotConverter.INSTANCE;
        } else {
            return SLOT_CONVERSION.get(class1.getName());
        }
    }

    /**
     * Creates a {@link RegistryObjectType} for the object.
     * 
     * @param object
     *            the object
     * @return the RegistryObjectType
     * @throws SerializationException
     *             on error serializing the object
     * @throws ReflectionException
     *             on error reflectively accessing the object
     */
    public static RegistryObjectType newRegistryObject(Object object,
            IRegistryEncoder encoderStrategy) throws SerializationException,
            ReflectionException {

        RegistryObjectType registryObject = new RegistryObjectType();
        String registryObjectType = null;
        String registryObjectId = null;
        List<SlotType> slots = new ArrayList<SlotType>();

        boolean registryObjectNotFound = true;

        // Walk up the inheritance tree to find all Classes
        for (Class<?> c = object.getClass(); c.getSuperclass() != null; c = c
                .getSuperclass()) {

            // Is this the first instance of a Class annotated with
            // @RegistryObject?
            if (registryObjectNotFound
                    && c.isAnnotationPresent(RegistryObject.class)) {

                // Skip all other possible declarations of RegistryObject
                registryObjectNotFound = false;

                // Generate the id for the Object based on it's annotation or
                // generate a random one if no information was supplied in the
                // annotation.
                registryObjectId = RegistryUtil.getRegistryObjectKey(object);
                if (registryObjectId == null) {
                    registryObjectId = RegistryUtil.generateRegistryObjectId();
                }

                // TODO: This will change? once a versioning strategy is decided
                // on
                registryObject.setId(registryObjectId);
                registryObject.setLid(registryObjectId);

                RegistryObject ro = c.getAnnotation(RegistryObject.class);

                // Use the specified object type name or the current class name
                // as the Object type if not specified.
                Class<?> objectType = ro.objectType();
                registryObjectType = (Object.class == objectType) ? c.getName()
                        : objectType.getName();

                if (ro.storeContent()) {
                    // Store the Base64 encoded Object in a slot called
                    // "content"
                    slots.add(encoderStrategy.encodeObject(object));
                }
                // Set the ObjectType so it can be distinguished from other
                // Objects
                // more easily.
                registryObject
                        .setObjectType(RegistryObjectTypes.REGISTRY_OBJECT
                                + ":" + registryObjectType);

                String objectOwner = ReflectionUtil.getAnnotatedField(
                        registryObject, RegistryObjectOwner.class);
                registryObject.setOwner(objectOwner);
                if (objectOwner == null) {
                    registryObject.setOwner(DEFAULT_OWNER);
                }

                registryObject.setName(getInternationalString(ReflectionUtil
                        .getAnnotatedField(registryObject,
                                RegistryObjectName.class)));
                registryObject
                        .setDescription(getInternationalString(ReflectionUtil
                                .getAnnotatedField(registryObject,
                                        RegistryObjectDescription.class)));
                // Try to harvest the current version from the PayloadObject
                // if none exists, default to encoder provided numeric.
                VersionInfoType version = new VersionInfoType();
                String val = null;
                RegistryObjectVersion rov = ReflectionUtil
                        .getAnnotationFromClass(object.getClass(),
                                RegistryObjectVersion.class);
                if (rov != null) {
                    val = String.valueOf(rov.value());
                }
                // no value set in annotation field, apply version default.
                if (val == null) {
                    // default
                    val = registryObjectDefaultVersion;
                }
                version.setUserVersionName(val);
                registryObject.setVersionInfo(version);
                
                // We need the actual payload class, not just it's ID for version comparisons
                String clazz = object.getClass().getCanonicalName();
                SlotType classNameSlot = new SlotType(registryObjectClassName, new StringValueType(clazz));
                slots.add(classNameSlot);
            }

            // Look through all fields that need to be persisted to the
            // registry.
            for (Field f : c.getDeclaredFields()) {

                // Only interested in fields annotated with SlotAttribute
                if (f.isAnnotationPresent(SlotAttribute.class)) {

                    SlotAttribute slotAttribute = f
                            .getAnnotation(SlotAttribute.class);
                    SlotConverter converter;

                    String z = f.getName();

                    // If the attribute has a SlotConverter specified, get
                    // an instance of it.
                    if (f.isAnnotationPresent(SlotAttributeConverter.class)) {
                        converter = ReflectionUtil.newInstanceOfAssignableType(
                                SlotConverter.class,
                                f.getAnnotation(SlotAttributeConverter.class)
                                        .value());
                    }
                    // If a converter was not specified, look one up based
                    // on field type
                    else {
                        converter = RegistryUtil.getSlotConversion(f.getType());
                    }

                    // Get the value of the attribute using a "getter"
                    Object v = ReflectionUtil.getter(object, z);
                    // Skip a null field
                    if (v == null) {
                        continue;
                    }

                    // If a slot name has been specified, use that value
                    // instead.
                    String slotName = slotAttribute.value();
                    if (!slotName.isEmpty()) {
                        z = slotName;
                    }

                    slots.addAll(converter.getSlots(z, v));
                }
            }

            registryObject.getSlot().addAll(slots);

        }

        return registryObject;
    }

    public static Map<String, RegistryObjectType> getAssociatedObjects(
            Object object, IRegistryEncoder encoderStrategy)
            throws ReflectionException, SerializationException {

        Map<String, RegistryObjectType> ids = new HashMap<String, RegistryObjectType>();

        for (Class<?> c = object.getClass(); c.getSuperclass() != null; c = c
                .getSuperclass()) {

            // Look through all fields for associated RegistryObjects.
            for (Field f : c.getDeclaredFields()) {

                if (f.isAnnotationPresent(RegistryObjectAssociation.class)) {

                    String z = f.getName();
                    // Get the value of the attribute using a "getter"
                    Object v = ReflectionUtil.getter(object, z);

                    // Get the AssociationMapper to extract the subordinate
                    // registry objects.
                    RegistryObjectAssociation a = f
                            .getAnnotation(RegistryObjectAssociation.class);
                    RegistryObjectResolver mapper = ReflectionUtil
                            .newInstanceOfAssignableType(
                                    RegistryObjectResolver.class, a.value());
                    List<Object> objects = mapper.getRegistryObjects(v);

                    for (Object obj : objects) {
                        RegistryObjectType o = newRegistryObject(obj,
                                encoderStrategy);
                        ids.put(o.getId(), o);
                    }
                }
            }
        }

        return ids;
    }

    public static Map<String, AssociationInfo> getAssociations(Object object)
            throws ReflectionException, SerializationException {

        Map<String, AssociationInfo> ids = new HashMap<String, AssociationInfo>();

        for (Class<?> c = object.getClass(); c.getSuperclass() != null; c = c
                .getSuperclass()) {

            // Check Class for associations that are not "contains"
            if (c.isAnnotationPresent(RegistryObject.class)) {
                RegistryObject a = c.getAnnotation(RegistryObject.class);
                AssociationMapping[] mappings = a.associationMappings();
                if (mappings.length > 0) {
                    for (AssociationMapping m : mappings) {
                        String type = m.associationType();
                        String[] fields = m.keyFields();
                        Class<?> targetObject = m.targetObject();
                        Object o = ReflectionUtil.newInstance(targetObject);
                        for (String field : fields) {
                            Object f = ReflectionUtil.getter(object, field);
                            ReflectionUtil.setter(o, field, f);
                        }
                        ids.put(getRegistryObjectKey(o), new AssociationInfo(
                                type, m.required()));
                    }
                }
            }

            // Look through all fields for associated RegistryObjects.
            for (Field f : c.getDeclaredFields()) {

                if (f.isAnnotationPresent(RegistryObjectAssociation.class)) {

                    String z = f.getName();
                    // Get the value of the attribute using a "getter"
                    Object v = ReflectionUtil.getter(object, z);

                    if (v != null) {
                        // Get the AssociationMapper to extract the subordinate
                        // registry objects.
                        RegistryObjectAssociation a = f
                                .getAnnotation(RegistryObjectAssociation.class);
                        RegistryObjectResolver mapper = ReflectionUtil
                                .newInstanceOfAssignableType(
                                        RegistryObjectResolver.class, a.value());
                        String associationType = a.associationType();
                        List<Object> objects = mapper.getRegistryObjects(v);

                        for (Object obj : objects) {
                            ids.put(getRegistryObjectKey(obj),
                                    new AssociationInfo(associationType, true));
                        }
                    }
                }
            }
        }

        return ids;
    }

    /**
     * Create a new StringSlot with the given name and value.
     * 
     * @param name
     *            The name to use for the StringSlot.
     * 
     * @param value
     *            The value to set for the StringSlot.
     * 
     * @return A new StringSlot with the given name and value.
     */
    public static SlotType newStringSlot(String name, String value) {
        SlotType slot = new SlotType();
        StringValueType slotValue = new StringValueType();
        slot.setName(name);
        slotValue.setStringValue(value);
        slot.setSlotValue(slotValue);
        return slot;
    }

    /*
     * <RegistryObject xsi:type="ClassificationNodeType"
     * 
     * parent="urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject"
     * code="com.raytheon.uf.common.datadelivery.registry.DataSetMetaData"
     * 
     * lid=
     * "urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.DataSetMetaData"
     * id=
     * "urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.DataSetMetaData"
     * xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> </RegistryObject>
     */
    public static RegistryObjectType getObjectTypeNode(String objectType) {

        ClassificationNodeType node = new ClassificationNodeType();
        node.setId(objectType);
        node.setLid(objectType);
        node.setParent(RegistryObjectTypes.REGISTRY_OBJECT);
        // Add the namespace separator length to get the correct substring
        node.setCode(objectType.substring(RegistryObjectTypes.REGISTRY_OBJECT
                .length() + 1));

        return node;
    }

    /**
     * Create a new {@link SubmitObjectsRequest}.
     * 
     * @param asList
     *            the list of registry object types
     * @param mode
     *            the {@link Mode} to use
     * @return the request
     */
    public static SubmitObjectsRequest newSubmitObjects(
            List<RegistryObjectType> asList, Mode mode) {
        SubmitObjectsRequest request = new SubmitObjectsRequest();
        request.setCheckReferences(false);
        request.setMode(mode);
        RegistryObjectListType registryObjectList = new RegistryObjectListType();
        registryObjectList.getRegistryObject().addAll(asList);
        request.setRegistryObjectList(registryObjectList);
        return request;

    }

    /**
     * Return a <code>List</code> of <code>AssociationType</code> Objects that
     * can be used to make association in the Registry between the source Object
     * and the target object(s).
     * 
     * @param sourceObjectId
     *            The Id of the source object for the association.
     * 
     * @param dependentObjects
     *            A <code>Map</code> that uses the id of the target registry
     *            objects to add as the key for the <code>Map</code> to the
     *            association to make between the source object ant the target
     *            object.
     * 
     * @return A <code>List</code> of <code>AssociationType</code> Objects.
     */
    public static List<RegistryObjectType> makeAssociations(
            String sourceObjectId, Map<String, AssociationInfo> dependentObjects) {

        List<RegistryObjectType> associations = new ArrayList<RegistryObjectType>();
        for (String targetObjectId : dependentObjects.keySet()) {
            AssociationType at = new AssociationType();
            at.setObjectType(AssociationType.class.getName());
            at.setSourceObject(sourceObjectId);
            at.setTargetObject(targetObjectId);
            at.setType(dependentObjects.get(targetObjectId)
                    .getAssociationType());
            at.setId(RegistryUtil.generateRegistryObjectId());
            at.setLid(at.getId());
            associations.add(at);
        }
        return associations;
    }

    /**
     * Search the Class hierarchy for an Object for the
     * <code>@RegistryObject</code> annotation.
     * 
     * @param object
     *            An Object to inspect.
     * 
     * @return Whether or not the Class hierarchy contains a Class with the
     *         <code>@RegistryObject</code> annotation.
     */
    public static boolean isRegistryObject(Object object) {
        for (Class<?> c = object.getClass(); c.getSuperclass() != null; c = c
                .getSuperclass()) {
            if (c.isAnnotationPresent(RegistryObject.class)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Get a failed {@link RegistryQueryResponse} for the {@link Throwable}.
     * 
     * @param response
     *            the response to set to failed
     * @param throwable
     *            the throwable causing the failure
     * @return the response
     */
    public static <R extends RegistryResponse<S>, T extends Throwable, S> R getFailedResponse(
            R response, T throwable) {
        response.setErrors(Arrays.<Throwable> asList(throwable));
        response.setStatus(OperationStatus.FAILED);
        return response;
    }

    /**
     * Get a failed {@link RegistryQueryResponse} for the
     * {@link CommunicationException}.
     * 
     * @param response
     *            the response to set to failed
     * @param e
     *            the exception causing the failure
     * @return the response
     */
    public static <R extends RegistryResponse<S>, S> R getFailedResponse(
            R response, CommunicationException e) {
        String message = e.getMessage();
        String errorMessage = (message
                .indexOf(RegistryErrorMessage.DATABASE_ERROR_MESSAGE) != -1) ? RegistryErrorMessage.FAILED_TO_CONNECT_TO_DATABASE
                : message;
        return getFailedResponse(response, new RegistryException(errorMessage,
                e));
    }

    /**
     * Creates a new InternationalStringType from a given String
     * 
     * @param str
     *            The string from which to construct the InternationalStringType
     * @return The new InternationalStringType instance
     */
    public static InternationalStringType getInternationalString(String str) {
        return getInternationalString(str, Languages.EN_US);
    }

    /**
     * Creates a new InternationalStringType from a given String
     * 
     * @param str
     *            The string from which to construct the InternationalStringType
     * @param lan
     *            The language of the string
     * @return The new InternationalStringType instance
     */
    public static InternationalStringType getInternationalString(String str,
            String lang) {
        InternationalStringType intlString = new InternationalStringType();
        LocalizedStringType localString = new LocalizedStringType();
        localString.setLang(lang);
        localString.setValue(str);
        List<LocalizedStringType> localizedList = intlString
                .getLocalizedString();
        localizedList.add(localString);
        return intlString;
    }
}
