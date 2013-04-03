package com.raytheon.uf.common.registry.ebxml;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.ReflectionUtil;

/**
 * Query to search the Registry by Id.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Sep 30, 2012 1187       djohnson    {@link Class} variables are not serializable by default.
 * Nov 05, 2012 1306       djohnson    Remove dynamic serialize field level adapters.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class IdQuery<T> extends AdhocRegistryQuery<T> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(IdQuery.class);

    private static final String OBJECT_ID_QUERY = "from RegistryObjectType as obj where obj.id in (%1$s)";
    
    @XmlAttribute
    @DynamicSerializeElement
    protected StringAttribute idAttribute;

    @XmlAttribute
    @DynamicSerializeElement
    private String returnTypeString;

    protected transient Class<T> returnType;

    /**
     * Default constructor, added only to comply with dynamic serialization.
     * 
     * @deprecated Added only to comply with dynamic serialization
     */
    @Deprecated
    public IdQuery() {
    }

    /**
     * Create a new IdQuery that will assign the runtime type of the Object
     * returned to the provided returnType Class. For example, if you have a
     * registry Object id, and you know that the id belongs to an Object of
     * Class "my.example.Foo", then create an IdQuery using:
     * 
     * <pre>
     * IdQuery myQuery = new IdQuery(my.example.Foo.class);
     * </pre>
     * 
     * @param class1
     *            The typed Class to return results from this Query as.
     */
    public IdQuery(Class<T> class1) {
        this.returnType = class1;
        this.returnTypeString = returnType.getName();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<T> getObjectType() {
        return returnType;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<T> getResultType() {
        return getReturnType();
    }

    /**
     * A setter for the queryable attribute id equals a single String value.
     * Using this setter will search for a single registry object.
     *
     * @param collectionName
     *        The value of the collectionName attribute to search for.
     */
    public void setID(String id) {
        idAttribute = new StringAttribute(id);
    }


    /**
     * A setter for the queryable attribute id equals a List of String values.
     * Using this setter will search for a multiple registry objects.
     *   
     * All of the Id's provided should be of the same type of Object.
     *
     * @param collectionName
     *        The value of the collectionName attribute to search for.
     */
    public void setIDs(List<String> ids) {
        idAttribute = new StringAttribute(ids);
    }

    /**
     * @return the returnType
     */
    @SuppressWarnings("deprecation")
    public Class<T> getReturnType() {
        if (returnType == null) {
            if (returnTypeString != null) {
                setReturnTypeString(returnTypeString);
            } else {
                statusHandler
                        .error("The return type string is not set, and return type is null."
                                + "  Unable to restore the return type class!");
            }
        }
        return returnType;
    }

    /**
     * @return the returnTypeString
     */
    public String getReturnTypeString() {
        return returnTypeString;
    }

    /**
     * Added to comply with dynamic serialization.
     * 
     * @param returnType
     *            the returnType to set
     * @deprecated Added only to comply with dynamic serialization
     */
    @Deprecated
    @SuppressWarnings("unchecked")
    public void setReturnTypeString(String string) {
        this.returnTypeString = string;
        this.returnType = (Class<T>) ReflectionUtil.forName(string);
    }

    /**
     * Added to comply with dynamic serialization.
     * 
     * @deprecated Added only to comply with dynamic serialization
     * 
     * @return the idAttribute
     */
    @Deprecated
    public StringAttribute getIdAttribute() {
        return idAttribute;
    }

    /**
     * Added to comply with dynamic serialization. Please use
     * {@link #setID(String)} or {@link #setIDs(List)} instead.
     * 
     * @param idAttribute
     *            the idAttribute to set
     * @deprecated Added only to comply with dynamic serialization
     */
    @Deprecated
    public void setIdAttribute(StringAttribute idAttribute) {
        this.idAttribute = idAttribute;
    }

    /**
     * Create the HQL String used to query the registry.
     * 
     * @return The HQL query.
     */
    @Override
    protected String getHQL() {
        
        // Build the HQL query..
        return String.format(OBJECT_ID_QUERY, this.idAttribute.getQueryValue());
    }
}
