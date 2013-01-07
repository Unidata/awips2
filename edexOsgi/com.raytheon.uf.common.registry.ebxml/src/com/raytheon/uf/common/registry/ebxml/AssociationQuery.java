package com.raytheon.uf.common.registry.ebxml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import com.raytheon.uf.common.registry.BaseQuery;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Query to search the Registry by association of source object to target
 * objects. Queries can be made against a combination of specific source or
 * target objects, the association between those objects or the object type of
 * the source or target objects. Additional you can return a <code>List</code>
 * of the referenced Objects are the associations themselves by setting the
 * isReturnObjects attribute appropriately.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2012 #455       jspinks     Initial creation
 * Jun 21, 2012 #736       djohnson    Add thrift serialization annotations.
 * Aug 27, 2012 0743       djohnson    Fixes to serializable attribute getter/setters.
 * Oct 05, 2012 1195       djohnson    Remove see javadoc tie back to RegistryManager.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AssociationQuery extends BaseQuery<Object> {

    @XmlAttribute
    @DynamicSerializeElement
    protected boolean returnObjects = true;

    @XmlAttribute
    @DynamicSerializeElement
    protected String associationType;
    
    @XmlAttribute
    @DynamicSerializeElement
    protected String sourceObjectId;

    @XmlAttribute
    @DynamicSerializeElement
    protected String sourceObjectType;
    
    @XmlAttribute
    @DynamicSerializeElement
    protected String targetObjectId;
    
    @XmlAttribute
    @DynamicSerializeElement
    protected String targetObjectType;
    
    /**
     * Queries of this type can return non-homogeneous results, so
     * the return type is Object.  Runtime inspection of the returned
     * Objects is required to determine their type.
     * 
     * @return The java typed Class Object to use to assign the 
     *         runtime type of the Objects retrieved from the registry.
     */
    @Override
    public Class<Object> getObjectType() {
        return Object.class;
    }

    /**
     * Querying the registry requires the use of a QueryRequest Object.
     * This base Object supports different types of queries.  This method 
     * provides the RegistryManager a means to determine the query type 
     * that should be used in conjunction with the slots provided by
     * the getSlots() method to produce the correct query to locate 
     * registry Objects. 
     *  
     * @return The constant "FindAssociatedObjects" to mark queries 
     *         generated using this Class as finding associated 
     *         objects. 
     */
    @Override
    public String getQueryType() {
        if (returnObjects) {
            return RegistryUtil.QUERY_TYPE_ASSOCIATED_OBJECTS;
        } else {
            return RegistryUtil.QUERY_TYPE_ASSOCIATIONS;
        }
    }

    /**
     * Querying the registry requires the use of a QueryRequest Object.
     * This Object queries the registry based on the slots add to the
     * query.  This method provides the slots necessary to execute a
     * FindAssociatedObjects query.
     *  
     * @return The slots to add to a QueryRequest to find the desired
     *         registry Objects. 
     */
    @Override
    public List<SlotType> getSlots() {
        List<SlotType> slots = new ArrayList<SlotType>();

        if (sourceObjectId != null) {
            slots.add(RegistryUtil.newStringSlot("sourceObjectId", sourceObjectId));    
        }    

        if (targetObjectId != null) {
            slots.add(RegistryUtil.newStringSlot("targetObjectId", targetObjectId));    
        }    

        if (sourceObjectType != null) {
            slots.add(RegistryUtil.newStringSlot("sourceObjectType", sourceObjectType));    
        }    

        if (targetObjectType != null) {
            slots.add(RegistryUtil.newStringSlot("targetObjectType", targetObjectType));    
        }    

        if (associationType != null) {
            slots.add(RegistryUtil.newStringSlot("associationType", associationType));    
        }    

        return slots;
    }

    /**
     * Retrieve the isReturnObjects attribute from this Query.  If true,
     * when this Query is submitted to the <code>RegistryManager</code>
     * the resulting <code>List</code> will contain the referenced Objects
     * stored in the Registry.  If false, a <code>List</code> of <code>AssociationType</code>
     * Objects will be returned. 
     * 
     * @return
     * 
     * @see AssociationType
     */
    public boolean isReturnObjects() {
        return returnObjects;
    }
    
    /**
     * Matches Associations whose type attribute references a ClassificationNode
     * where rim:ClassificationNode/@path matches specified value. 
     *
     * @param associationType
     *        The value of the associationType attribute to search for.
     */
    public void setAssociationType(String associationType) {
        this.associationType = associationType;
    }

    /**
     * Set the isReturnObjects attribute from this Query.
     * 
     * @return The value of the isReturnObjects attribute.
     * 
     * @see AssociationQuery#isReturnObjects()
     */
    public void setReturnObjects(boolean isReturnObjects) {
        this.returnObjects = isReturnObjects;
    }

    /**
     * A setter for the queryable attribute source object id equals a
     * String.  The String may contain the wildcard characters '%', to
     * match multiple characters, or '?' to wildcard a single character.
     * 
     * For the AssociationQuery to execute correctly, one of the attributes
     * sourceObjectId or targetObjectId MUST be set.
     *   
     * @param sourceObjectId
     *        The value of the sourceObjectId attribute to search for.
     */
    public void setSourceObjectId(String sourceObjectId) {
        this.sourceObjectId = sourceObjectId;
    }
    
    /**
     * Match associations whose sourceObject attribute references a 
     * RegistryObject whose objectType attribute matches the id of the 
     * ClassificationNode where rim:ClassificationNode/@path matches 
     * specified value  
     *   
     * @param sourceObjectId
     *        The value of the path attribute to search for.
     */
    public void setSourceObjectType(String sourceObjectType) {
        this.sourceObjectType = sourceObjectType;
    }

    /**
     * A setter for the queryable attribute target object id equals a
     * String.  The String may contain the wildcard characters '%', to
     * match multiple characters, or '?' to wildcard a single character.
     *   
     * For the AssociationQuery to execute correctly, one of the attributes
     * sourceObjectId or targetObjectId MUST be set.
     * 
     * @param targetObjectId
     *        The value of the targetObjectId attribute to search for.
     */
    public void setTargetObjectId(String targetObjectId) {
        this.targetObjectId = targetObjectId;
    }
    
    /**
     * Match associations whose targetObject attribute references a 
     * RegistryObject whose objectType attribute matches the id of the 
     * ClassificationNode where rim:ClassificationNode/@path matches 
     * specified value  
     *   
     * @param targetObjectType
     *        The value of the path attribute to search for.
     */
    public void setTargetObjectType(String targetObjectType) {
        this.targetObjectType = targetObjectType;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<Object> getResultType() {
        return getObjectType();
    }

    /**
     * @return the associationType
     */
    public String getAssociationType() {
        return associationType;
    }

    /**
     * @return the sourceObjectId
     */
    public String getSourceObjectId() {
        return sourceObjectId;
    }

    /**
     * @return the sourceObjectType
     */
    public String getSourceObjectType() {
        return sourceObjectType;
    }

    /**
     * @return the targetObjectId
     */
    public String getTargetObjectId() {
        return targetObjectId;
    }

    /**
     * @return the targetObjectType
     */
    public String getTargetObjectType() {
        return targetObjectType;
    }
}
