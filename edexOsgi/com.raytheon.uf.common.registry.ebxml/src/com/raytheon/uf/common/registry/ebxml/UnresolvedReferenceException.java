package com.raytheon.uf.common.registry.ebxml;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;

/**
 * An Exception that captures reference errors encountered while attempting to
 * store a RegistyObject.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012 455        jspinks     Initial creation
 * Sep 20, 2012 1187       djohnson    Add DynamicSerializeTypeAdapter.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@DynamicSerializeTypeAdapter(factory = UnresolvedReferenceExceptionTypeAdapter.class)
public class UnresolvedReferenceException extends Exception {

    @Override
    public String getMessage() {
        // Add the ObjectIds to the detailMessage..
        return super.getMessage() + format(objectReferenceIds);
    }

    private String format(List<String> objectReferenceIds2) {
        StringBuilder sb = new StringBuilder();
        if (objectReferenceIds2.size() > 0) {
          for (String id : objectReferenceIds2) {
            sb.append(id + ", ");
          }
          // Remove the last comma and space.
          sb.delete(sb.length() -2, sb.length());
        }
        
        return "[" + sb.toString() + "]";
    }

    private static final long serialVersionUID = -5828493861683538769L;
    
    // A List of the ids that could not be resolved.
    private final List<String> objectReferenceIds = new ArrayList<String>();

    /**
     * Constructor that accepts a Message and a List of the unresolved registry
     * object ids.
     * 
     * @param message
     *            The message for this Exception.
     * 
     * @param objectReferenceIds
     *            A List of the unresolved registry object ids.
     */
    public UnresolvedReferenceException(String message, List<String> objectReferenceIds) {
        super(message);
        setObjectReferenceIds(objectReferenceIds);    
    }

    /**
     * Set the objectRefenceIds attribute.
     * 
     * @param objectReferenceIds
     *        The value to set for the objectReferenceIds attribute.
     */
    public void setObjectReferenceIds(List<String> objectReferenceIds) {
        this.objectReferenceIds.clear();
        this.objectReferenceIds.addAll(objectReferenceIds);
    }

    /**
     * Get the objectReferenceIds attribute.
     * 
     * @return A List of unresolved registry object id references.
     */
    public List<String> getObjectReferenceIds() {
        return objectReferenceIds;
    }

}
