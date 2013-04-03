package com.raytheon.uf.common.registry;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.BuiltInTypeSupport;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Container Object used to aggregate status, errors and results from
 * interactions with the registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012            jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 20, 2012 0743       djohnson    Finish making registry type-safe.
 * Sep 14, 2012 1169       djohnson    Document errors field behavior.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@DynamicSerialize
public class RegistryResponse<T> {

    @DynamicSerializeElement
    private List<T> registryObjects = new ArrayList<T>();

    /**
     * NOTE: The deserialized object is NOT an instance of the original
     * throwable type. See {@link BuiltInTypeSupport.ThrowableSerializer} for
     * details.
     */
    @DynamicSerializeElement
    private List<Throwable> errors = new ArrayList<Throwable>();

    @DynamicSerializeElement
    private OperationStatus status;

    /**
     * Create a RegistryResponse.
     */
    public RegistryResponse() {
        this.status = OperationStatus.FAILED;
    }

    /**
     * A setter for the status attribute.
     * 
     * @param status
     *        The value to set the status attribute to.
     */
    public void setStatus(OperationStatus status) {
        this.status = status;
    }

    /**
     * A getter for the status attribute.
     * 
     * @return The value of the status attribute.
     */
    public OperationStatus getStatus() {
        return status;
    }

    /**
     * A setter for the registryObjects attribute.
     * 
     * @param status
     *        The value to set the registryObjects attribute to.
     */
    public void setRegistryObjects(List<T> registryObjects) {
        this.registryObjects = registryObjects;
    }

    /**
     * A getter for the registryObjects attribute.
     * 
     * @return The value of the registryObjects attribute.
     */
    public List<T> getRegistryObjects() {
        return registryObjects;
    }

    /**
     * A setter for the errors attribute.
     * 
     * @param status
     *        The value to set the errors attribute to.
     */
    public void setErrors(List<Throwable> errors) {
        this.errors = errors;
    }

    /**
     * A getter for the errors attribute.
     * 
     * @return The value of the errors attribute.
     */
    public List<Throwable> getErrors() {
        return errors;
    }

}
