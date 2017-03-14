package com.raytheon.uf.common.registry;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Enumeration for an operation status.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2012 736        djohnson    Moved from RegistryResponse.
 * 
 * </pre>
 * 
 * @author djohnson
 */
@DynamicSerialize
public enum OperationStatus {
    SUCCESS, PARTIAL_SUCCESS, FAILED;
}
