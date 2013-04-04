package com.raytheon.uf.edex.registry.ebxml.constants;

/**
 * This class holds the canonical ClassificationNodes are defined for the
 * RegistryResponseStatus ClassificationScheme. These constants are reported in
 * the status component of a RegistryResponseType returned to a client to
 * indicate the status of their request
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/9/2012     184         bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryResponseStatus {

    /**
     * This status specifies that the request encountered a failure. This value
     * MUST never be returned since a server MUST indicate failure conditions by
     * returning an appropriate fault message.
     */
    public static final String FAILURE = "urn:oasis:names:tc:ebxml-regrep:ResponseStatusType:Failure";

    /**
     * This status specifies that the request was partially successful. Certain
     * requests such as federated queries allow this status to be returned
     */
    public static final String PARTIAL_SUCCESS = "urn:oasis:names:tc:ebxml-regrep:ResponseStatusType:PartialSuccess";

    /**
     * This status specifies that the request was successful
     */
    public static final String SUCCESS = "urn:oasis:names:tc:ebxml-regrep:ResponseStatusType:Success";

    /**
     * This status specifies that the response is not yet available. This may be
     * the case if this RegistryResponseType represents an immediate response to
     * an asynchronous request where the actual response is not yet available.
     */
    public static final String UNAVAILABLE = "urn:oasis:names:tc:ebxml-regrep:ResponseStatusType:Unavailable";
}
