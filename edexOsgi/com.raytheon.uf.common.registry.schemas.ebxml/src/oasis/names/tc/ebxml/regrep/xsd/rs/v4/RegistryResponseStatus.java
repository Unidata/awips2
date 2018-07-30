package oasis.names.tc.ebxml.regrep.xsd.rs.v4;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

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
 * 4/9/2013     1802       bphillip    Moved into EBXML common plugin
 * Apr 24, 2013 1910       djohnson    RegistryResponseStatus is now an enum.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlEnum
public enum RegistryResponseStatus {

    /**
     * This status specifies that the request encountered a failure. This value
     * MUST never be returned since a server MUST indicate failure conditions by
     * returning an appropriate fault message.
     */
    @XmlEnumValue(RegistryResponseStatus.FAILURE_STRING)
    FAILURE,

    /**
     * This status specifies that the request was partially successful. Certain
     * requests such as federated queries allow this status to be returned
     */
    @XmlEnumValue(RegistryResponseStatus.PARTIAL_SUCCESS_STRING)
    PARTIAL_SUCCESS,

    /**
     * This status specifies that the request was successful
     */
    @XmlEnumValue(RegistryResponseStatus.SUCCESS_STRING)
    SUCCESS,

    /**
     * This status specifies that the response is not yet available. This may be
     * the case if this RegistryResponseType represents an immediate response to
     * an asynchronous request where the actual response is not yet available.
     */
    @XmlEnumValue(RegistryResponseStatus.UNAVAILABLE_STRING)
    UNAVAILABLE;

    private static final String FAILURE_STRING = "urn:oasis:names:tc:ebxml-regrep:ResponseStatusType:Failure";

    private static final String PARTIAL_SUCCESS_STRING = "urn:oasis:names:tc:ebxml-regrep:ResponseStatusType:PartialSuccess";

    private static final String SUCCESS_STRING = "urn:oasis:names:tc:ebxml-regrep:ResponseStatusType:Success";

    private static final String UNAVAILABLE_STRING = "urn:oasis:names:tc:ebxml-regrep:ResponseStatusType:Unavailable";
}
