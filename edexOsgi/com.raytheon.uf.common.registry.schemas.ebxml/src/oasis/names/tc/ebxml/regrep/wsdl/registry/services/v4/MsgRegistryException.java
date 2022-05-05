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

package oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4;

import javax.xml.ws.WebFault;

import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlNamespaces;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;

/**
 * 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2012                     bphillip    Initial implementation
 * 10/17/2013    1682       bphillip    Added software history
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@WebFault(name = "RegistryException", targetNamespace = EbxmlNamespaces.RS_URI)
@DynamicSerializeTypeAdapter(factory = MsgRegistryExceptionTypeAdapter.class)
public class MsgRegistryException extends Exception {

    /**
	 * 
	 */
    private static final long serialVersionUID = -6291227840209631340L;

    /**
     * Java type that goes as soapenv:Fault detail element.
     * 
     */
    private final RegistryExceptionType faultInfo;

    /**
     * 
     * @param message
     * @param faultInfo
     */
    public MsgRegistryException(String message, RegistryExceptionType faultInfo) {
        super(message);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @param message
     * @param faultInfo
     * @param cause
     */
    public MsgRegistryException(String message,
            RegistryExceptionType faultInfo, Throwable cause) {
        super(message, cause);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @return returns fault bean:
     *         oasis.names.tc.ebxml_regrep.xsd.rs._4.RegistryExceptionType
     */
    public RegistryExceptionType getFaultInfo() {
        return faultInfo;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("REGISTRY EXCEPTION:\n");
        builder.append("\t Message: ").append(this.getMessage()).append("\n");
        builder.append(faultInfo.toString());
        return builder.toString();
    }

}
