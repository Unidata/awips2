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

import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;
import javax.xml.ws.WebEndpoint;
import javax.xml.ws.WebServiceClient;
import javax.xml.ws.WebServiceFeature;

import com.raytheon.uf.common.registry.EbxmlNamespaces;

/**
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
@WebServiceClient(name = "ValidatorSOAPService", targetNamespace = EbxmlNamespaces.RR_SERV_URI)
public class ValidatorSOAPService extends Service {

    public ValidatorSOAPService(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    /**
     * 
     * A template SOAP endpoint for ebXML RegRep Validator interface.
     * 
     * 
     * @return returns Validator
     */
    @WebEndpoint(name = "ValidatorPort")
    public Validator getValidatorPort() {
        return super.getPort(new QName(EbxmlNamespaces.RR_SERV_URI,
                "ValidatorPort"), Validator.class);
    }

    /**
     * 
     * A template SOAP endpoint for ebXML RegRep Validator interface.
     * 
     * 
     * @param features
     *            A list of {@link javax.xml.ws.WebServiceFeature} to configure
     *            on the proxy. Supported features not in the
     *            <code>features</code> parameter will have their default
     *            values.
     * @return returns Validator
     */
    @WebEndpoint(name = "ValidatorPort")
    public Validator getValidatorPort(WebServiceFeature... features) {
        return super.getPort(new QName(EbxmlNamespaces.RR_SERV_URI,
                "ValidatorPort"), Validator.class, features);
    }

}
