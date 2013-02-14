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

package oasis.names.tc.ebxml.regrep.xsd.lcm.v4;

import javax.xml.bind.annotation.XmlRegistry;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * This object contains factory methods for each Java content interface and Java
 * element interface generated in the oasis.names.tc.ebxml_regrep.xsd.lcm._4
 * package.
 * <p>
 * An ObjectFactory allows you to programatically construct new instances of the
 * Java representation for XML content. The Java representation of XML content
 * can consist of schema derived interfaces and classes representing the binding
 * of schema type definitions, element declarations and model groups. Factory
 * methods for each of these are provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory implements ISerializableObject {

    /**
     * Create a new ObjectFactory that can be used to create new instances of
     * schema derived classes for package:
     * oasis.names.tc.ebxml_regrep.xsd.lcm._4
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link UpdateActionType }
     * 
     */
    public UpdateActionType createUpdateActionType() {
        return new UpdateActionType();
    }

    /**
     * Create an instance of {@link UpdateObjectsRequest }
     * 
     */
    public UpdateObjectsRequest createUpdateObjectsRequest() {
        return new UpdateObjectsRequest();
    }

    /**
     * Create an instance of {@link RemoveObjectsRequest }
     * 
     */
    public RemoveObjectsRequest createRemoveObjectsRequest() {
        return new RemoveObjectsRequest();
    }

    /**
     * Create an instance of {@link SubmitObjectsRequest }
     * 
     */
    public SubmitObjectsRequest createSubmitObjectsRequest() {
        return new SubmitObjectsRequest();
    }

}
