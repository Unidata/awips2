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
package com.raytheon.uf.edex.ebxml.test;

import java.util.List;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.edex.ebxml.util.EbxmlJaxbManager;
import com.raytheon.uf.edex.ebxml.util.EbxmlUtil;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateActionType;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AnyValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.InternationalStringType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.InternationalStringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.LocalizedStringType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringQueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class UpdateMarshaller {

    /**
     * @param args
     *            none
     */
    public static void main(String[] args) {
        oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory lcmFactory = new oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory();
        oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory rimFactory = new oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory();
        oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory queryFactory = new oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory();

        UpdateObjectsRequest request = lcmFactory.createUpdateObjectsRequest();
        request.setId("0");
        request.setComment("Test SubmitObjects");
        request.setMode(Mode.CREATE_OR_REPLACE);
        
        
        ObjectRefListType objRefList = new ObjectRefListType();
        ObjectRefType ref = new ObjectRefType();
        ref.setId("0");
        objRefList.getObjectRef().add(ref);
        request.setObjectRefList(objRefList);
        

        UpdateActionType updateAction = new UpdateActionType();
        updateAction.setMode("UPDATE");
        InternationalStringValueType val = new InternationalStringValueType();
        InternationalStringType t = new InternationalStringType();
        LocalizedStringType lst = new LocalizedStringType();
        lst.setLang("en-US");
        lst.setValue("New Description");
        t.getLocalizedString().add(lst);
        val.setValue(t);
        updateAction.setValueHolder(val);
        StringQueryExpressionType queryType = rimFactory
                .createStringQueryExpressionType();
        queryType.setQueryLanguage("XPath");
        queryType.setValue("./rim:Name");
        updateAction.setSelector(queryType);
        request.getUpdateAction().add(updateAction);

        EbxmlJaxbManager manager = EbxmlJaxbManager.getInstance();
        try {
            // Marshal it
            String requestStr = manager.marshal(request);
            // Print it
            System.out.println(requestStr);
        } catch (JAXBException e) {
            // Or, print error
            e.printStackTrace();
        }
    }
}
