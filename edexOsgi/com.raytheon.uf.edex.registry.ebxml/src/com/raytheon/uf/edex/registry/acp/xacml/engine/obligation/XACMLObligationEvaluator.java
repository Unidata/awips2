package com.raytheon.uf.edex.registry.acp.xacml.engine.obligation;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.policy.AttributeAssignmentType;
import org.opensaml.xacml.policy.ObligationType;

import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpressionEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

public class XACMLObligationEvaluator {

    private static Map<String, XACMLObligation> obligationMap = new HashMap<String, XACMLObligation>();

    private static XACMLObligationEvaluator instance = new XACMLObligationEvaluator();

    static {
        try {
            Class<?>[] classes = EbxmlObjectUtil
                    .getClasses(XACMLObligationEvaluator.class.getPackage()
                            .getName() + ".impl");
            for (Class<?> clazz : classes) {
                XACMLObligation obligation = (XACMLObligation) clazz
                        .newInstance();
                obligationMap.put(obligation.getObligationId(), obligation);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static XACMLObligationEvaluator getInstance() {
        return instance;
    }

    private XACMLObligationEvaluator() {

    }

    public void addObligation(XACMLObligation obligation) {
        obligationMap.put(obligation.getObligationId(), obligation);
    }

    public Object evaluate(ObligationType obligation, RequestType request)
            throws XACMLException {
        String obligationId = obligation.getObligationId();
        XACMLObligation obligationImpl = obligationMap.get(obligationId);
        if (obligationImpl == null) {
            throw new UnsupportedOperationException(
                    "No implementation available to evaluation obligation type ["
                            + obligationId + "]");
        }

        List<AttributeAssignmentType> attrAssignments = obligation
                .getAttributeAssignments();
        Object[] obligationArgs = new Object[attrAssignments.size()];
        for (int i = 0; i < attrAssignments.size(); i++) {
            obligationArgs[i] = XACMLExpressionEvaluator.getInstance()
                    .evaluate(attrAssignments.get(i), request);
        }
        return obligationImpl.evaluate(obligationArgs);
    }
}
