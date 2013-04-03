package com.raytheon.uf.edex.registry.acp.xacml.engine.function;

import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

public class XACMLTest {

    private static final String TEMPLATE = "<bean id=\"[ID]\" class=\"[CLASS]\" />";

    private static final String REF_TEMPLATE = "<ref bean=\"[ID]\" />";

    private static final String PACKAGE_INFO = "package-info";

    /**
     * @param args
     */
    public static void main(String[] args) {
        StringBuffer buffer = new StringBuffer();
        try {
            // buffer.append(generateLines(XACMLFunctionEvaluator.class,
            // "XACML Functions"));
            // buffer.append(generateLines(XACMLExpressionEvaluator.class,
            // "XACML Expressions"));
            // buffer.append(generateLines(CombinerAlgorithmEvaluator.class,
            // "XACML Policy/Rule Combining Algorithms"));
            buffer.append(generateLines(ElementEvaluator.class,
                    "XACML Policy Elements"));
        } catch (Exception e) {
            e.printStackTrace();
        }
        // System.out.println(buffer);
    }

    private static StringBuffer generateLines(Class<?> clazzName, String label)
            throws Exception {
        Class<?>[] classes = EbxmlObjectUtil.getClasses(clazzName.getPackage()
                .getName() + ".impl");
        StringBuffer buf = new StringBuffer();
        buf.append("<!-- Classes for " + label + " -->\n\n");

        for (Class<?> clazz : classes) {
            if (!clazz.getName().contains(PACKAGE_INFO)) {
                Object algo = clazz.newInstance();
                String id = algo.getClass().getSimpleName();
                String className = algo.getClass().toString()
                        .replace("class ", "");
                String line = TEMPLATE.replace("[ID]", id).replace("[CLASS]",
                        className);
                buf.append(line).append("\n");
                System.out.println(REF_TEMPLATE.replace("[ID]", id));
            }
        }
        buf.append("\n<!-- End of Classes for " + label + " -->\n");
        return buf;
    }
}
