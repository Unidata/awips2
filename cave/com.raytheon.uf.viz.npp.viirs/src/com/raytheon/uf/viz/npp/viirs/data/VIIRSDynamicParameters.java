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
package com.raytheon.uf.viz.npp.viirs.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod.MethodType;

/**
 * Class for resolving viirs parameters dynamically. Parses parameter as
 * expression which may include basic math (+,-,*,/) or simple function calls
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDynamicParameters {

    private static final String FUNCTION_PREFIX = "viirs.";

    private static enum Operator {
        ADD("+", 1, FUNCTION_PREFIX + "Math.add"), SUBTRACT("-", 1,
                FUNCTION_PREFIX + "Math.subtract"), MULTIPLY("*", 2,
                FUNCTION_PREFIX + "Math.multiply"), DIVIDE("/", 2,
                FUNCTION_PREFIX + "Math.divide");

        private String token;

        private int priority;

        private String function;

        private Operator(String token, int priority, String function) {
            this.token = token;
            this.priority = priority;
            this.function = function;
        }
    }

    private static class Node {

        protected String token;

        protected Node(String token) {
            this.token = token;
        }

        protected String getNodeName() {
            return token;
        }

    }

    private static class OperatorNode extends FunctionNode {

        protected Operator operator;

        protected Node leftChild;

        protected Node rightChild;

        protected OperatorNode(String token, Operator operator) {
            super(token);
            this.function = operator.function;
            this.operator = operator;
        }

        @Override
        public int getPriority() {
            return operator.priority;
        }

        @Override
        public void setChildren(Stack<Node> stack) {
            this.rightChild = stack.pop();
            this.leftChild = stack.pop();
        }

        @Override
        public Collection<Node> getChildren() {
            return Arrays.asList(leftChild, rightChild);
        }

        @Override
        protected String getNodeName() {
            return leftChild.getNodeName() + operator.token
                    + rightChild.getNodeName();
        }

    }

    private static class FunctionNode extends Node {

        private static final String FUNCTION_FORMAT = "%s(%s)";

        protected String function;

        protected Node parameter;

        protected FunctionNode(String token) {
            super(token.endsWith(OPEN_PAREN) ? token.substring(0,
                    token.length() - 1) : token);
            this.function = FUNCTION_PREFIX + this.token;
        }

        public int getPriority() {
            return Integer.MAX_VALUE;
        }

        public void setChildren(Stack<Node> stack) {
            this.parameter = stack.pop();
        }

        protected String getFunctionName() {
            return function;
        }

        @Override
        protected String getNodeName() {
            return String.format(FUNCTION_FORMAT, token,
                    parameter.getNodeName());
        }

        public Collection<Node> getChildren() {
            return Arrays.asList(parameter);
        }

    }

    private static final Map<String, Operator> operatorMap = new HashMap<String, Operator>();

    static {
        for (Operator op : Operator.values()) {
            operatorMap.put(op.token, op);
        }
    }

    private static final String OPEN_PAREN = "(";

    private static final String CLOSE_PAREN = ")";

    private static final String WAVELENGTH_PARAMETER_GROUP = "((\\d+\\.\\d+)([a-zA-Z]+))";

    private static final Pattern OPERATOR_PATTERN = Pattern
            .compile("([ ]*[\\+/\\-\\*\\(\\)][ ]*)");

    /** Pattern for a function, matches 'functionName(' group(1) is functionName */
    private static final Pattern FUNCTION_PATTERN = Pattern
            .compile("(([a-zA-Z])+\\()");

    /** Tokenizes a parameter expression for parsing */
    private static final Pattern PARAMETER_EXPRESSION_PATTERN = Pattern
            .compile(FUNCTION_PATTERN.pattern() + "|([a-zA-Z0-9\\.]+)|"
                    + OPERATOR_PATTERN.pattern());

    private static final Pattern WAVELENGTH_PARAMETER_PATTERN = Pattern
            .compile("^" + WAVELENGTH_PARAMETER_GROUP + "$");

    private static final String WAVELENGTH_PARAMTER_FORMAT = "%s%s";

    /**
     * Creates derived parameter descriptions for the viirs dynamic parameter
     * 
     * @param parameter
     * @param descriptions
     */
    public static void createParameterDefinitions(String parameter,
            VIIRSDataInventory inventory, List<String> parameterSources) {
        Node astRoot = createExpressionTree(parameter);
        createParameterDefinitions(astRoot, inventory, parameterSources);
    }

    private static void createParameterDefinitions(Node astNode,
            VIIRSDataInventory inventory, List<String> parameterSources) {
        String nodeName = astNode.getNodeName();
        String nodeSource = inventory.getParameterSource(nodeName);
        if (astNode instanceof FunctionNode) {
            FunctionNode node = (FunctionNode) astNode;

            // Process children nodes
            Collection<Node> children = node.getChildren();
            for (Node child : children) {
                // Ensure we have definition for child
                createParameterDefinitions(child, inventory, parameterSources);
            }

            DerivParamDesc desc = inventory.getParameterDescription(nodeName);
            if (desc == null) {
                // Create description object for function
                desc = new DerivParamDesc();
                desc.setAbbreviation(nodeName);
                desc.setName(nodeName);

                // Create method object for function desc
                DerivParamMethod method = new DerivParamMethod();
                method.setName(node.getFunctionName());
                method.setMethodType(MethodType.PYTHON);
                desc.addMethod(method);

                // Process children nodes
                for (Node child : children) {
                    // Create field for child in our definition method
                    DerivParamField field = new DerivParamField();
                    String fieldName = child.getNodeName();
                    field.setParam(fieldName);
                    field.setValidSource(inventory
                            .getParameterSource(fieldName));
                    method.addField(field);
                }

                inventory.addParameterDescription(nodeName, desc);
            }
        }
        if (nodeSource != null
                && parameterSources.contains(nodeSource) == false) {
            parameterSources.add(nodeSource);
        }
    }

    /**
     * Takes a parameter/wavelength combo and creates a single parameter with
     * wavelength embedded in it
     * 
     * @param wavelength
     * @param parameter
     * @return
     */
    public static String createParameter(Number wavelength, String parameter) {
        String[] parts = parseExpression(parameter);
        StringBuffer buffer = new StringBuffer();
        for (String part : parts) {
            String tmp = part.trim();
            if (!OPERATOR_PATTERN.matcher(tmp).matches()
                    && !FUNCTION_PATTERN.matcher(tmp).matches()) {
                // Not operator, not function
                Matcher paramCheck = WAVELENGTH_PARAMETER_PATTERN.matcher(part);
                if (paramCheck.find() == false) {
                    // Does't have a wavelength on parameter, add it
                    part = String.format(WAVELENGTH_PARAMTER_FORMAT,
                            wavelength, tmp);
                }
            }
            buffer.append(part);
        }

        return buffer.toString();
    }

    /**
     * Creates an abstract syntax tree from the parameter expression.
     * 
     * @param expression
     * @return
     */
    private static Node createExpressionTree(String expression) {
        Stack<FunctionNode> functionStack = new Stack<FunctionNode>();
        Stack<Node> treeStack = new Stack<Node>();
        String[] parts = parseExpression(expression);
        for (int i = 0; i < parts.length; ++i) {
            String token = parts[i].trim();
            if (isGroupStart(token)) {
                if (token.length() > 1) {
                    // Start of function
                    functionStack.push(new FunctionNode(token));
                } else {
                    // Start of paren group
                    functionStack.push(null);
                }
            } else if (isGroupEnd(token)) {
                // End of group/function, reduce until we find matching paren
                while (functionStack.peek() != null) {
                    boolean endOfFunc = (functionStack.peek() instanceof OperatorNode) == false;
                    reduce(functionStack, treeStack);
                    if (endOfFunc) {
                        break;
                    }
                }
                if (functionStack.isEmpty() == false
                        && functionStack.peek() == null) {
                    // Pop matching paren, didn't end with function start
                    functionStack.pop();
                }
            } else if (OPERATOR_PATTERN.matcher(token).matches()) {
                // Operator (+-/*), get operator
                Operator operator = operatorMap.get(token);
                OperatorNode op = new OperatorNode(token, operator);
                while (functionStack.isEmpty() == false) {
                    FunctionNode next = functionStack.peek();
                    if (next instanceof OperatorNode
                            && op.getPriority() < next.getPriority()) {
                        reduce(functionStack, treeStack);
                    } else {
                        // Done reducing
                        break;
                    }
                }
                functionStack.push(op);
            } else if (WAVELENGTH_PARAMETER_PATTERN.matcher(token).matches()) {
                treeStack.push(new Node(token));
            } else {
                System.err.println("Error parsing token: " + token);
            }
        }
        while (functionStack.isEmpty() == false) {
            reduce(functionStack, treeStack);
        }
        return treeStack.peek();
    }

    private static boolean isGroupStart(String token) {
        return token.endsWith(OPEN_PAREN);
    }

    private static boolean isGroupEnd(String token) {
        return token.equals(CLOSE_PAREN);
    }

    /**
     * Removes from the operator stack, sets left/right then adds to tree stack
     * 
     * @param functionStack
     * @param treeStack
     */
    private static void reduce(Stack<FunctionNode> functionStack,
            Stack<Node> treeStack) {
        FunctionNode N = functionStack.pop();
        N.setChildren(treeStack);
        treeStack.push(N);
    }

    /**
     * Splits an expression by function/parameter/math operators
     * 
     * @param expression
     * @return
     */
    private static String[] parseExpression(String expression) {
        List<String> parts = new ArrayList<String>();
        Matcher math = PARAMETER_EXPRESSION_PATTERN.matcher(expression);
        while (math.find()) {
            parts.add(math.group());
        }
        return parts.toArray(new String[parts.size()]);
    }

}
