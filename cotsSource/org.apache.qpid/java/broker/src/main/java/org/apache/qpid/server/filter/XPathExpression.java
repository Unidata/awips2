/**
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.qpid.server.filter;
//
// Based on like named file from r450141 of the Apache ActiveMQ project <http://www.activemq.org/site/home.html>
//

import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.server.queue.Filterable;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

/**
 * Used to evaluate an XPath Expression in a JMS selector.
 */
public final class XPathExpression implements BooleanExpression {

    private static final Logger log = Logger.getLogger(XPathExpression.class);
    private static final String EVALUATOR_SYSTEM_PROPERTY = "org.apache.qpid.server.filter.XPathEvaluatorClassName";
    private static final String DEFAULT_EVALUATOR_CLASS_NAME=XalanXPathEvaluator.class.getName();

    private static final Constructor EVALUATOR_CONSTRUCTOR;

    static {
        String cn = System.getProperty(EVALUATOR_SYSTEM_PROPERTY, DEFAULT_EVALUATOR_CLASS_NAME);
        Constructor m = null;
        try {
            try {
                m = getXPathEvaluatorConstructor(cn);
            } catch (Throwable e) {
                log.warn("Invalid "+XPathEvaluator.class.getName()+" implementation: "+cn+", reason: "+e,e);
                cn = DEFAULT_EVALUATOR_CLASS_NAME;
                try {
                    m = getXPathEvaluatorConstructor(cn);
                } catch (Throwable e2) {
                    log.error("Default XPath evaluator could not be loaded",e);
                }
            }
        } finally {
            EVALUATOR_CONSTRUCTOR = m;
        }
    }

    private static Constructor getXPathEvaluatorConstructor(String cn) throws ClassNotFoundException, SecurityException, NoSuchMethodException {
        Class c = XPathExpression.class.getClassLoader().loadClass(cn);
        if( !XPathEvaluator.class.isAssignableFrom(c) ) {
            throw new ClassCastException(""+c+" is not an instance of "+XPathEvaluator.class);
        }
        return c.getConstructor(new Class[]{String.class});
    }

    private final String xpath;
    private final XPathEvaluator evaluator;

    static public interface XPathEvaluator {
        public boolean evaluate(Filterable message);
    }

    XPathExpression(String xpath) {
        this.xpath = xpath;
        this.evaluator = createEvaluator(xpath);
    }

    private XPathEvaluator createEvaluator(String xpath2) {
        try {
            return (XPathEvaluator)EVALUATOR_CONSTRUCTOR.newInstance(new Object[]{xpath});
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause();
            if( cause instanceof RuntimeException ) {
                throw (RuntimeException)cause;
            }
            throw new RuntimeException("Invalid XPath Expression: "+xpath+" reason: "+e.getMessage(), e);
        } catch (Throwable e) {
            throw new RuntimeException("Invalid XPath Expression: "+xpath+" reason: "+e.getMessage(), e);
        }
    }

    public Object evaluate(Filterable message)  {
//        try {
//FIXME this is flow to disk work
//            if( message.isDropped() )
//                return null;
            return evaluator.evaluate(message) ? Boolean.TRUE : Boolean.FALSE;
//        } catch (IOException e) {
//
//            JMSException exception = new JMSException(e.getMessage());
//            exception.initCause(e);
//            throw exception;
//
//        }

    }

    public String toString() {
        return "XPATH "+ConstantExpression.encodeString(xpath);
    }

    /**
     * @param message
     * @return true if the expression evaluates to Boolean.TRUE.
     * @throws AMQException
     */
    public boolean matches(Filterable message)
    {
        Object object = evaluate(message);
        return object!=null && object==Boolean.TRUE;
    }

}
