/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.server.management;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanConstructorInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.NotCompliantMBeanException;

import org.apache.qpid.management.common.mbeans.annotations.MBeanAttribute;
import org.apache.qpid.management.common.mbeans.annotations.MBeanConstructor;
import org.apache.qpid.management.common.mbeans.annotations.MBeanDescription;
import org.apache.qpid.management.common.mbeans.annotations.MBeanOperation;
import org.apache.qpid.management.common.mbeans.annotations.MBeanOperationParameter;

/**
 * This class is a utility class to introspect the MBean class and the management
 * interface class for various purposes.
 * @author  Bhupendra Bhardwaj
 * @version 0.1
 */
class MBeanIntrospector {

    private static final String _defaultAttributeDescription = "Management attribute";
    private static final String _defaultOerationDescription = "Management operation";
    private static final String _defaultConstructorDescription = "MBean constructor";
    private static final String _defaultMbeanDescription = "Management interface of the MBean";

    /**
     * Introspects the management interface class for MBean attributes.
     * @param interfaceClass
     * @return MBeanAttributeInfo[]
     * @throws NotCompliantMBeanException
     */
    static MBeanAttributeInfo[] getMBeanAttributesInfo(Class interfaceClass)
        throws NotCompliantMBeanException
    {
        List<MBeanAttributeInfo> attributesList = new ArrayList<MBeanAttributeInfo>();

        /**
         * Using reflection, all methods of the managemetn interface will be analysed,
         * and MBeanInfo will be created.
         */
        for (Method method : interfaceClass.getMethods())
        {
            String name = method.getName();
            Class<?>  resultType = method.getReturnType();
            MBeanAttributeInfo attributeInfo = null;

            if (isAttributeGetterMethod(method))
            {
                String desc = getAttributeDescription(method);
                attributeInfo = new MBeanAttributeInfo(name.substring(3),
                                                resultType.getName(),
                                                desc,
                                                true,
                                                false,
                                                false);
                int index = getIndexIfAlreadyExists(attributeInfo, attributesList);
                if (index == -1)
                {
                    attributesList.add(attributeInfo);
                }
                else
                {
                    attributeInfo = new MBeanAttributeInfo(name.substring(3),
                                                resultType.getName(),
                                                desc,
                                                true,
                                                true,
                                                false);
                    attributesList.set(index, attributeInfo);
                }
            }
            else if (isAttributeSetterMethod(method))
            {
                String desc = getAttributeDescription(method);
                attributeInfo = new MBeanAttributeInfo(name.substring(3),
                                                method.getParameterTypes()[0].getName(),
                                                desc,
                                                false,
                                                true,
                                                false);
                int index = getIndexIfAlreadyExists(attributeInfo, attributesList);
                if (index == -1)
                {
                    attributesList.add(attributeInfo);
                }
                else
                {
                    attributeInfo = new MBeanAttributeInfo(name.substring(3),
                                                method.getParameterTypes()[0].getName(),
                                                desc,
                                                true,
                                                true,
                                                false);
                    attributesList.set(index, attributeInfo);
                }
            }
            else if (isAttributeBoolean(method))
            {
                attributeInfo = new MBeanAttributeInfo(name.substring(2),
                                                resultType.getName(),
                                                getAttributeDescription(method),
                                                true,
                                                false,
                                                true);
                attributesList.add(attributeInfo);
            }
        }

        return attributesList.toArray(new MBeanAttributeInfo[0]);
    }

    /**
     * Introspects the management interface class for management operations.
     * @param interfaceClass
     * @return MBeanOperationInfo[]
     */
    static MBeanOperationInfo[] getMBeanOperationsInfo(Class interfaceClass)
    {
        List<MBeanOperationInfo> operationsList = new ArrayList<MBeanOperationInfo>();

        for (Method method : interfaceClass.getMethods())
        {
            if (!isAttributeGetterMethod(method) &&
                !isAttributeSetterMethod(method) &&
                !isAttributeBoolean(method))
            {
                operationsList.add(getOperationInfo(method));
            }
        }

        return operationsList.toArray(new MBeanOperationInfo[0]);
    }

    /**
     * Checks if the method is an attribute getter method.
     * @param method
     * @return true if the method is an attribute getter method.
     */
    private static boolean isAttributeGetterMethod(Method method)
    {
        if (!(method.getName().equals("get")) &&
            method.getName().startsWith("get") &&
            method.getParameterTypes().length == 0 &&
            !method.getReturnType().equals(void.class))
        {
            return true;
        }

        return false;
    }

    /**
     * Checks if the method is an attribute setter method.
     * @param method
     * @return true if the method is an attribute setter method.
     */
    private static boolean isAttributeSetterMethod(Method method)
    {
        if (!(method.getName().equals("set")) &&
            method.getName().startsWith("set") &&
            method.getParameterTypes().length == 1 &&
            method.getReturnType().equals(void.class))
        {
            return true;
        }

        return false;
    }

    /**
     * Checks if the attribute is a boolean and the method is a isX kind og method.
     * @param method
     * @return true if the method is an attribute isX type of method
     */
    private static boolean isAttributeBoolean(Method method)
    {
        if (!(method.getName().equals("is")) &&
            method.getName().startsWith("is") &&
            method.getParameterTypes().length == 0 &&
            method.getReturnType().equals(boolean.class))
        {
            return true;
        }

        return false;
    }

    /**
     * Helper method to retrieve the attribute index from the list of attributes.
     * @param attribute
     * @param list
     * @return attribute index no. -1 if attribtue doesn't exist
     * @throws NotCompliantMBeanException
     */
    private static int getIndexIfAlreadyExists(MBeanAttributeInfo attribute,
                                                    List<MBeanAttributeInfo> list)
        throws NotCompliantMBeanException
    {
        String exceptionMsg = "Conflicting attribute methods for attribute " + attribute.getName();

        for (MBeanAttributeInfo memberAttribute : list)
        {
            if (attribute.getName().equals(memberAttribute.getName()))
            {
                if (!attribute.getType().equals(memberAttribute.getType()))
                {
                    throw new NotCompliantMBeanException(exceptionMsg);
                }
                if (attribute.isReadable() && memberAttribute.isReadable())
                {
                    if (attribute.isIs() != memberAttribute.isIs())
                    {
                        throw new NotCompliantMBeanException(exceptionMsg);
                    }
                }

                return list.indexOf(memberAttribute);
            }
        }

        return -1;
    }

    /**
     * Retrieves the attribute description from annotation
     * @param attributeMethod
     * @return attribute description
     */
    private static String getAttributeDescription(Method attributeMethod)
    {
        MBeanAttribute anno = attributeMethod.getAnnotation(MBeanAttribute.class);
        if (anno != null)
        {
            return anno.description();
        }
        return _defaultAttributeDescription;
    }

    /**
     * Introspects the method to retrieve the operation information.
     * @param operation
     * @return MBeanOperationInfo
     */
    private static MBeanOperationInfo getOperationInfo(Method operation)
    {
        MBeanOperationInfo operationInfo = null;
        Class<?> returnType = operation.getReturnType();

        MBeanParameterInfo[] paramsInfo = getParametersInfo(operation.getParameterAnnotations(),
                                                            operation.getParameterTypes());

        String operationDesc = _defaultOerationDescription;
        int impact = MBeanOperationInfo.UNKNOWN;

        if (operation.getAnnotation(MBeanOperation.class) != null)
        {
            operationDesc = operation.getAnnotation(MBeanOperation.class).description();
            impact = operation.getAnnotation(MBeanOperation.class).impact();
        }
        operationInfo = new MBeanOperationInfo(operation.getName(),
                                               operationDesc,
                                               paramsInfo,
                                               returnType.getName(),
                                               impact);

        return operationInfo;
    }

    /**
     * Constructs the parameter info.
     * @param paramsAnno
     * @param paramTypes
     * @return MBeanParameterInfo[]
     */
    private static MBeanParameterInfo[] getParametersInfo(Annotation[][] paramsAnno,
                                                          Class<?>[] paramTypes)
    {
        int noOfParams = paramsAnno.length;

        MBeanParameterInfo[] paramsInfo = new MBeanParameterInfo[noOfParams];

        for (int i = 0; i < noOfParams; i++)
        {
            MBeanParameterInfo paramInfo = null;
            String type = paramTypes[i].getName();
            for (Annotation anno : paramsAnno[i])
            {
                String name,desc;
                if (MBeanOperationParameter.class.isInstance(anno))
                {
                    name = MBeanOperationParameter.class.cast(anno).name();
                    desc = MBeanOperationParameter.class.cast(anno).description();
                    paramInfo = new MBeanParameterInfo(name, type, desc);
                }
            }


            if (paramInfo == null)
            {
                paramInfo = new MBeanParameterInfo("p " + (i + 1), type, "parameter " + (i + 1));
            }
            if (paramInfo != null)
                paramsInfo[i] = paramInfo;
        }

        return paramsInfo;
    }

    /**
     * Introspects the MBean class for constructors
     * @param implClass
     * @return MBeanConstructorInfo[]
     */
    static MBeanConstructorInfo[] getMBeanConstructorsInfo(Class implClass)
    {
        List<MBeanConstructorInfo> constructors = new ArrayList<MBeanConstructorInfo>();

        for (Constructor cons : implClass.getConstructors())
        {
            MBeanConstructorInfo constructorInfo = getMBeanConstructorInfo(cons);
            //MBeanConstructorInfo constructorInfo = new MBeanConstructorInfo("desc", cons);
            if (constructorInfo != null)
                constructors.add(constructorInfo);
        }

        return constructors.toArray(new MBeanConstructorInfo[0]);
    }

    /**
     * Retrieves the constructor info from given constructor.
     * @param cons
     * @return MBeanConstructorInfo
     */
    private static MBeanConstructorInfo getMBeanConstructorInfo(Constructor cons)
    {
        String desc = null;
        Annotation anno = cons.getAnnotation(MBeanConstructor.class);
        if (anno != null && MBeanConstructor.class.isInstance(anno))
        {
            desc = MBeanConstructor.class.cast(anno).value();
        }

        //MBeanParameterInfo[] paramsInfo = getParametersInfo(cons.getParameterAnnotations(),
        //                                                    cons.getParameterTypes());

        return new MBeanConstructorInfo(cons.getName(),
                                        desc != null ? _defaultConstructorDescription : desc ,
                                        null);
    }

    /**
     * Retrieves the description from the annotations of given class
     * @param annotatedClass
     * @return class description
     */
    static String getMBeanDescription(Class annotatedClass)
    {
        Annotation anno = annotatedClass.getAnnotation(MBeanDescription.class);
        if (anno != null && MBeanDescription.class.isInstance(anno))
        {
            return MBeanDescription.class.cast(anno).value();
        }
        return _defaultMbeanDescription;
    }

}
