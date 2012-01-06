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
package org.apache.qpid.management.wsdm.capabilities;

import java.lang.management.ManagementFactory;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javassist.CtClass;
import javassist.CtMethod;

import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanOperationInfo;
import javax.management.MBeanParameterInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.apache.qpid.management.domain.handler.impl.QpidDomainObject;
import org.apache.qpid.management.wsdm.common.EntityInstanceNotFoundFault;
import org.apache.qpid.management.wsdm.common.MethodInvocationFault;
import org.apache.qpid.management.wsdm.common.NoSuchAttributeFault;
import org.apache.qpid.management.wsdm.common.QManFault;

/**
 * Test case for MBean capability builder.
 * 
 * @author Andrea Gazzarini
 */
public class MBeanCapabilityBuilderTest extends TestCase
{
	
	/**
	 * Management interface for an mbean that has no properties and no 
	 * methods.
	 * 
	 * @author Andrea Gazzarini
	 */
	public interface NoPropertiesNoMethodsMBean 
	{
	}

	/**
	 * Implementation of the managenent interface described above.
	 * 
	 * @author Andrea Gazzarini
	 */
	public class NoPropertiesNoMethods implements NoPropertiesNoMethodsMBean 
	{
	}

	private MBeanCapabilityBuilder _builder;
	private ObjectName _objectName;
	
	/**
	 * Set up fixture for this test case.
	 */
	protected void setUp() throws Exception
	{
		_builder = new MBeanCapabilityBuilder();
		_objectName = new ObjectName("Test:Name=aName,class=DomainObject");
	}
	
	/**
	 * Tests that state change that occcurs on the begin() method when the requested 
	 * class has not been defined.
	 */
	public void testBegin_withClassNotYetDefined() throws Exception
	{
		_builder.begin(_objectName);
		assertEquals(_builder._state,_builder._classNotAvailable);
	}

	/**
	 * Tests that state change that occcurs on the begin() method when the requested 
	 * class has not been defined.
	 */
	public void testBegin_withClassAlreadyDefined() throws Exception
	{
		_objectName = new ObjectName("Test:Name=aString,class=MBeanCapabilityBuilder");
		_builder.begin(_objectName);
		
		assertTrue(_builder._state instanceof DummyCapabilityBuilder);
		assertSame(_builder._endAttributeHandler, _builder._noPropertyHasBeenDefined);
	}
	
	/**
	 * Tests the generateGetter method().
	 */
	public void testGenerateGetter()
	{
		String name ="MyProperty";
		String type = Long.class.getName();
		String expected = 
			"public "+
			type+
			" get"+
			name+
			"() throws NoSuchAttributeFault,EntityInstanceNotFoundFault,QManFault { return ("+
			type+
			") getAttribute(\""+
			name+
			"\"); }";
		
		String result = _builder.generateGetter(type, name,name);
		assertEquals(expected,result);
	}
	
	/**
	 * Tests the generateGetter method().
	 */
	public void testGenerateSetter()
	{
		String name ="MyProperty";
		String type = Long.class.getName();
		String expected = 
			"public void setMyProperty("+
			type+
			" newValue) throws NoSuchAttributeFault,EntityInstanceNotFoundFault,QManFault { setAttribute(\""+
			name+
			"\", newValue); }";
		
		String result = _builder.generateSetter(type, name,name);
		assertEquals(expected,result);
	}
	
	/**
	 * Tests buils of a capability that has no properties and methods
	 * 
	 * <br>precondition : the incoming entity definition is empty (no properties and no methods)
	 * <br>postcondition : the capability class is built successfully and has no props and methods.
	 * 								The getPropertyNames returns an empty QName array.
	 */
	public void testOK_WithNoPropertiesNoMethods() throws Exception {

		ObjectName name = new ObjectName("Test:Name=NoPropertiesNoMethods");
		
		MBeanServer server = ManagementFactory.getPlatformMBeanServer();
		server.registerMBean(new NoPropertiesNoMethods(), name);
		
		_builder.begin(name);
		_builder.endAttributes();
		_builder.endOperations();
		Class<MBeanCapability> capabilityClass = _builder.getCapabilityClass();
		
		Field[] fields = capabilityClass.getDeclaredFields();
		Method [] methods = capabilityClass.getDeclaredMethods();
		
		assertEquals(Arrays.toString(fields),0,fields.length);
		assertEquals(Arrays.toString(methods),1,methods.length);
		
		Method getPropertyNames = methods[0];
		assertEquals("getPropertyNames",getPropertyNames.getName());
		
		
		MBeanCapability capability = capabilityClass.newInstance();
		QName [] properties = (QName[]) getPropertyNames.invoke(capability);
		assertEquals(0,properties.length);
	}
	
	/**
	 * Tests the whole execution of the builder.
	 */
	@SuppressWarnings("unchecked")
	public void testBuildOK() throws Exception
	{
		MBeanServer server = ManagementFactory.getPlatformMBeanServer();
		QpidDomainObject mbean = new QpidDomainObject();
		server.registerMBean(mbean, _objectName);
		
		_builder.begin(_objectName);
		
		CtClass definition = _builder._capabilityClassDefinition;
		assertEquals(
				MBeanCapability.class.getName(),
				definition.getSuperclass().getName());

		MBeanInfo metadata = server.getMBeanInfo(_objectName);

		for (MBeanAttributeInfo attribute : metadata.getAttributes())
		{
			_builder.onAttribute(attribute);
			checkAttribute(attribute, definition);
			
			assertSame(
					_builder._endAttributeHandler, 
					_builder._atLeastThereIsOneProperty);
		}
		
		for (MBeanOperationInfo operation : metadata.getOperations())
		{
			_builder.onOperation(operation);
			checkOperation(operation,definition);
		}
		
		_builder.endAttributes();
		_builder.endOperations();
		
		assertNotNull(_builder.getCapabilityClass());
	}

	/**
	 * Checks an operation / method after that it has been declared on 
	 * capability definition.
	 * 
	 * @param operation the (JMX) operation metadata.
	 * @param definition the capability class definition.
	 * @throws Exception when something goes wrong during introspection.
	 */
	private void checkOperation(MBeanOperationInfo operation, CtClass definition) throws Exception
	{
		CtMethod method = definition.getDeclaredMethod(operation.getName());
		assertNotNull(method);
		
		checkExceptionTypes(
				method.getExceptionTypes(), 
				new String[]{
					QManFault.class.getName(),
					EntityInstanceNotFoundFault.class.getName(),
					MethodInvocationFault.class.getName()});
		
		assertEquals(Result.class.getName(),method.getReturnType().getName());
		
		CtClass [] parameterTypes = method.getParameterTypes();
		MBeanParameterInfo [] parameterMetadata = operation.getSignature();
		
		assertEquals(parameterTypes.length, parameterMetadata.length);
		for (int i = 0; i < parameterMetadata.length; i++)
		{
			assertEquals(
					parameterTypes[i].getName(),
					Class.forName(parameterMetadata[i].getType()).getCanonicalName());
		}
	}

	/**
	 * Checks the exception types associated with a method.
	 * 
	 * @param exceptionTypes the exception types actually thrown.
	 * @param expectedExceptionNames the expected exception types (as strings).
	 */
	private void checkExceptionTypes(CtClass [] exceptionTypes, String [] expectedExceptionNames)
	{
		List<String> exceptionNames = new ArrayList<String>(exceptionTypes.length);
		for (CtClass exception : exceptionTypes)
		{
			exceptionNames.add(exception.getName());
		}
		
		for (String expectedExceptionName : expectedExceptionNames)
		{
			exceptionNames.remove(expectedExceptionName);			
		}

		assertTrue(exceptionNames.isEmpty());		
	}
	
	/**
	 * Checks an attribute after that it has been declared on capability definition.
	 * 
	 * @param attribute the (JMX) attribute metadata.
	 * @param definition the capability class definition.
	 * @throws Exception when something goes wrong during introspection.
	 */
	private void checkAttribute(MBeanAttributeInfo attribute, CtClass definition) throws Exception
	{
		String name = _builder.getNameForAccessors(attribute.getName());
	
		String newPropertyDeclaration = 
			new StringBuilder("new QName(Names.NAMESPACE_URI, \"")
				.append(attribute.getName())
				.append("\", Names.PREFIX),")
				.toString();
		assertTrue(_builder._properties.indexOf(newPropertyDeclaration) != -1);
		
		if (attribute.isReadable())
		{
			CtMethod getter = definition.getDeclaredMethod("get"+name);
			assertNotNull(getter);

			checkExceptionTypes(
					getter.getExceptionTypes(), 
					new String[]{
						QManFault.class.getName(),
						NoSuchAttributeFault.class.getName(),
						EntityInstanceNotFoundFault.class.getName()});
			
			assertEquals(0,getter.getParameterTypes().length);
			assertEquals(attribute.getType(),getter.getReturnType().getName());
		}	
		
		if (attribute.isWritable())
		{
			CtMethod setter = definition.getDeclaredMethod("set"+name);
			assertNotNull(setter);

			checkExceptionTypes(
					setter.getExceptionTypes(), 
					new String[]{
						QManFault.class.getName(),
						NoSuchAttributeFault.class.getName(),
						EntityInstanceNotFoundFault.class.getName()});
			
			CtClass [] parameterTypes = setter.getParameterTypes();
			
			assertEquals(1,parameterTypes.length);
			assertEquals(attribute.getType(),parameterTypes[0].getName());
			assertEquals(void.class.getName(),setter.getReturnType().getName());				
		}
	}
}