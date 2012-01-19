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
package org.apache.qpid.management.wsdm;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import javax.xml.namespace.QName;

import org.apache.muse.core.proxy.ProxyHandler;
import org.apache.muse.core.proxy.ReflectionProxyHandler;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.qpid.management.Names;
import org.apache.qpid.management.wsdm.capabilities.Result;

/**
 * Test case for QMan operation invocation interface.
 * 
 * @author Andrea Gazzarini
 */
public class OperationInvocationInterfaceTestCase extends BaseWsDmAdapterTestCase
{
	private Map<String, ProxyHandler> _invocationHandlers = createInvocationHandlers();

	/**
	 * Test operation invocation on WS-Resource.
	 * This method tests the exchange of a byte type array between requestor 
	 * and service provider.
	 * 
	 * <br>precondition : 	a WS-Resource exists and is registered and the requested 
	 * 								operation is available on that. 
	 * <br>postcondition : invocations are executed successfully, no exception is thrown 
	 * 								and byte array are correctly returned.
	 */
	@SuppressWarnings("unchecked")
	public void testOperationInvocationOK_withByteArray() throws Exception
	{
		byte [] expectedByteResult = {1,3,4,2,2,44,22,3,3,55,66};

		Object result = _resourceClient.invoke(
				_invocationHandlers.get("echoWithByteArray"), 
				new Object[]{expectedByteResult});

		Method getOutputParameters = result.getClass().getMethod("getOutputParameters");
		
		Map<String,Object> out = (Map<String, Object>) getOutputParameters.invoke(result);
		
		assertEquals("Output parameters must be 1.",1,out.size());
		assertArrayEquals(expectedByteResult, out.get(byte[].class.getName()));
	}		
		
	/**
	 * Test a simple operation invocation on a WS-Resource.
	 * This method tests a simple operation without any input and output parameters.
	 * 
	 * <br>precondition : a ws resource exists and is registered and the requested operation 
	 * 								is available on that. 
	 * <br>postcondition : invocations are executed successfully an no exception is thrown.
	 */
	@SuppressWarnings("unchecked")
	public void testSimpleOperationInvocationOK() throws Exception
	{
		Object result = _resourceClient.invoke(
				_invocationHandlers.get("voidWithoutArguments"), 
				null);

		assertNotNull(result);
	}

	/**
	 * Test a the invocation on a WS-Resource with a method that throws an exception..
	 * 
	 * <br>precondition : 	a ws resource exists and is registered and the requested 
	 * 								operation is available on that. 
	 * <br>postcondition : an exception is thrown by the requested method.
	 */
	@SuppressWarnings("unchecked")
	public void testInvocationException_OK() throws Exception
	{
		try 
		{
		 	_resourceClient.invoke(
					_invocationHandlers.get("throwsException"), 
					null);
		 	fail("The requested operation has thrown an exception so a Soap Fault is expected...");
		} catch(SoapFault expected)
		{
		}
	}	
	
	/**
	 * Test operation invocation on WS-Resource.
	 * This method tests the exchange of UUID type between requestor and service provider.
	 * 
	 * <br>precondition : 	a WS-Resource exists and is registered and the requested operation 
	 * 								is available on that. 
	 * <br>postcondition : invocations are executed successfully, no exception is thrown 
	 * 								and parameters are correctly returned.
	 */
	@SuppressWarnings("unchecked")
	public void testOperationInvocationOK_withUUID() throws Exception
	{
		UUID expectedUuid = UUID.randomUUID();

		Object result = _resourceClient.invoke(
				_invocationHandlers.get("echoWithUUID"), 
				new Object[]{expectedUuid});

		Method getOutputParameters = result.getClass().getMethod("getOutputParameters");
		
		Map<String,Object> out = (Map<String, Object>) getOutputParameters.invoke(result);
		
		assertEquals("Output parameters must be 1.",1,out.size());
		assertEquals(expectedUuid, out.get("uuid"));
	}		

	/**
	 * Test operation invocation on WS-Resource.
	 * This method tests the exchange of Map type between requestor and service provider.
	 * For this test exchanged arrays contain :
	 * 
	 * <br>precondition : 	a ws resource exists and is registered and the requested 
	 * 								operation is available on that. 
	 * <br>postcondition : invocations are executed successfully, no exception is 
	 * 								thrown and parameters are correctly returned.
	 */
	@SuppressWarnings("unchecked")
	public void testOperationInvocationOK_withMap() throws Exception
	{
		Map<String,Object> expectedMap = new HashMap<String, Object>();
		expectedMap.put("p1", new Long(1));
		expectedMap.put("p2", Boolean.TRUE);
		expectedMap.put("p3", 1234d);
		expectedMap.put("p4", 11.2f);
		expectedMap.put("p5", 1272);
		expectedMap.put("p6", (short)12);
		expectedMap.put("p7", "aString");
		expectedMap.put("p8", "http://qpid.apache.org");
		expectedMap.put("p9", new Date(12383137128L));
		expectedMap.put("p10", new byte[]{1,2,2,3,3,4});
		
		Object result = _resourceClient.invoke(
				_invocationHandlers.get("echoWithMap"), 
				new Object[]{expectedMap});

		Method getOutputParameters = result.getClass().getMethod("getOutputParameters");
		
		Map<String,Object> out = (Map<String, Object>) ((Map<String, Object>) getOutputParameters.invoke(result)).get("map");
		
		assertEquals("Output parameters must be 10.",10,out.size());
			assertEquals(expectedMap.get("p1"),out.get("p1"));
			assertEquals(expectedMap.get("p2"),out.get("p2"));
			assertEquals(expectedMap.get("p3"),out.get("p3"));
			assertEquals(expectedMap.get("p4"),out.get("p4"));
			assertEquals(expectedMap.get("p5"),out.get("p5"));
			assertEquals(expectedMap.get("p6"),out.get("p6"));
			assertEquals(expectedMap.get("p7"),out.get("p7"));
			assertEquals(expectedMap.get("p8"),out.get("p8"));
			assertEquals(expectedMap.get("p9"),out.get("p9"));
			assertTrue( Arrays.equals((byte[])expectedMap.get("p10"),(byte[])out.get("p10")));
	}			
	
	/**
	 * Test operation invocation on WS-Resource.
	 * This method tests the exchange of simple types between requestor and 
	 * service provider.
	 * 
	 * With simple types we mean :
	 * 
	 * <ul>
	 * 	<li>java.lang.Long / long (xsd:long)
	 * 	<li>java.lang.Integer / int (xsd:int / xsd:integer)
	 * 	<li>java.lang.Double/ double (xsd:double)
	 * 	<li>java.lang.Float / float (xsd:float)
	 * 	<li>java.lang.Short / short (xsd:short)
	 * 	<li>java.lang.Boolean / boolean (xsd:boolean)
	 * 	<li>java.lang.String (xsd:string)
	 * 	<li>java.net.URI (xsd:anyURI)
	 * 	<li>java.util.Date(xsd:dateTime)
	 * </ul>
	 * 
	 * <br>precondition : 	a ws resource exists and is registered and the requested operation is 
	 * 								available on that. 
	 * <br>postcondition : invocations are executed successfully, no exception is thrown and 
	 * 								parameters are correctly returned.
	 */
	@SuppressWarnings("unchecked")
	public void testOperationInvocationOK_withSimpleTypes() throws Exception
	{
		Long expectedLongResult = new Long(1373);
		Boolean expectedBooleanResult = Boolean.TRUE;
		Double expectedDoubleResult = new Double(12763.44);
		Float expectedFloatResult = new Float(2727.233f);
		Integer expectedIntegerResult = new Integer(28292);
		Short expectedShortResult = new Short((short)227);
		String expectedStringResult = "expectedStringResult";
		URI expectedUriResult = URI.create("http://qpid.apache.org/");
		Date expectedDateResult = new Date();
		
		Object result = _resourceClient.invoke(
				_invocationHandlers.get("echoWithSimpleTypes"), 
				new Object[]{
					expectedLongResult,
					expectedBooleanResult,
					expectedDoubleResult,
					expectedFloatResult,
					expectedIntegerResult,
					expectedShortResult,
					expectedStringResult,
					expectedUriResult,
					expectedDateResult});

		Method getOutputParameters = result.getClass().getMethod("getOutputParameters");
		Map<String,Object> out = (Map<String, Object>) getOutputParameters.invoke(result);
		
		assertEquals("Output parameters must be 9.",9,out.size());
		assertTrue("Long output parameter not found on result object.",out.containsValue(expectedLongResult));
		assertTrue("Boolean output parameter not found on result object.",out.containsValue(expectedBooleanResult));
		assertTrue("Double output parameter not found on result object.",out.containsValue(expectedDoubleResult));
		assertTrue("Float output parameter not found on result object.",out.containsValue(expectedFloatResult));
		assertTrue("Integer output parameter not found on result object.",out.containsValue(expectedIntegerResult));
		assertTrue("Short output parameter not found on result object.",out.containsValue(expectedShortResult));
		assertTrue("String output parameter not found on result object.",out.containsValue(expectedStringResult));
		assertTrue("URI output parameter not found on result object.",out.containsValue(expectedUriResult));
		assertTrue("Date output parameter not found on result object.",out.containsValue(expectedDateResult));		
	}
	
	/**
	 * Test operation invocation on WS-Resource.
	 * This method tests the exchange of arrays between requestor and service provider.
	 * For this test exchanged arrays contain :
	 * 
	 * <ul>
	 * 	<li>java.lang.Long  (xsd:long)
	 * 	<li>java.lang.Integer (xsd:int / xsd:integer)
	 * 	<li>java.lang.Double (xsd:double)
	 * 	<li>java.lang.Float (xsd:float)
	 * 	<li>java.lang.Short (xsd:short)
	 * 	<li>java.lang.Boolean (xsd:boolean)
	 * 	<li>java.lang.String (xsd:string)
	 * 	<li>java.net.URI (xsd:anyURI)
	 * 	<li>java.util.Date(xsd:dateTime)
	 * </ul>
	 * 
	 * <br>precondition : a ws resource exists and is registered and the requested operation is available on that. 
	 * <br>postcondition : invocations are executed successfully, no exception is thrown and parameters are correctly returned.
	 */
	@SuppressWarnings("unchecked")
	public void testOperationInvocationOK_withWrapperArrays() throws Exception
	{
		Long [] expectedLongResult = {new Long(2),new Long(1),new Long(3),new Long(4)};
		Boolean [] expectedBooleanResult = { Boolean.TRUE,Boolean.FALSE,Boolean.FALSE};
		Double [] expectedDoubleResult = {12763.44d,2832.33d,2292.33d,22293.22d};
		Float [] expectedFloatResult = {2727.233f,1f,2f,4f,5.4f,33.2f};
		Integer [] expectedIntegerResult = {1,2,3,4,55,66,77,88,99};
		Short [] expectedShortResult = {(short)227,(short)23,(short)9};
		String [] expectedStringResult = {"s1","s2","s333","s4"};
		URI [] expectedUriResult = {
				URI.create("http://qpid.apache.org/"),
				URI.create("http://www.apache.org"),
				URI.create("http://projects.apache.org")};
		
		Date [] expectedDateResult = {
				new Date(), 
				new Date(38211897),
				new Date(903820382)};
		
		Object result = _resourceClient.invoke(
				_invocationHandlers.get("echoWithArrays"), 
				new Object[]{
					expectedLongResult,
					expectedBooleanResult,
					expectedDoubleResult,
					expectedFloatResult,
					expectedIntegerResult,
					expectedShortResult,
					expectedStringResult,
					expectedUriResult,
					expectedDateResult});

		Method getOutputParameters = result.getClass().getMethod("getOutputParameters");
		Map<String,Object> out = (Map<String, Object>) getOutputParameters.invoke(result);
		
		assertEquals("Output parameters must be 9.",9,out.size());
		assertTrue("Long array doesn't match.",Arrays.equals(expectedLongResult, (Long[])out.get(Long.class.getName())));
		assertTrue("Boolean array doesn't match.",Arrays.equals(expectedBooleanResult, (Boolean[])out.get(Boolean.class.getName())));
		assertTrue("Double array doesn't match.",Arrays.equals(expectedDoubleResult, (Double[])out.get(Double.class.getName())));
		assertTrue("Float array doesn't match.",Arrays.equals(expectedFloatResult, (Float[])out.get(Float.class.getName())));
		assertTrue("Integer array doesn't match.", Arrays.equals(expectedIntegerResult, (Integer[])out.get(Integer.class.getName())));
		assertTrue("Short array doesn't match.",Arrays.equals(expectedShortResult, (Short[])out.get(Short.class.getName())));
		assertTrue("String array doesn't match.",Arrays.equals(expectedStringResult, (String[])out.get(String.class.getName())));
		assertTrue("URI array doesn't match.",Arrays.equals(expectedUriResult, (URI[])out.get(URI.class.getName())));
		assertTrue("Date array doesn't match.",Arrays.equals(expectedDateResult, (Date[])out.get(Date.class.getName())));
	}	

	/**
	 * Test operation invocation on WS-Resource.
	 * This method tests the exchange of primitive type arrays between requestor and service provider.
	 * NOte that even the sent array contain primtiive type QMan deals only with objects so in the result 
	 * object you will find the corresponding wrapper types.
	 * 
	 * For this test exchanged arrays contain :
	 * 
	 * <ul>
	 * 	<li>java.lang.Long / long (xsd:long)
	 * 	<li>java.lang.Integer / int (xsd:int / xsd:integer)
	 * 	<li>java.lang.Double/ double (xsd:double)
	 * 	<li>java.lang.Float / float (xsd:float)
	 * 	<li>java.lang.Short / short (xsd:short)
	 * 	<li>java.lang.Boolean / boolean (xsd:boolean)
	 * </ul>
	 * 
	 * <br>precondition : a ws resource exists and is registered and the requested operation is available on that. 
	 * <br>postcondition : invocations are executed successfully, no exception is thrown and parameters are correctly returned.
	 */
	@SuppressWarnings("unchecked")
	public void testOperationInvocationOK_withPrimitiveArrays() throws Exception
	{
		long [] expectedLongResult = {1L,2L,3L,4L};
		boolean [] expectedBooleanResult = { true,false,false};
		double [] expectedDoubleResult = {12763.44d,2832.33d,2292.33d,22293.22d};
		float [] expectedFloatResult = {2727.233f,1f,2f,4f,5.4f,33.2f};
		int [] expectedIntegerResult = {1,2,3,4,55,66,77,88,99};
		short [] expectedShortResult = {(short)227,(short)23,(short)9};
		
		Object result = _resourceClient.invoke(
				_invocationHandlers.get("echoWithSimpleTypeArrays"), 
				new Object[]{
					expectedLongResult,
					expectedBooleanResult,
					expectedDoubleResult,
					expectedFloatResult,
					expectedIntegerResult,
					expectedShortResult});

		Method getOutputParameters = result.getClass().getMethod("getOutputParameters");
		Map<String,Object> out = (Map<String, Object>) getOutputParameters.invoke(result);
		
		assertEquals("Output parameters must be 6.",6,out.size());
		assertArrayEquals(expectedLongResult, out.get(long.class.getName()));
		assertArrayEquals(expectedBooleanResult, out.get(boolean.class.getName()));
		assertArrayEquals(expectedDoubleResult, out.get(double.class.getName()));
		assertArrayEquals(expectedFloatResult, out.get(float.class.getName()));
		assertArrayEquals(expectedIntegerResult, out.get(int.class.getName()));
		assertArrayEquals(expectedShortResult, out.get(short.class.getName()));
	}	
	
	/**
	 * Internal method used for array comparison using reflection.
	 * 
	 * @param expectedArray the expected array.
	 * @param resultArray the array that must match the expected one.
	 */
	private void assertArrayEquals(Object expectedArray, Object resultArray) 
	{
		int expectedArrayLength = Array.getLength(expectedArray);
		int resultArrayLength = Array.getLength(resultArray);
		
		assertEquals(expectedArrayLength,resultArrayLength);
		
		for (int index = 0; index < expectedArrayLength; index++)
		{
			Object expected = Array.get(expectedArray, index);
			Object result = Array.get(resultArray, index);
			
			assertEquals(expected,result);
		}
	}
	
	private Map<String,ProxyHandler> createInvocationHandlers() 
	{
		Map<String, ProxyHandler> handlers = new HashMap<String, ProxyHandler>();
		
		ProxyHandler handler = new ReflectionProxyHandler();
        handler.setAction(Names.NAMESPACE_URI+"/"+"voidWithoutArguments");
        handler.setRequestName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"voidWithoutArgumentsRequest", 
        				Names.PREFIX));
        handler.setRequestParameterNames(new QName[]{});       
        handler.setResponseName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"voidWithoutArgumentsResponse",  
        				Names.PREFIX));
        handler.setReturnType(Result.class); 
        
        ProxyHandler exceptionHandler = new ReflectionProxyHandler();
        exceptionHandler.setAction(Names.NAMESPACE_URI+"/"+"throwsException");
        exceptionHandler.setRequestName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"throwsExceptionRequest", 
        				Names.PREFIX));
        
        exceptionHandler.setRequestParameterNames(new QName[]{});        
        exceptionHandler.setResponseName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"throwsExceptionResponse",  
        				Names.PREFIX));
        
        exceptionHandler.setReturnType(Result.class); 
        
        ProxyHandler echoWithWrapperTypesHandler = new ReflectionProxyHandler();
        echoWithWrapperTypesHandler.setAction(Names.NAMESPACE_URI+"/"+"echoWithSimpleTypes");
        echoWithWrapperTypesHandler.setRequestName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithSimpleTypesRequest", 
        				Names.PREFIX));
        
        echoWithWrapperTypesHandler.setRequestParameterNames(new QName[]{
        		new QName(Names.NAMESPACE_URI,"p1",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p2",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p3",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p4",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p5",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p6",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p7",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p8",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p9",Names.PREFIX),
        });        
        
        echoWithWrapperTypesHandler.setResponseName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithSimpleTypesResponse", 
        				Names.PREFIX));
        
        echoWithWrapperTypesHandler.setReturnType(Result.class);
        
        ProxyHandler echoWithArrayOfWrapperTypes = new ReflectionProxyHandler();
        echoWithArrayOfWrapperTypes.setAction(Names.NAMESPACE_URI+"/"+"echoWithArrays");
        echoWithArrayOfWrapperTypes.setRequestName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithArraysRequest",  
        				Names.PREFIX));
        
        echoWithArrayOfWrapperTypes.setRequestParameterNames(new QName[]{
        		new QName(Names.NAMESPACE_URI,"p1",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p2",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p3",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p4",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p5",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p6",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p7",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p8",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p9",Names.PREFIX),
        });        
        
        echoWithArrayOfWrapperTypes.setResponseName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithArraysResponse", 
        				Names.PREFIX));
        
        echoWithArrayOfWrapperTypes.setReturnType(Result.class);
        
        ProxyHandler echoWithArrayOfPrimitiveTypes = new ReflectionProxyHandler();
        echoWithArrayOfPrimitiveTypes.setAction(Names.NAMESPACE_URI+"/"+"echoWithSimpleTypeArrays");
        echoWithArrayOfPrimitiveTypes.setRequestName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithSimpleTypeArraysRequest",  
        				Names.PREFIX));
        
        echoWithArrayOfPrimitiveTypes.setRequestParameterNames(new QName[]{
        		new QName(Names.NAMESPACE_URI,"p1",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p2",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p3",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p4",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p5",Names.PREFIX),
        		new QName(Names.NAMESPACE_URI,"p6",Names.PREFIX)});        
        
        echoWithArrayOfPrimitiveTypes.setResponseName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithSimpleTypeArraysResponse", 
        				Names.PREFIX));
        
        echoWithArrayOfPrimitiveTypes.setReturnType(Result.class);
        
        ProxyHandler echoWithByteArray = new EnhancedReflectionProxyHandler();
        echoWithByteArray.setAction(Names.NAMESPACE_URI+"/"+"echoWithByteArray");
        echoWithByteArray.setRequestName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithByteArrayRequest",  
        				Names.PREFIX));
        
        echoWithByteArray.setRequestParameterNames(
        		new QName[]{
        				new QName(Names.NAMESPACE_URI,"p1",Names.PREFIX)});        
        
        echoWithByteArray.setResponseName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithByteArrayResponse", 
        				Names.PREFIX));
        
        echoWithByteArray.setReturnType(Result.class);
        
        ProxyHandler echoWithUUID = new EnhancedReflectionProxyHandler();
        echoWithUUID.setAction(Names.NAMESPACE_URI+"/"+"echoWithUUID");
        echoWithUUID.setRequestName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithUUIDRequest",  
        				Names.PREFIX));
        
        echoWithUUID.setRequestParameterNames(
        		new QName[]{
        				new QName(Names.NAMESPACE_URI,"p1",Names.PREFIX)});        
        
        echoWithUUID.setResponseName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithUUIDResponse", 
        				Names.PREFIX));
        
        echoWithUUID.setReturnType(Result.class);
        
        ProxyHandler echoWithMap = new EnhancedReflectionProxyHandler();
        echoWithMap.setAction(Names.NAMESPACE_URI+"/"+"echoWithMap");
        echoWithMap.setRequestName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithMapRequest",  
        				Names.PREFIX));
        
        echoWithMap.setRequestParameterNames(
        		new QName[]{
        				new QName(Names.NAMESPACE_URI,"p1",Names.PREFIX)});        
        
        echoWithMap.setResponseName(
        		new QName(
        				Names.NAMESPACE_URI, 
        				"echoWithMapResponse", 
        				Names.PREFIX));
        
        echoWithMap.setReturnType(Result.class);
                
        handlers.put("voidWithoutArguments",handler);
        handlers.put("echoWithSimpleTypes",echoWithWrapperTypesHandler);
        handlers.put("echoWithArrays",echoWithArrayOfWrapperTypes);
        handlers.put("echoWithSimpleTypeArrays", echoWithArrayOfPrimitiveTypes);
        handlers.put("echoWithByteArray", echoWithByteArray);
        handlers.put("echoWithUUID", echoWithUUID);
        handlers.put("echoWithMap", echoWithMap);
        handlers.put("throwsException",exceptionHandler);
        return handlers;
	}	
}