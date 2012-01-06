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
package org.apache.qpid.qman.debug;

import javax.management.ObjectName;

import org.apache.muse.util.xml.XmlUtils;
import org.apache.qpid.transport.util.Logger;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Utility class used for debbugging XML messages
 * 
 * @author Andrea Gazzarini
 */
public class XmlDebugger {
	public final static Logger LOGGER = Logger.get(XmlDebugger.class);
	
	/**
	 * Prints out to log the given node.
	 * 
	 * @param node the xml node to be printed out.
	 */
	public static void debug(Node node) 
	{
		if (LOGGER.isDebugEnabled())
		{
			LOGGER.debug(XmlUtils.toString(node, false,true));
		}
	}

	/**
	 * Prints out to log the given node.
	 * 
	 * @param node the xml node to be printed out.
	 */
	public static void debug(ObjectName resourceId, Node node) 
	{
		if (LOGGER.isDebugEnabled())
		{
			LOGGER.debug(resourceId+" : "+XmlUtils.toString(node, false,true));
		}
	}

	
	/**
	 * Prints out to log the given element array.
	 * 
	 * @param elements the element array to be printed out.
	 */
	public static void debug(Element [] elements) 
	{
		if (LOGGER.isDebugEnabled())
		{
			StringBuilder builder = new StringBuilder();
			for (Element element : elements) {
				builder.append(XmlUtils.toString(element,false,true));
				builder.append(System.getProperty("line.separator"));
			}
			LOGGER.debug(builder.toString());
		}
	}
	
	/**
	 * Prints out to log the given node.
	 * 
	 * @param node the xml node to be printed out.
	 */
	public static void debug(String node) 
	{
			LOGGER.debug(node);
	}
}