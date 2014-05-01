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
package org.apache.qpid.example.shared;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

/**
 * Class that provides helper methods for JNDI
 */
public class InitialContextHelper
{

    public static final String _defaultPropertiesName = "example.properties";
    protected static Properties _fileProperties;
    protected static InitialContext _initialContext;
    protected static final Logger _log = LoggerFactory.getLogger(InitialContextHelper.class);

    public InitialContextHelper(String propertiesName) throws ContextException
    {
        try
        {
            if ((propertiesName == null) || (propertiesName.length() == 0))
            {
                propertiesName = _defaultPropertiesName;
            }

            _fileProperties = new Properties();
            ClassLoader cl = this.getClass().getClassLoader();

            // NB: Need to change path to reflect package if moving classes around !
            InputStream is = cl.getResourceAsStream("org/apache/qpid/example/shared/" + propertiesName);
            _fileProperties.load(is);
            _initialContext = new InitialContext(_fileProperties);
        }
        catch (IOException e)
        {
            throw new ContextException(e.toString(), e);
        }
        catch (NamingException n)
        {
            throw new ContextException(n.toString(), n);
        }
    }

    public Properties getFileProperties()
    {
        return _fileProperties;
    }

    public InitialContext getInitialContext()
    {
        return _initialContext;
    }

}
