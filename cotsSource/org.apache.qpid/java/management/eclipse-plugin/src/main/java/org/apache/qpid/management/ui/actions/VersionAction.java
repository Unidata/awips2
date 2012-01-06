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
package org.apache.qpid.management.ui.actions;

import java.io.InputStream;
import java.util.Properties;

import org.apache.qpid.management.ui.Constants;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IWorkbenchWindow;

public class VersionAction extends Action
{        
    private IWorkbenchWindow _window;
    public static final String VERSION_RESOURCE = "qpidversion.properties";

    public static final String PRODUCT_NAME_PROPERTY = "qpid.name";
    public static final String RELEASE_VERSION_PROPERTY = "qpid.version";
    public static final String BUILD_VERSION_PROPERTY = "qpid.svnversion";
    
    private static final String DEFAULT = "unknown";
    private static String _releaseVersion;
    private static String _buildVersion;
    private static String _text;
    
    static
    {
        Properties props = new Properties();
        try
        {
            InputStream propertyStream = VersionAction.class.getClassLoader().getResourceAsStream(VERSION_RESOURCE);
            if (propertyStream != null)           
            {
                props.load(propertyStream);
                _releaseVersion = readPropertyValue(props, RELEASE_VERSION_PROPERTY);
                _buildVersion = readPropertyValue(props, BUILD_VERSION_PROPERTY); 
                _text = "Build Version : " + _buildVersion + "\n" +
                        "Release Version : " + _releaseVersion;
            }
            else
            {
                _text = "Build Version :  \n" +
                        "Release Version :  ";
            }
        }
        catch (Exception ex)
        {
            MBeanUtility.printStackTrace(ex);
        }
    }
    
    public VersionAction(IWorkbenchWindow window)
    {
        _window = window;
        setText("About " + Constants.APPLICATION_NAME);
        setId("qpidmc.about");
        setActionDefinitionId("qpidmc.about");
    }
    
    private static String readPropertyValue(Properties props, String propertyName)
    {
        String retVal = (String) props.get(propertyName);
        if (retVal == null)
        {
            retVal = DEFAULT;
        }
        return retVal;
    }
    
    public void run()
    {
        MessageDialog.openInformation(_window.getShell(), Constants.APPLICATION_NAME, _text);
    }
}
