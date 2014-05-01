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
package org.apache.qpid.management.ui.views;

import java.util.ArrayList;

import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.model.OperationData;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;

/**
 * Abstract class for all the control classes of tabs.
 * @author Bhupendra Bhardwaj
 */
public abstract class TabControl
{
    public static final String CONTROLLER = "controller"; 
    protected ManagedBean _mbean = null;
    protected TabFolder _tabFolder = null;
    
    private static java.util.List<String> simpleTypes = new ArrayList<String>();
    
    static
    {
        simpleTypes.add("java.math.BigDecimal");
        simpleTypes.add("java.math.BigInteger");
        simpleTypes.add("java.lang.Boolean");
        simpleTypes.add("java.lang.Byte");
        simpleTypes.add("java.lang.Character");
        simpleTypes.add("java.util.Date");
        simpleTypes.add("java.lang.Double");
        simpleTypes.add("java.lang.Float");
        simpleTypes.add("java.lang.Integer");
        simpleTypes.add("java.lang.Long");
        simpleTypes.add("javax.management.ObjectName");
        simpleTypes.add("java.lang.Short");
        simpleTypes.add("java.lang.String");
        simpleTypes.add("boolean");
    }
    
    public TabControl(TabFolder tabFolder)
    {
        _tabFolder = tabFolder;
    }
    
    /**
     * @return controller composite for the tab
     */
    public Control getControl()
    {
        return null;
    }
    
    public void refresh(ManagedBean mbean)
    {
        if (mbean == null)
        {
            refresh();
        }
    }
    
    public void refresh()
    {
        
    }
    
    public void refresh(ManagedBean mbean, OperationData opData)
    {
        
    }
    
    /**
     * Sets focus on a widget
     */
    public void setFocus()
    {
        
    }
    
    public boolean isSimpleType(Object data)
    {        
        return simpleTypes.contains(data.getClass().getName());
    }
}
