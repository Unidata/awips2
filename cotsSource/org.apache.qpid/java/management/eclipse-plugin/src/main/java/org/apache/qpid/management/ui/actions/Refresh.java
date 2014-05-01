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

import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.views.MBeanView;
import org.apache.qpid.management.ui.views.NavigationView;
import org.eclipse.jface.action.IAction;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * This action refreshes both the views -Navigation and MBeanView
 * @author Bhupendra Bhardwaj
 */
public class Refresh extends AbstractAction implements IWorkbenchWindowActionDelegate
{    
    public void run(IAction action)
    {
        if(_window != null)
        {   
            NavigationView view = (NavigationView)_window.getActivePage().findView(NavigationView.ID);
            view.refresh();
            
            MBeanView mbeanview = (MBeanView)_window.getActivePage().findView(MBeanView.ID);
            try
            {
                mbeanview.refresh();
            }
            catch (Exception ex)
            {
                MBeanUtility.handleException(ex);
            }
        }
    }
}
