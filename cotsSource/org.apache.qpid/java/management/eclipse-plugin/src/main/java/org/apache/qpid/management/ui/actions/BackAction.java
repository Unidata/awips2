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
import org.eclipse.jface.action.Action;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;


public class BackAction extends Action implements IWorkbenchAction
{
    private static final String ID = "org.apache.qpid.management.ui.actions.back";

    public BackAction()
    {
        setText("Back");
        setImageDescriptor(org.apache.qpid.management.ui.Activator.getImageDescriptor("/icons/back.gif"));
        setId(ID);
    }

    public void run()
    {
        MBeanView mbeanview = (MBeanView)PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView(MBeanView.ID);
        try
        {
            mbeanview.back();
        }
        catch (Exception ex)
        {
            MBeanUtility.handleException(ex);
        }

    }

    public void dispose()
    {

    }
}
