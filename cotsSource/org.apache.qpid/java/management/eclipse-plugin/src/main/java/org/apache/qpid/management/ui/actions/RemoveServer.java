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

import org.apache.qpid.management.ui.exceptions.InfoRequiredException;
import org.apache.qpid.management.ui.views.NavigationView;
import org.apache.qpid.management.ui.views.ViewUtility;
import org.eclipse.jface.action.IAction;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public class RemoveServer extends AbstractAction implements IWorkbenchWindowActionDelegate
{       
    public void run(IAction action)
    {
        if(_window != null)
        {   
            NavigationView view = (NavigationView)_window.getActivePage().findView(NavigationView.ID);
            try
            {
                view.removeServer();
            }
            catch(InfoRequiredException ex)
            {
                ViewUtility.popupInfoMessage("Remove Qpid server", ex.getMessage());
            }
            catch(Exception ex)
            {
                handleException(ex, "Server could not be removed", null);
            }
        }
    }
}
