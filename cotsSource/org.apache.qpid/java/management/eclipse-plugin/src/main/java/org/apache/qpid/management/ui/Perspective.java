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
package org.apache.qpid.management.ui;

import org.apache.qpid.management.ui.views.MBeanView;
import org.apache.qpid.management.ui.views.NavigationView;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * 
 * @author Bhupendra Bhardwaj
 */
public class Perspective implements IPerspectiveFactory
{
	public void createInitialLayout(IPageLayout layout)
    {
		String editorArea = layout.getEditorArea();
		layout.setEditorAreaVisible(false);
        
		// standalone view meaning it can't be docked or stacked with other views, and it doesn't have a title bar.        
		layout.addStandaloneView(NavigationView.ID, true, IPageLayout.LEFT, 0.30f, editorArea);
        layout.addStandaloneView(MBeanView.ID, true, IPageLayout.RIGHT, 0.70f, editorArea);
		
		layout.getViewLayout(NavigationView.ID).setCloseable(false);
        layout.getViewLayout(MBeanView.ID).setCloseable(false);       
	}
}
