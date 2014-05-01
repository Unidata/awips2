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

import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.views.MBeanView;
import org.apache.qpid.management.ui.views.NavigationView;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.WorkbenchWindowControlContribution;

public class RefreshIntervalComboPanel extends WorkbenchWindowControlContribution
{
    private Display _display = null;
    private RefreshTask _refreshTask = null;
    
    private static int DEFAULT_INDEX = 2; //15seconds
    private static final int[] INTERVALS = new int[]{5,10,15,30,60,120,300,600};

    public RefreshIntervalComboPanel()
    {
        super();
    }
    
    public RefreshIntervalComboPanel(String id)
    {
        super(id);
    }

    @Override
    public Control createControl(Composite parent)
    {
        _display = parent.getDisplay();
        
        Composite holder = new Composite(parent,SWT.NONE);
        holder.setLayout(new GridLayout(2,false));
        holder.setLayoutData(new GridData(SWT.LEFT,SWT.TOP,false,false));
        
        Label refreshLabel = new Label(holder,SWT.NONE);
        refreshLabel.setLayoutData(new GridData(SWT.LEFT,SWT.TOP,false,true));
        refreshLabel.setText("Refresh Interval: ");
        
        final Combo combo = new Combo(holder, SWT.NONE | SWT.DROP_DOWN | SWT.READ_ONLY);
        combo.setLayoutData(new GridData(SWT.LEFT,SWT.TOP,false,false));
        for(int i=0; i< INTERVALS.length ; i++)
        {
            combo.add(String.valueOf(INTERVALS[i]) + " seconds");
        }
        combo.select(DEFAULT_INDEX);
        
        _refreshTask = new RefreshTask(INTERVALS[DEFAULT_INDEX]);
        _display.timerExec(1000 * INTERVALS[DEFAULT_INDEX], _refreshTask);
        
        combo.addModifyListener(
                new ModifyListener()
                {
                    public void modifyText(final ModifyEvent e)
                    {
                        _display.timerExec(-1, _refreshTask); //cancels existing refresh runnable
                        _refreshTask = new RefreshTask(INTERVALS[combo.getSelectionIndex()]);
                        _display.timerExec(0, _refreshTask); //immediately refresh and begin new schedule
                    }
                });
        
        return holder;
    }
    
    
    private class RefreshTask implements Runnable
    {
        private int seconds;
        
        public RefreshTask(int secs)
        {
            this.seconds = secs;
        }

        @Override
        public void run()
        {
            IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
            if(window != null)
            {
                final MBeanView mbView = (MBeanView)window.getActivePage().findView(MBeanView.ID);

                final NavigationView navView = (NavigationView)window.getActivePage().findView(NavigationView.ID);
                try
                {
                    mbView.refresh();
                    navView.refresh();
                }
                catch (Exception ex)
                {
                    MBeanUtility.handleException(ex);
                }

                _display.timerExec(1000 * seconds, this);
            }
        }
        
    }
    
}
