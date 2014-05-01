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

import static org.apache.qpid.management.ui.Constants.BUTTON_CLEAR;
import static org.apache.qpid.management.ui.Constants.CONSOLE_IMAGE;
import static org.apache.qpid.management.ui.Constants.FONT_BUTTON;

import java.util.ArrayList;
import java.util.List;

import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ServerRegistry;
import org.apache.qpid.management.ui.model.NotificationObject;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;

public class VHNotificationsTabControl extends TabControl
{
    protected FormToolkit  _toolkit;
    protected Form _form;
    protected Table _table = null;
    protected TableViewer _tableViewer  = null;
     
    protected Thread _worker = null;
    protected boolean _workerRunning = false;
    
    protected List<NotificationObject> _notifications = null;
    
    private static final String COLUMN_OBJ = "Object Name";
    private static final String COLUMN_SEQ  = "Sequence No";
    private static final String COLUMN_TIME = "TimeStamp";
    private static final String COLUMN_TYPE  = "Type";
    private static final String COLUMN_MSG  = "Notification Message";
    protected static final String[] _tableTitles = new String [] {
            COLUMN_OBJ,
            COLUMN_SEQ,
            COLUMN_TIME,
            COLUMN_TYPE,
            COLUMN_MSG
         };
    
    protected Button _clearButton       = null;
    
    public VHNotificationsTabControl(TabFolder tabFolder)
    {
        super(tabFolder);
        _toolkit = new FormToolkit(_tabFolder.getDisplay());
        _form = _toolkit.createForm(_tabFolder);
        GridLayout gridLayout = new GridLayout();      
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;       
        _form.getBody().setLayout(gridLayout);
        
        createWidgets();
    }
    
    protected void createWidgets()
    {       
        addButtons();  
        createTableViewer();
    }
    
    /**
     * @see TabControl#getControl()
     */
    public Control getControl()
    {
        return _form;
    }

    /**
     * Creates clear buttin and refresh button
     */
    protected void addButtons()
    {    
        Composite composite = _toolkit.createComposite(_form.getBody(), SWT.NONE);
        composite.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
        composite.setLayout(new GridLayout(2,false));
        
        // Add Clear Button
        _clearButton = _toolkit.createButton(composite, BUTTON_CLEAR, SWT.PUSH | SWT.CENTER);
        _clearButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        GridData gridData = new GridData(SWT.LEAD, SWT.TOP, true, false);
        gridData.widthHint = 80;
        _clearButton.setLayoutData(gridData);
        _clearButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {  
                ServerRegistry serverRegistry = ApplicationRegistry.getServerRegistry(MBeanView.getServer());
                IStructuredSelection ss = (IStructuredSelection)_tableViewer.getSelection();
                if(!ss.isEmpty())
                {
                    //clear selected Notifications
                    serverRegistry.clearNotifications(null, ss.toList());
                }
                else if(_notifications != null)
                {
                    //clear all the notifications, if there are any
                    
                    //check the user is certain of this clear-all operation
                    int response = ViewUtility.popupOkCancelConfirmationMessage(
                            "Clear Notifications", "Clear all Notifications for this VirtualHost?");
                    if(response != SWT.OK)
                    {
                        return;
                    }
                    
                    synchronized(this)
                    {
                        serverRegistry.clearNotifications(null, _notifications);
                    }
                }

                refresh();
            }
        });
        //add description
        Label desc = _toolkit.createLabel(composite,"Clears the selected Notifications, or all if none are selected");
        desc.setLayoutData(new GridData(SWT.LEFT,SWT.CENTER, false, false));
    }
    
    /**
     * Creates table to display notifications
     */
    private void createTable()
    {
        _table = _toolkit.createTable(_form.getBody(), SWT.MULTI | SWT.FULL_SELECTION);
        _table.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        
        TableColumn column = new TableColumn(_table, SWT.NONE);
        column.setText(_tableTitles[0]);
        column.setWidth(100);
        
        column = new TableColumn(_table, SWT.NONE);
        column.setText(_tableTitles[1]);
        column.setWidth(100); 

        column = new TableColumn(_table, SWT.NONE);
        column.setText(_tableTitles[2]);
        column.setWidth(130);
        
        column = new TableColumn(_table, SWT.NONE);
        column.setText(_tableTitles[3]);
        column.setWidth(100);
        
        column = new TableColumn(_table, SWT.NONE);
        column.setText(_tableTitles[4]);
        column.setWidth(500);
        
        _table.setHeaderVisible(true);
        _table.setLinesVisible(true);
    }
    
    /**
     * Creates JFace viewer for the notifications table
     */
    protected void createTableViewer()
    {
        createTable();
        _tableViewer = new TableViewer(_table);
        _tableViewer.setUseHashlookup(true);
        _tableViewer.setContentProvider(new ContentProviderImpl());
        _tableViewer.setLabelProvider(new LabelProviderImpl());
        _tableViewer.setColumnProperties(_tableTitles);
        
        addTableListeners();
    }
    
    /**
     * Adds listeners to the viewer for displaying notification details 
     */
    protected void addTableListeners()
    {
        _tableViewer.addDoubleClickListener(new IDoubleClickListener()
            {
                Display display = null;
                Shell   shell = null;
                public void doubleClick(DoubleClickEvent event)
                {
                    display = Display.getCurrent();
                    shell = new Shell(display, SWT.BORDER | SWT.CLOSE | SWT.MIN | SWT.MAX | SWT.RESIZE);
                    shell.setText("Notification");
                    shell.setImage(ApplicationRegistry.getImage(CONSOLE_IMAGE));

                    int x = display.getBounds().width;
                    int y = display.getBounds().height;
                    shell.setBounds(x/4, y/4, x/2, y/3);
                    StructuredSelection selection = (StructuredSelection)event.getSelection();
                    createPopupContents((NotificationObject)selection.getFirstElement());
                    shell.open();
                    while (!shell.isDisposed()) {
                        if (!display.readAndDispatch()) {
                            display.sleep();
                        }
                    }
                    
                    //If you create it, you dispose it.
                    shell.dispose();
                }

                private void createPopupContents(NotificationObject obj)
                {                    
                    shell.setLayout(new GridLayout());
                    
                    Composite parent = _toolkit.createComposite(shell, SWT.NONE);
                    parent.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
                    GridLayout layout = new GridLayout(4, true);
                    parent.setLayout(layout);
                    
                    // Object name record
                    Label key = _toolkit.createLabel(parent, COLUMN_OBJ, SWT.TRAIL); 
                    GridData layoutData = new GridData(SWT.TRAIL, SWT.TOP, false, false,1,1);
                    key.setLayoutData(layoutData);
                    Text  value = _toolkit.createText(parent, obj.getSourceName(), SWT.BEGINNING | SWT.BORDER |SWT.READ_ONLY);
                    value.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false,3,1));

                    // Sequence no record
                    key = _toolkit.createLabel(parent, COLUMN_SEQ, SWT.TRAIL);             
                    layoutData = new GridData(SWT.TRAIL, SWT.TOP, false, false,1,1);
                    key.setLayoutData(layoutData);
                    value = _toolkit.createText(parent, ""+obj.getSequenceNo(), SWT.BEGINNING | SWT.BORDER |SWT.READ_ONLY);
                    value.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false,3,1));

                    // Time row
                    key = _toolkit.createLabel(parent, COLUMN_TIME, SWT.TRAIL);
                    key.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, true, false,1,1));
                    value = _toolkit.createText(parent, obj.getTimeStamp(), SWT.BEGINNING | SWT.BORDER | SWT.READ_ONLY);
                    value.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false,3,1));

                    key = _toolkit.createLabel(parent, COLUMN_TYPE, SWT.TRAIL);
                    key.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, true, false,1,1));
                    value = _toolkit.createText(parent, obj.getType(), SWT.BEGINNING | SWT.BORDER | SWT.READ_ONLY);
                    value.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false,3,1));

                    key = _toolkit.createLabel(parent, COLUMN_MSG, SWT.TRAIL);
                    key.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, true, false,1,1));
                    value = _toolkit.createText(parent, obj.getMessage(), SWT.MULTI | SWT.WRAP| SWT.BORDER | SWT.V_SCROLL | SWT.READ_ONLY);
                    GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1);
                    gridData.heightHint = 100;
                    value.setLayoutData(gridData);
                }
            });
    }
    
    public void refresh()
    {        
        if(_workerRunning)
        {
            //perform an single immediate-update
            updateTableViewer();   
        }
        else
        {
            //start a worker to do the update and keep going as required
            _workerRunning = true;
            _worker = new Thread(new Worker()); 
            _worker.start();
        }

        _form.layout(true);   
        _form.getBody().layout(true, true);
    }
    
    /**
     * Content provider class for the table viewer
     */
    protected class ContentProviderImpl implements IStructuredContentProvider, INotificationViewer
    {
        public void inputChanged(Viewer v, Object oldInput, Object newInput)
        {
            
        }
        public void dispose()
        {
            
        }
        public Object[] getElements(Object parent) 
        {
            return _notifications.toArray(new NotificationObject[0]);
        }
        public void addNotification(NotificationObject notification)
        {
            _tableViewer.add(notification);
        }
        
        public void addNotification(List<NotificationObject> notificationList)
        {
            _tableViewer.add(notificationList.toArray(new NotificationObject[0]));
        }
    }
    
    /**
     * Label provider for the table viewer
     */
    protected class LabelProviderImpl implements ITableLabelProvider
    {
        List<ILabelProviderListener> listeners = new ArrayList<ILabelProviderListener>();       
        public void addListener(ILabelProviderListener listener)
        {
            listeners.add(listener);
        }
        
        public void dispose(){
            
        }
        
        public Image getColumnImage(Object element, int columnIndex)
        {
            return null;
        }
        
        public String getColumnText(Object element, int columnIndex)
        {
            String result = null;
            NotificationObject t = (NotificationObject)element;
            switch(columnIndex)
            {
            case 0 : 
                result = t.getSourceName();
                break;
            case 1 : 
                result = String.valueOf(t.getSequenceNo());
                break;
            case 2 :
                result = String.valueOf(t.getTimeStamp());
                break;
            case 3 : 
                result = t.getType();
                break;
            case 4 : 
                result = t.getMessage();
                break;
            default : 
                result = "";
            }
            
            return result;
        }
        
        public boolean isLabelProperty(Object element, String property)
        {
            return false;
        }
        
        public void removeListener(ILabelProviderListener listener)
        {
            listeners.remove(listener);
        }
    } // end of LabelProviderImpl
    
    
    protected void setWorkerRunning(boolean running)
    {
        _workerRunning = running;
    }
    
    /**
     * Worker class which keeps looking if there are new notifications coming from server for the selected mbean
     */
    private class Worker implements Runnable
    {
        private boolean keepGoing = true; 
        
        public void run()
        {
            final Display display = _tabFolder.getDisplay();
            
            while(keepGoing)
            {
                if (display == null || display.isDisposed())
                {
                    setWorkerRunning(false);
                    break; //stop the thread
                }
                
                display.syncExec(new Runnable()
                {
                    public void run()
                    {
                        if (_form == null || _form.isDisposed())
                        {
                            setWorkerRunning(false);
                            keepGoing = false; //exit the loop and stop the thread
                        }
                        else
                        {
                            keepGoing = _form.isVisible();
                            setWorkerRunning(keepGoing);
                        }

                        if (keepGoing)
                        {
                            updateTableViewer();
                        }
                    }
                });
            
                if (keepGoing)
                {
                    sleep();
                }
            }
        }
        
        private void sleep()
        {
            try
            {
                Thread.sleep(2000);
            }
            catch(Exception ex)
            {

            }  
        }
    }
    
    /**
     * Updates the table with new notifications received from mbean server for all mbeans in this virtual host
     */
    protected void updateTableViewer()
    {
        String virtualhost = MBeanView.getVirtualHost();
        
        ServerRegistry serverRegistry = ApplicationRegistry.getServerRegistry(MBeanView.getServer());        
        List<NotificationObject> newList = serverRegistry.getNotifications(virtualhost);
        
        synchronized(this)
        {
            _notifications = newList;
            _tableViewer.setInput(_notifications);
        }
    }

}
