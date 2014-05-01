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
package org.apache.qpid.management.ui.views.connection;

import java.util.Collection;

import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.TabularDataSupport;

import org.apache.qpid.management.ui.ApiVersion;
import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.ServerRegistry;
import org.apache.qpid.management.common.mbeans.ManagedConnection;
import org.apache.qpid.management.ui.jmx.JMXManagedObject;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.views.MBeanView;
import org.apache.qpid.management.ui.views.TabControl;
import org.apache.qpid.management.ui.views.ViewUtility;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;


/**
 * Control class for the Connection mbean Operations tab.
 */
public class ConnectionOperationsTabControl extends TabControl
{
    private FormToolkit _toolkit;
    private Form        _form;
    private Table _table = null;
    private TableViewer _tableViewer = null;
    private Composite _paramsComposite = null;
            
    private TabularDataSupport _channels = null;
    private ManagedConnection _cmb;
    private ApiVersion _ApiVersion;
    
    static final String CHAN_ID = ManagedConnection.COMPOSITE_ITEM_NAMES[0];
    static final String TRANSACTIONAL = ManagedConnection.COMPOSITE_ITEM_NAMES[1];
    static final String DEFAULT_QUEUE = ManagedConnection.COMPOSITE_ITEM_NAMES[2];
    static final String UNACKED_COUNT = ManagedConnection.COMPOSITE_ITEM_NAMES[3];
    static final String FLOW_BLOCKED = ManagedConnection.COMPOSITE_ITEM_NAMES[4];
    
    public ConnectionOperationsTabControl(TabFolder tabFolder, JMXManagedObject mbean, MBeanServerConnection mbsc)
    {
        super(tabFolder);
        _mbean = mbean;
        _ApiVersion = ApplicationRegistry.getServerRegistry(mbean).getManagementApiVersion();
        _cmb = (ManagedConnection) MBeanServerInvocationHandler.newProxyInstance(mbsc, 
                                mbean.getObjectName(), ManagedConnection.class, false);
        _toolkit = new FormToolkit(_tabFolder.getDisplay());
        _form = _toolkit.createForm(_tabFolder);
        _form.getBody().setLayout(new GridLayout());
        createComposites();
        createWidgets();
    }
    
    private void createComposites()
    {
        _paramsComposite = _toolkit.createComposite(_form.getBody(), SWT.NONE);
        _paramsComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        _paramsComposite.setLayout(new GridLayout());
    }
    
    /**
     * @see TabControl#getControl()
     */
    public Control getControl()
    {
        return _form;
    }
    
    /**
     * @see TabControl#setFocus()
     */
    public void setFocus()
    {
        _table.setFocus();
    }
    
    @Override
    public void refresh(ManagedBean mbean)
    {
        _channels = null;
        try
        {
            //gather a list of all channels on the connection for display and selection
            _channels = (TabularDataSupport) _cmb.channels();
        }
        catch (Exception e)
        {
            MBeanUtility.handleException(mbean,e);
        }
        
        _tableViewer.setInput(_channels);

        layout();
    }
    
    public void layout()
    {
        _form.layout(true);
        _form.getBody().layout(true, true);
    }
    
    private void createWidgets()
    {
        Group viewChannelsGroup = new Group(_paramsComposite, SWT.SHADOW_NONE);
        viewChannelsGroup.setBackground(_paramsComposite.getBackground());
        viewChannelsGroup.setText("Channels");
        viewChannelsGroup.setLayout(new GridLayout());
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.heightHint = 250;
        gridData.minimumHeight = 250;
        
        viewChannelsGroup.setLayoutData(gridData);
               
        _table = new Table (viewChannelsGroup, SWT.SINGLE | SWT.SCROLL_LINE | SWT.BORDER | SWT.FULL_SELECTION);
        _table.setLinesVisible (true);
        _table.setHeaderVisible (true);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        _table.setLayoutData(data);
        
        _tableViewer = new TableViewer(_table);
        final TableSorter tableSorter = new TableSorter();
      
        String[] titles;
        if(_ApiVersion.greaterThanOrEqualTo(1, 5))
        {
            titles = new String[]{"Id", "Transactional", "Num Unacked Msg", "Default Queue", "Flow Blocked"};
        }
        else
        {
            titles = new String[]{"Id", "Transactional", "Num Unacked Msg", "Default Queue"};
        }
        int[] bounds = { 50, 110, 145, 200, 110 };
        for (int i = 0; i < titles.length; i++) 
        {
            final int index = i;
            final TableColumn column = new TableColumn (_table, SWT.NONE);

            column.setText(titles[i]);
            column.setWidth(bounds[i]);
            column.setResizable(true);

            //Setting the right sorter
            column.addSelectionListener(new SelectionAdapter() 
            {
                @Override
                public void widgetSelected(SelectionEvent e) 
                {
                    tableSorter.setColumn(index);
                    final TableViewer viewer = _tableViewer;
                    int dir = viewer .getTable().getSortDirection();
                    if (viewer.getTable().getSortColumn() == column) 
                    {
                        dir = dir == SWT.UP ? SWT.DOWN : SWT.UP;
                    } 
                    else 
                    {
                        dir = SWT.UP;
                    }
                    viewer.getTable().setSortDirection(dir);
                    viewer.getTable().setSortColumn(column);
                    viewer.refresh();
                }
            });

        }
        
        _tableViewer.setContentProvider(new ContentProviderImpl());
        _tableViewer.setLabelProvider(new LabelProviderImpl());
        _tableViewer.setSorter(tableSorter);
        _table.setSortColumn(_table.getColumn(0));
        _table.setSortDirection(SWT.UP);
        
        Composite buttonsComposite = _toolkit.createComposite(viewChannelsGroup);
        gridData = new GridData(SWT.RIGHT, SWT.BOTTOM, false, false);
        buttonsComposite.setLayoutData(gridData);
        buttonsComposite.setLayout(new GridLayout(2,false));
        
        final Button commitButton = _toolkit.createButton(buttonsComposite, "Commit Transactions", SWT.PUSH);
        commitButton.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));
        commitButton.setEnabled(false);
        commitButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                int selectionIndex = _table.getSelectionIndex();

                if (selectionIndex != -1)
                {
                    final CompositeData selectedChannel = (CompositeData)_table.getItem(selectionIndex).getData();
                    Integer id = (Integer) selectedChannel.get(CHAN_ID);

                    int response = ViewUtility.popupOkCancelConfirmationMessage("Commit Transactions", 
                                                        "Commit transactions for channel:" + id + " ?");

                    if (response == SWT.OK)
                    {
                        try
                        {
                            _cmb.commitTransactions(id);
                            ViewUtility.operationResultFeedback(null, "Commited transactions", null);
                        }
                        catch (Exception e1)
                        {
                            ViewUtility.operationFailedStatusBarMessage("Error commiting transactions");
                            MBeanUtility.handleException(_mbean, e1);
                        }

                        refresh(_mbean);;
                    }
                }
            }
        });
        
        final Button rollbackButton = _toolkit.createButton(buttonsComposite, "Rollback Transactions", SWT.PUSH);
        rollbackButton.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));
        rollbackButton.setEnabled(false);
        rollbackButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                int selectionIndex = _table.getSelectionIndex();

                if (selectionIndex != -1)
                {
                    final CompositeData selectedChannel = (CompositeData)_table.getItem(selectionIndex).getData();
                    Integer id = (Integer) selectedChannel.get(CHAN_ID);

                    int response = ViewUtility.popupOkCancelConfirmationMessage("Rollback Transactions", 
                            "Rollback transactions for channel:" + id + " ?");

                    if (response == SWT.OK)
                    {
                        try
                        {
                            _cmb.rollbackTransactions(id);
                            ViewUtility.operationResultFeedback(null, "Rolled back transactions", null);
                        }
                        catch (Exception e1)
                        {
                            ViewUtility.operationFailedStatusBarMessage("Error rolling back transactions");
                            MBeanUtility.handleException(_mbean, e1);
                        }

                        refresh(_mbean);;
                    }
                }

            }
        });
        
        //listener for double clicking to open the selection mbean
        _table.addMouseListener(new MouseListener()                                              
        {
            // MouseListener implementation
            public void mouseDoubleClick(MouseEvent event)
            {
                openMBean(_table);
            }
            
            public void mouseDown(MouseEvent e){}
            public void mouseUp(MouseEvent e){}
        });
        
        _tableViewer.addSelectionChangedListener(new ISelectionChangedListener(){
            public void selectionChanged(SelectionChangedEvent evt)
            {
                int selectionIndex = _table.getSelectionIndex();

                if (selectionIndex != -1)
                {
                    final CompositeData selectedChannel = (CompositeData)_table.getItem(selectionIndex).getData();
                    Boolean transactional = (Boolean) selectedChannel.get(TRANSACTIONAL);
                    
                    if(transactional)
                    {
                        rollbackButton.setEnabled(true);
                        commitButton.setEnabled(true);
                    }
                    else
                    {
                        rollbackButton.setEnabled(false);
                        commitButton.setEnabled(false);
                    }
                }
                else
                {
                    rollbackButton.setEnabled(false);
                    commitButton.setEnabled(true);
                }
            }
        });
        
        Composite opsComposite = _toolkit.createComposite(_paramsComposite);
        gridData = new GridData(SWT.LEFT, SWT.FILL, false, true);
        opsComposite.setLayoutData(gridData);
        opsComposite.setLayout(new GridLayout(3,false));
        
        Group closeConnectionGroup = new Group(opsComposite, SWT.SHADOW_NONE);
        closeConnectionGroup.setBackground(opsComposite.getBackground());
        closeConnectionGroup.setText("Close Connection");
        gridData = new GridData(SWT.LEFT, SWT.TOP, true, false);
        closeConnectionGroup.setLayoutData(gridData);
        closeConnectionGroup.setLayout(new GridLayout());
        
        final Button closeConnectionButton = _toolkit.createButton(closeConnectionGroup, "Close Connection", SWT.PUSH);
        closeConnectionButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                int response = ViewUtility.popupOkCancelConfirmationMessage("Close Connection", 
                        "Are you sure you wish to close the Connection?");

                if (response == SWT.OK)
                {
                    try
                    {
                        _cmb.closeConnection();
                    }
                    catch (Exception e1)
                    {
                        MBeanUtility.handleException(_mbean, e1);
                    }
                }
            }
        });
    }
    
    
    /**
     * Content Provider class for the table viewer
     */
    private class ContentProviderImpl  implements IStructuredContentProvider
    {
        
        public void inputChanged(Viewer v, Object oldInput, Object newInput)
        {
            
        }
        
        public void dispose()
        {
            
        }
        
        public Object[] getElements(Object parent)
        {
            Collection<Object> rowCollection = ((TabularDataSupport) parent).values();
           
            return rowCollection.toArray();
        }
    }
    
    /**
     * Label Provider class for the table viewer
     */
    private class LabelProviderImpl extends LabelProvider implements ITableLabelProvider
    {
        @Override
        public String getColumnText(Object element, int columnIndex)
        {
            switch (columnIndex)
            {
                case 0 : // id column 
                    return String.valueOf(((CompositeDataSupport) element).get(CHAN_ID));
                case 1 : // transactional column 
                    return String.valueOf(((CompositeDataSupport) element).get(TRANSACTIONAL));
                case 2 : // num unacked msgs column 
                    return String.valueOf(((CompositeDataSupport) element).get(UNACKED_COUNT));
                case 3 : // default queue column 
                    return String.valueOf(((CompositeDataSupport) element).get(DEFAULT_QUEUE));
                case 4 : // flow blocked column 
                    return String.valueOf(((CompositeDataSupport) element).get(FLOW_BLOCKED));
                default :
                    return "-";
            }
        }
        
        @Override
        public Image getColumnImage(Object element, int columnIndex)
        {
            return null;
        }
        
    }

    /**
     * Sorter class for the table viewer.
     *
     */
    public class TableSorter extends ViewerSorter
    {
        private int column;
        private static final int ASCENDING = 0;
        private static final int DESCENDING = 1;

        private int direction = DESCENDING;

        public TableSorter()
        {
            this.column = 0;
            direction = ASCENDING;
        }

        public void setColumn(int column)
        {
            if (column == this.column)
            {
                // Same column as last sort; toggle the direction
                direction = 1 - direction;
            }
            else
            {
                // New column; do an ascending sort
                this.column = column;
                direction = ASCENDING;
            }
        }

        @Override
        public int compare(Viewer viewer, Object e1, Object e2)
        {
            CompositeData chan1 = (CompositeData) e1;
            CompositeData chan2 = (CompositeData) e2;
            
            int comparison = 0;
            switch(column)
            {
                case 0:
                    comparison = ((Integer) chan1.get(CHAN_ID)).compareTo((Integer)chan2.get(CHAN_ID));
                    break;
                case 1:
                    comparison = String.valueOf(chan1.get(TRANSACTIONAL)).compareTo(
                                                        String.valueOf(chan2.get(TRANSACTIONAL)));
                    break;
                case 2:
                    comparison = ((Long) chan1.get(UNACKED_COUNT)).compareTo((Long)chan2.get(UNACKED_COUNT));
                    break;
                case 3:
                    comparison = String.valueOf(chan1.get(DEFAULT_QUEUE)).compareTo(
                                                        String.valueOf(chan2.get(DEFAULT_QUEUE)));
                    break;
                case 4:
                    comparison = String.valueOf(chan1.get(FLOW_BLOCKED)).compareTo(
                                                        String.valueOf(chan2.get(FLOW_BLOCKED)));
                    break;
                default:
                    comparison = 0;
            }
            // If descending order, flip the direction
            if(direction == DESCENDING)
            {
                comparison = -comparison;
            }
            return comparison;
        }
    }
    
    private void openMBean(Table table)
    {
        int selectionIndex = table.getSelectionIndex();

        if (selectionIndex == -1)
        {
            return;
        }
        
        CompositeData channelResult = (CompositeData) table.getItem(selectionIndex).getData();
        String queueName = (String) channelResult.get(DEFAULT_QUEUE);
        
        if(queueName == null)
        {
            return;
        }
        
        ServerRegistry serverRegistry = ApplicationRegistry.getServerRegistry(_mbean);
        ManagedBean selectedMBean = serverRegistry.getQueue(queueName, _mbean.getVirtualHostName());

        if(selectedMBean == null)
        {
            ViewUtility.popupErrorMessage("Error", "Unable to retrieve the selected MBean to open it");
            return;
        }

        IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow(); 
        MBeanView view = (MBeanView) window.getActivePage().findView(MBeanView.ID);
        try
        {
            view.openMBean(selectedMBean);
        }
        catch (Exception ex)
        {
            MBeanUtility.handleException(selectedMBean, ex);
        }
    }

}
