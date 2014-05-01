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
package org.apache.qpid.management.ui.views.exchange;

import static org.apache.qpid.management.ui.Constants.EXCHANGE_TYPE;
import static org.apache.qpid.management.ui.Constants.DEFAULT_EXCHANGE_TYPE_VALUES;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.TabularDataSupport;

import org.apache.qpid.management.ui.ApiVersion;
import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.ServerRegistry;
import org.apache.qpid.management.common.mbeans.ManagedExchange;
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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;


/**
 * Control class for the Exchange mbean Operations tab.
 */
public class ExchangeOperationsTabControl extends TabControl
{
    private FormToolkit _toolkit;
    private ScrolledForm        _form;
    private Table _keysTable = null;
    private TableViewer _keysTableViewer = null;
    private Table _queuesTable = null;
    private TableViewer _queuesTableViewer = null;
    private Composite _paramsComposite = null;
            
    private TabularDataSupport _bindings = null;
    private ManagedExchange _emb;
    private ApiVersion _ApiVersion;
    
    static final String BINDING_KEY = ManagedExchange.COMPOSITE_ITEM_NAMES[0];
    static final String QUEUES = ManagedExchange.COMPOSITE_ITEM_NAMES[1];
    
    public ExchangeOperationsTabControl(TabFolder tabFolder, JMXManagedObject mbean, MBeanServerConnection mbsc)
    {
        super(tabFolder);
        _mbean = mbean;
        _ApiVersion = ApplicationRegistry.getServerRegistry(mbean).getManagementApiVersion();
        _emb = (ManagedExchange) MBeanServerInvocationHandler.newProxyInstance(mbsc, 
                                mbean.getObjectName(), ManagedExchange.class, false);
        _toolkit = new FormToolkit(_tabFolder.getDisplay());
        _form = _toolkit.createScrolledForm(_tabFolder);
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
        _keysTable.setFocus();
    }
    
    @Override
    public void refresh(ManagedBean mbean)
    {
        _bindings = null;
        try
        {
            //gather a list of all keys and queues for display and selection
            _bindings = (TabularDataSupport) _emb.bindings();
        }
        catch (Exception e)
        {
            MBeanUtility.handleException(_mbean,e);
        }

        _keysTableViewer.setInput(_bindings);
        
        //if we have a Qpid JMX API 1.3+ server
        if(_ApiVersion.greaterThanOrEqualTo(1, 3))
        {
            //if it is a fanout exchange
            if(isFanoutExchange())
            {
                //if there are any queue bindings, there is a single wildcard binding key
                //auto-select it to show all the queues bound to the exchange
                if (_keysTable.getItemCount() == 1)
                {
                    _keysTable.setSelection(0);
                    updateQueuesTable();
                }
            }
        }

        layout();
    }
    
    public void layout()
    {
        _form.layout(true);
        _form.getBody().layout(true, true);
    }
    
    private void createWidgets()
    {
        Group bindingsGroup = new Group(_paramsComposite, SWT.SHADOW_NONE);
        bindingsGroup.setBackground(_paramsComposite.getBackground());
        bindingsGroup.setText("Bindings");
        bindingsGroup.setLayout(new GridLayout(2,false));
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        bindingsGroup.setLayoutData(gridData);
        
        Composite tablesComposite = _toolkit.createComposite(bindingsGroup);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.minimumHeight = 250;
        gridData.heightHint = 250;
        tablesComposite.setLayoutData(gridData);
        tablesComposite.setLayout(new GridLayout(2,false));
        
        _keysTable = new Table (tablesComposite, SWT.SINGLE | SWT.SCROLL_LINE | SWT.BORDER | SWT.FULL_SELECTION);
        _keysTable.setLinesVisible(true);
        _keysTable.setHeaderVisible(true);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        _keysTable.setLayoutData(data);
        
        _keysTableViewer = new TableViewer(_keysTable);
        final TableSorter tableSorter = new TableSorter(BINDING_KEY);
        
        String[] titles = {"Binding Key"};
        int[] bounds = {200};
        for (int i = 0; i < titles.length; i++) 
        {
            final int index = i;
            final TableColumn column = new TableColumn (_keysTable, SWT.NONE);

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
                    final TableViewer viewer = _keysTableViewer;
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
        
        _keysTableViewer.setContentProvider(new ContentProviderImpl(BINDING_KEY));
        _keysTableViewer.setLabelProvider(new LabelProviderImpl(BINDING_KEY));
        _keysTableViewer.setSorter(tableSorter);
        _keysTable.setSortColumn(_keysTable.getColumn(0));
        _keysTable.setSortDirection(SWT.UP);
        
        
        _queuesTable = new Table (tablesComposite, SWT.SINGLE | SWT.SCROLL_LINE | SWT.BORDER | SWT.FULL_SELECTION);
        _queuesTable.setLinesVisible (true);
        _queuesTable.setHeaderVisible (true);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        _queuesTable.setLayoutData(data);
        
        _queuesTableViewer = new TableViewer(_queuesTable);
        final TableSorter queuesTableSorter = new TableSorter(QUEUES);
        
        titles = new String[]{"Queue Names"};
        bounds = new int[]{225};
        for (int i = 0; i < titles.length; i++) 
        {
            final int index = i;
            final TableColumn column = new TableColumn (_queuesTable, SWT.NONE);

            column.setText(titles[i]);
            column.setWidth(bounds[i]);
            column.setResizable(true);

            //Setting the right sorter
            column.addSelectionListener(new SelectionAdapter() 
            {
                @Override
                public void widgetSelected(SelectionEvent e) 
                {
                	queuesTableSorter.setColumn(index);
                    final TableViewer viewer = _queuesTableViewer;
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
        
        _queuesTableViewer.setContentProvider(new ContentProviderImpl(QUEUES));
        _queuesTableViewer.setLabelProvider(new LabelProviderImpl(QUEUES));
        _queuesTableViewer.setSorter(queuesTableSorter);
        _queuesTable.setSortColumn(_queuesTable.getColumn(0));
        _queuesTable.setSortDirection(SWT.UP);
        _queuesTableViewer.setInput(new String[]{"Select a binding key to view queues"});
        
        //listener for double clicking to open the selection mbean
        _queuesTable.addMouseListener(new MouseListener()                                              
        {
            // MouseListener implementation
            public void mouseDoubleClick(MouseEvent event)
            {
                openMBean(_queuesTable);
            }
            
            public void mouseDown(MouseEvent e){}
            public void mouseUp(MouseEvent e){}
        });
        
        _keysTableViewer.addSelectionChangedListener(new ISelectionChangedListener(){
            public void selectionChanged(SelectionChangedEvent evt)
            {
                updateQueuesTable();
            }
        });
        
        //Side Buttons
        Composite buttonsComposite = _toolkit.createComposite(bindingsGroup);
        gridData = new GridData(SWT.FILL, SWT.FILL, false, true);
        buttonsComposite.setLayoutData(gridData);
        buttonsComposite.setLayout(new GridLayout());

        final Button createBindingButton = _toolkit.createButton(buttonsComposite, "Create ...", SWT.PUSH);
        createBindingButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                createNewBinding(createBindingButton.getShell());
            }
        });
        
    }

    private void updateQueuesTable()
    {
        int selectionIndex = _keysTable.getSelectionIndex();

        if (selectionIndex != -1)
        {
            final CompositeData selectedMsg = (CompositeData)_keysTable.getItem(selectionIndex).getData();

            String[] queues = (String[]) selectedMsg.get(QUEUES);
            _queuesTableViewer.setInput(queues);
        }
        else
        {
            _queuesTableViewer.setInput(new String[]{"Select a binding key to view queues"});
        }
    }
    
    private boolean isFanoutExchange()
    {
        return _mbean.getProperty(EXCHANGE_TYPE).equalsIgnoreCase(DEFAULT_EXCHANGE_TYPE_VALUES[1]);

    }
    
    /**
     * Content Provider class for the table viewer
     */
    private class ContentProviderImpl implements IStructuredContentProvider
    {
    	String type;
    	
    	public ContentProviderImpl(String type)
    	{
    		this.type = type;
    	}

        public void inputChanged(Viewer v, Object oldInput, Object newInput)
        {
            
        }
        
        public void dispose()
        {
            
        }
        
        public Object[] getElements(Object parent)
        {
        	if(type.equals(BINDING_KEY))
        	{
        		Collection<Object> rowCollection = ((TabularDataSupport) parent).values();

        		return rowCollection.toArray();
        	}
        	else
        	{
        		//we have the list of queues, return directly
        		return (String[]) parent;
        	}
        }
    }
    
    /**
     * Label Provider class for the routing key table viewer
     */
    private class LabelProviderImpl extends LabelProvider implements ITableLabelProvider
    {
    	String type;
    	
    	public LabelProviderImpl(String type)
    	{
    		this.type = type;
    	}
    	
        @Override
        public String getColumnText(Object element, int columnIndex)
        {
        	if(type.equals(BINDING_KEY)) //binding num and queue name table
        	{
        		switch (columnIndex)
        		{
        			case 0 : // key column 
        				return String.valueOf(((CompositeDataSupport) element).get(BINDING_KEY));
        			default :
        				return "";
        		}
        	}
        	else //binding key-value pair table
        	{
        		switch (columnIndex)
        		{
        			case 0 : //queue name column 
        				return String.valueOf(element);
        			default :
        				return "";
        		}
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
        
    	private String type;
    	
        public TableSorter(String type)
        {
        	this.type = type;
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
            int comparison = 0;
            
            if(type.equals(BINDING_KEY))//binding num and queue name table
            {
                CompositeData binding1 = (CompositeData) e1;
                CompositeData binding2 = (CompositeData) e2;
                
                switch(column)
                {                   
                    case 0:
                        comparison = ((String) binding1.get(BINDING_KEY)).compareTo((String) binding2.get(BINDING_KEY));
                        break;
                    default:
                        comparison = 0;
                }
            }
            else //binding key-value pair table
            {
                switch(column)
                {
                    case 0:
                        comparison = ((String)e1).compareTo((String) e2);
                        break;
                    default:
                        comparison = 0;
                }
            }
            
            
            // If descending order, flip the direction
            if(direction == DESCENDING)
            {
                comparison = -comparison;
            }
            return comparison;
        }
    }
    
    private void createNewBinding(Shell parent)
    {
        final Shell shell = ViewUtility.createModalDialogShell(parent, "Create New Binding");

        Composite destinationComposite = _toolkit.createComposite(shell, SWT.NONE);
        destinationComposite.setBackground(shell.getBackground());
        destinationComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        destinationComposite.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(destinationComposite,"Queue:").setBackground(shell.getBackground());
        final Combo destinationCombo = new Combo(destinationComposite,SWT.NONE | SWT.READ_ONLY);
        destinationCombo.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        Composite bindingComposite = _toolkit.createComposite(shell, SWT.NONE);
        bindingComposite.setBackground(shell.getBackground());
        bindingComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        bindingComposite.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(bindingComposite,"Binding:").setBackground(shell.getBackground());
        final Text bindingText = new Text(bindingComposite, SWT.BORDER);
        bindingText.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        if(isFanoutExchange())
        {
            bindingText.setText("*");
        }

        Composite okCancelButtonsComp = _toolkit.createComposite(shell);
        okCancelButtonsComp.setBackground(shell.getBackground());
        okCancelButtonsComp.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, true, true));
        okCancelButtonsComp.setLayout(new GridLayout(2,false));
        
        Button okButton = _toolkit.createButton(okCancelButtonsComp, "OK", SWT.PUSH);
        okButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));
        Button cancelButton = _toolkit.createButton(okCancelButtonsComp, "Cancel", SWT.PUSH);
        cancelButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));
        
        List<String> queueList = ApplicationRegistry.getServerRegistry(_mbean).getQueueNames(_mbean.getVirtualHostName());
        
        if(queueList.size() == 0)
        {
            destinationCombo.setItems(new String[]{"No queues available"});
            okButton.setEnabled(false);
        }
        else
        {
            Collections.sort(queueList);
            destinationCombo.setItems(queueList.toArray(new String[0]));
        }
        destinationCombo.select(0);
        
        okButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                String binding = bindingText.getText();
                
                if (!isFanoutExchange() && (binding == null || binding.length() == 0))
                {                            
                    ViewUtility.popupErrorMessage("Create New Binding", "Please enter a valid binding");
                    return;
                }
                
                String destQueue = destinationCombo.getItem(destinationCombo.getSelectionIndex()).toString();
                
                shell.dispose();

                try
                {
                    _emb.createNewBinding(destQueue, binding);
                    ViewUtility.operationResultFeedback(null, "Created new Binding", null);
                }
                catch (Exception e4)
                {
                    ViewUtility.operationFailedStatusBarMessage("Error creating new Binding");
                    MBeanUtility.handleException(_mbean, e4);
                }

                refresh(_mbean);
            }
        });

        cancelButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                shell.dispose();
            }
        });

        shell.setDefaultButton(okButton);
        shell.pack();
        ViewUtility.centerChildInParentShell(parent, shell);
        
        shell.open();
    }
    
    private void openMBean(Table table)
    {
        int selectionIndex = table.getSelectionIndex();

        if (selectionIndex == -1)
        {
            return;
        }
        
        String queueName = (String) table.getItem(selectionIndex).getData();
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
