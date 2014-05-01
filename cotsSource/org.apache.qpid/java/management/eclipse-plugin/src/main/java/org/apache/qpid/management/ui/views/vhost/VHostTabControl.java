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
package org.apache.qpid.management.ui.views.vhost;

import static org.apache.qpid.management.ui.Constants.DEFAULT_EXCHANGE_TYPE_VALUES;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;

import org.apache.qpid.management.ui.ApiVersion;
import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.ServerRegistry;
import org.apache.qpid.management.common.mbeans.ManagedBroker;
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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;


/**
 * Control class for the VirtualHostManager mbean operations tab.
 */
public class VHostTabControl extends TabControl
{
    private FormToolkit _toolkit;
    private Form        _form;
    private Table _queueTable = null;
    private TableViewer _queueTableViewer = null;
    private Table _exchangeTable = null;
    private TableViewer _exchangeTableViewer = null;
    
    private Composite _paramsComposite = null;

    private ManagedBroker _vhmb;
    private ApiVersion _ApiVersion;
    private List<ManagedBean> _queues;
    private List<ManagedBean> _exchanges;
    private ServerRegistry _serverRegistry;

    public VHostTabControl(TabFolder tabFolder, JMXManagedObject mbean, MBeanServerConnection mbsc)
    {
        super(tabFolder);
        _mbean = mbean;
        _serverRegistry = ApplicationRegistry.getServerRegistry(mbean);
        _ApiVersion = _serverRegistry.getManagementApiVersion();
        _vhmb = (ManagedBroker) MBeanServerInvocationHandler.newProxyInstance(mbsc, 
                                mbean.getObjectName(), ManagedBroker.class, false);
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
        _paramsComposite.setLayout(new GridLayout(2, true));
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

    }
    
    @Override
    public void refresh(ManagedBean mbean)
    {
        ServerRegistry serverRegistry = ApplicationRegistry.getServerRegistry(MBeanView.getServer());
        _queues = serverRegistry.getQueues(MBeanView.getVirtualHost());
        _exchanges = serverRegistry.getExchanges(MBeanView.getVirtualHost());

        _queueTableViewer.setInput(_queues);
        _exchangeTableViewer.setInput(_exchanges);
        
        layout();
    }
    
    public void layout()
    {
        _form.layout(true);
        _form.getBody().layout(true, true);
    }
    
    private void createWidgets()
    {
        Group queuesGroup = new Group(_paramsComposite, SWT.SHADOW_NONE);
        queuesGroup.setBackground(_paramsComposite.getBackground());
        queuesGroup.setText("Queues");
        queuesGroup.setLayout(new GridLayout(2,false));
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        queuesGroup.setLayoutData(gridData);
               
        _queueTable = new Table (queuesGroup, SWT.MULTI | SWT.SCROLL_LINE | SWT.BORDER | SWT.FULL_SELECTION);
        _queueTable.setLinesVisible (true);
        _queueTable.setHeaderVisible (true);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        _queueTable.setLayoutData(data);
        
        _queueTableViewer = new TableViewer(_queueTable);
        final TableSorter tableSorter = new TableSorter();
        
        String[] titles = {"Name"};
        int[] bounds = { 250 };
        for (int i = 0; i < titles.length; i++) 
        {
            final int index = i;
            final TableColumn column = new TableColumn (_queueTable, SWT.NONE);

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
                    final TableViewer viewer = _queueTableViewer;
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
        
        _queueTableViewer.setContentProvider(new ContentProviderImpl());
        _queueTableViewer.setLabelProvider(new LabelProviderImpl());
        _queueTableViewer.setSorter(tableSorter);
        _queueTable.setSortColumn(_queueTable.getColumn(0));
        _queueTable.setSortDirection(SWT.UP);
        
        Composite queuesRightComposite = _toolkit.createComposite(queuesGroup);
        gridData = new GridData(SWT.FILL, SWT.FILL, false, true);
        queuesRightComposite.setLayoutData(gridData);
        queuesRightComposite.setLayout(new GridLayout());
        
        final Button createQueueButton = _toolkit.createButton(queuesRightComposite, "Create ...", SWT.PUSH);
        createQueueButton.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));
        createQueueButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                createQueue(createQueueButton.getShell());
            }
        });
        
        final Button deleteQueueButton = _toolkit.createButton(queuesRightComposite, "Delete", SWT.PUSH);
        deleteQueueButton.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        deleteQueueButton.setEnabled(false);
        deleteQueueButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                deleteQueuesOrExchanges(deleteQueueButton.getShell(), VhostOperations.DELETE_QUEUE);
            }
        });
  
        _queueTableViewer.addSelectionChangedListener(new ISelectionChangedListener(){
            public void selectionChanged(SelectionChangedEvent evt)
            {
                int selectionIndex = _queueTable.getSelectionIndex();

                if (selectionIndex != -1)
                {
                    deleteQueueButton.setEnabled(true);
                }
                else
                {
                    deleteQueueButton.setEnabled(false);
                }
            }
        });

        //listener for double clicking to open the selection mbean
        _queueTable.addMouseListener(new MouseListener()                                              
        {
            // MouseListener implementation
            public void mouseDoubleClick(MouseEvent event)
            {
                openMBean(_queueTable);
            }
            
            public void mouseDown(MouseEvent e){}
            public void mouseUp(MouseEvent e){}
        });
        
        Group exchangesGroup = new Group(_paramsComposite, SWT.SHADOW_NONE);
        exchangesGroup.setBackground(_paramsComposite.getBackground());
        exchangesGroup.setText("Exchanges");
        exchangesGroup.setLayout(new GridLayout(2,false));
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        exchangesGroup.setLayoutData(gridData);
               
        _exchangeTable = new Table (exchangesGroup, SWT.MULTI | SWT.SCROLL_LINE | SWT.BORDER | SWT.FULL_SELECTION);
        _exchangeTable.setLinesVisible (true);
        _exchangeTable.setHeaderVisible (true);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        _exchangeTable.setLayoutData(data);
        
        _exchangeTableViewer = new TableViewer(_exchangeTable);
        final TableSorter exchangeTableSorter = new TableSorter();
        
        for (int i = 0; i < titles.length; i++) 
        {
            final int index = i;
            final TableColumn column = new TableColumn (_exchangeTable, SWT.NONE);

            column.setText(titles[i]);
            column.setWidth(bounds[i]);
            column.setResizable(true);

            //Setting the right sorter
            column.addSelectionListener(new SelectionAdapter() 
            {
                @Override
                public void widgetSelected(SelectionEvent e) 
                {
                    exchangeTableSorter.setColumn(index);
                    final TableViewer viewer = _exchangeTableViewer;
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
        
        _exchangeTableViewer.setContentProvider(new ContentProviderImpl());
        _exchangeTableViewer.setLabelProvider(new LabelProviderImpl());
        _exchangeTableViewer.setSorter(exchangeTableSorter);
        _exchangeTable.setSortColumn(_exchangeTable.getColumn(0));
        _exchangeTable.setSortDirection(SWT.UP);
        
        Composite exchangesRightComposite = _toolkit.createComposite(exchangesGroup);
        gridData = new GridData(SWT.FILL, SWT.FILL, false, true);
        exchangesRightComposite.setLayoutData(gridData);
        exchangesRightComposite.setLayout(new GridLayout());
        
        final Button createExchangeButton = _toolkit.createButton(exchangesRightComposite, "Create ...", SWT.PUSH);
        createExchangeButton.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));
        createExchangeButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                createExchange(createQueueButton.getShell());
            }
        });
        
        final Button deleteExchangeButton = _toolkit.createButton(exchangesRightComposite, "Delete", SWT.PUSH);
        deleteExchangeButton.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        deleteExchangeButton.setEnabled(false);
        deleteExchangeButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                deleteQueuesOrExchanges(deleteExchangeButton.getShell(), VhostOperations.DELETE_EXCHANGE);
            }
        });
  
        _exchangeTableViewer.addSelectionChangedListener(new ISelectionChangedListener(){
            public void selectionChanged(SelectionChangedEvent evt)
            {
                int selectionIndex = _exchangeTable.getSelectionIndex();

                if (selectionIndex != -1)
                {
                    deleteExchangeButton.setEnabled(true);
                }
                else
                {
                    deleteExchangeButton.setEnabled(false);
                }
            }
        });
        
        //listener for double clicking to open the selection mbean
        _exchangeTable.addMouseListener(new MouseListener()                                              
        {
            // MouseListener implementation
            public void mouseDoubleClick(MouseEvent event)
            {
                openMBean(_exchangeTable);
            }
            
            public void mouseDown(MouseEvent e){}
            public void mouseUp(MouseEvent e){}
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
        
        @SuppressWarnings("unchecked")
        public Object[] getElements(Object parent)
        {
            return ((List<ManagedBean>) parent).toArray();
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
                case 0 : // name column 
                    return ((ManagedBean) element).getName();
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
            ManagedBean mbean1 = (ManagedBean ) e1;
            ManagedBean mbean2 = (ManagedBean ) e2;
            
            int comparison = 0;
            switch(column)
            {
                case 0:
                    comparison = mbean1.getName().compareTo(mbean2.getName());
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
    
    private void createQueue(final Shell parent)
    {
        final Shell shell = ViewUtility.createModalDialogShell(parent, "Create Queue");
        
        Composite nameComposite = _toolkit.createComposite(shell, SWT.NONE);
        nameComposite.setBackground(shell.getBackground());
        nameComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        nameComposite.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(nameComposite,"Name:").setBackground(shell.getBackground());
        final Text nameText = new Text(nameComposite, SWT.BORDER);
        nameText.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        Composite ownerComposite = _toolkit.createComposite(shell, SWT.NONE);
        ownerComposite.setBackground(shell.getBackground());
        ownerComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        ownerComposite.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(ownerComposite,"Owner (optional):").setBackground(shell.getBackground());
        final Text ownerText = new Text(ownerComposite, SWT.BORDER);
        ownerText.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        Composite durableComposite = _toolkit.createComposite(shell, SWT.NONE);
        durableComposite.setBackground(shell.getBackground());
        GridData gridData = new GridData(SWT.FILL, SWT.TOP, true, false);
        gridData.minimumWidth = 220;
        durableComposite.setLayoutData(gridData);
        durableComposite.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(durableComposite,"Durable:").setBackground(shell.getBackground());
        final Button durableButton = new Button(durableComposite, SWT.CHECK);
        durableButton.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, true, false));

        Composite okCancelButtonsComp = _toolkit.createComposite(shell);
        okCancelButtonsComp.setBackground(shell.getBackground());
        okCancelButtonsComp.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, true, true));
        okCancelButtonsComp.setLayout(new GridLayout(2,false));
        
        Button okButton = _toolkit.createButton(okCancelButtonsComp, "OK", SWT.PUSH);
        okButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));
        Button cancelButton = _toolkit.createButton(okCancelButtonsComp, "Cancel", SWT.PUSH);
        cancelButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));

        okButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                String name = nameText.getText();
                
                if (name == null || name.length() == 0)
                {                            
                    ViewUtility.popupErrorMessage("Create Queue", "Please enter a valid name");
                    return;
                }
                
                String owner = ownerText.getText();
                
                if (owner != null && owner.length() == 0)
                {                            
                    owner = null;
                }
                
                boolean durable = durableButton.getSelection();
                
                shell.dispose();
                
                try
                {
                    _vhmb.createNewQueue(name, owner, durable);
                    
                    ViewUtility.operationResultFeedback(null, "Created Queue", null);
                    try
                    {   
                        //delay to allow mbean registration notification processing
                        Thread.sleep(250); 
                    }
                    catch(InterruptedException ie)
                    {
                        //ignore
                    }
                }
                catch(Exception e5)
                {
                    ViewUtility.operationFailedStatusBarMessage("Error creating Queue");
                    MBeanUtility.handleException(_mbean, e5);
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
    
    private void createExchange(final Shell parent)
    {
        final Shell shell = ViewUtility.createModalDialogShell(parent, "Create Exchange");

        Composite nameComposite = _toolkit.createComposite(shell, SWT.NONE);
        nameComposite.setBackground(shell.getBackground());
        nameComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        nameComposite.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(nameComposite,"Name:").setBackground(shell.getBackground());
        final Text nameText = new Text(nameComposite, SWT.BORDER);
        nameText.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        Composite typeComposite = _toolkit.createComposite(shell, SWT.NONE);
        typeComposite.setBackground(shell.getBackground());
        typeComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        typeComposite.setLayout(new GridLayout(2,false));
        
        String[] exchangeTypes;
        if(_ApiVersion.greaterThanOrEqualTo(1, 3))//if the server supports Qpid JMX API 1.3
        {//request the current exchange types from the broker
            try
            {
                exchangeTypes = _vhmb.getExchangeTypes();
            }
            catch (IOException e1)
            {
                exchangeTypes = DEFAULT_EXCHANGE_TYPE_VALUES;
            }
        }
        else //use the fallback defaults.
        {
            exchangeTypes = DEFAULT_EXCHANGE_TYPE_VALUES;
        }

        _toolkit.createLabel(typeComposite,"Type:").setBackground(shell.getBackground());
        final org.eclipse.swt.widgets.List typeList = new org.eclipse.swt.widgets.List(typeComposite, SWT.SINGLE | SWT.BORDER);
        typeList.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        typeList.setItems(exchangeTypes);
        
        Composite durableComposite = _toolkit.createComposite(shell, SWT.NONE);
        durableComposite.setBackground(shell.getBackground());
        GridData gridData = new GridData(SWT.FILL, SWT.TOP, true, false);
        gridData.minimumWidth = 220;
        durableComposite.setLayoutData(gridData);
        durableComposite.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(durableComposite,"Durable:").setBackground(shell.getBackground());
        final Button durableButton = new Button(durableComposite, SWT.CHECK);
        durableButton.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, true, false));


        Composite okCancelButtonsComp = _toolkit.createComposite(shell);
        okCancelButtonsComp.setBackground(shell.getBackground());
        okCancelButtonsComp.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, true, true));
        okCancelButtonsComp.setLayout(new GridLayout(2,false));
        
        Button okButton = _toolkit.createButton(okCancelButtonsComp, "OK", SWT.PUSH);
        okButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));
        Button cancelButton = _toolkit.createButton(okCancelButtonsComp, "Cancel", SWT.PUSH);
        cancelButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));

        okButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                String name = nameText.getText();
                
                if (name == null || name.length() == 0)
                {                            
                    ViewUtility.popupErrorMessage("Create Exchange", "Please enter a valid name");
                    return;
                }
                
                int selectedTypeIndex = typeList.getSelectionIndex();
                
                if (selectedTypeIndex == -1)
                {
                    ViewUtility.popupErrorMessage("Create Exchange", "Please select an Exchange type");
                    return;
                }
                
                String type = typeList.getItem(selectedTypeIndex);

                boolean durable = durableButton.getSelection();
                
                shell.dispose();
                
                try
                {
                    _vhmb.createNewExchange(name, type, durable);

                    ViewUtility.operationResultFeedback(null, "Created Exchange", null);
                    try
                    {   
                        //delay to allow mbean registration notification processing
                        Thread.sleep(250); 
                    }
                    catch(InterruptedException ie)
                    {
                        //ignore
                    }
                }
                catch(Exception e5)
                {
                    ViewUtility.operationFailedStatusBarMessage("Error creating Exchange");
                    MBeanUtility.handleException(_mbean, e5);
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
    
    private void deleteQueuesOrExchanges(Shell parent, final VhostOperations op)
    {
        Table table;
        String windowTitle;
        String dialogueMessage;
        final String feedBackMessage;
        final String failureFeedBackMessage;
        
        if(op.equals(VhostOperations.DELETE_QUEUE))
        {
            table = _queueTable;
            windowTitle = "Delete Queue(s)";
            dialogueMessage = "Delete Queue(s): ";
            feedBackMessage = "Queue(s) deleted";
            failureFeedBackMessage = "Error deleting Queue(s)";
        }
        else
        {
            table = _exchangeTable;
            windowTitle = "Delete Exchange(s)";
            dialogueMessage = "Delete Exchange(s): ";
            feedBackMessage = "Exchange(s) deleted";
            failureFeedBackMessage = "Error deleting Exchange(s)";
        }
        
        int selectionIndex = table.getSelectionIndex();
        if (selectionIndex == -1)
        {
            return;
        }
        
        int[] selectedIndices = table.getSelectionIndices();
        
        final ArrayList<ManagedBean> selectedMBeans = new ArrayList<ManagedBean>();
        
        for(int index = 0; index < selectedIndices.length ; index++)
        {
            ManagedBean selectedMBean = (ManagedBean)table.getItem(selectedIndices[index]).getData();
            selectedMBeans.add(selectedMBean);
        }
        
        
        final Shell shell = ViewUtility.createModalDialogShell(parent, windowTitle);

        _toolkit.createLabel(shell, dialogueMessage).setBackground(shell.getBackground());
        
        final Text headerText = new Text(shell, SWT.WRAP | SWT.V_SCROLL |  SWT.BORDER );
        headerText.setEditable(false);
        GridData data = new GridData(SWT.FILL, SWT.FILL, false, false);
        data.minimumHeight = 150;
        data.heightHint = 150;
        data.minimumWidth = 400;
        data.widthHint = 400;
        headerText.setLayoutData(data);

        String lineSeperator = System.getProperty("line.separator");
        for(ManagedBean mbean : selectedMBeans)
        {
            headerText.append(mbean.getName() + lineSeperator);
        }
        headerText.setSelection(0);
        
        Composite okCancelButtonsComp = _toolkit.createComposite(shell);
        okCancelButtonsComp.setBackground(shell.getBackground());
        okCancelButtonsComp.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, true, true));
        okCancelButtonsComp.setLayout(new GridLayout(2,false));
        
        Button okButton = _toolkit.createButton(okCancelButtonsComp, "OK", SWT.PUSH);
        okButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));
        Button cancelButton = _toolkit.createButton(okCancelButtonsComp, "Cancel", SWT.PUSH);
        cancelButton.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));

        okButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                shell.dispose();
                
                try
                {
                    //perform the deletes
                    for(ManagedBean mbean : selectedMBeans)
                    {
                        switch(op)
                        {
                            case DELETE_QUEUE:
                                _vhmb.deleteQueue(mbean.getName());
                                _serverRegistry.removeManagedObject(mbean);
                                break;
                            case DELETE_EXCHANGE:
                                _vhmb.unregisterExchange(mbean.getName());
                                break;
                        }
                        //remove the mbean from the server registry now instead of
                        //waiting for an mbean Unregistration Notification to do it
                        _serverRegistry.removeManagedObject(mbean);
                    }

                    ViewUtility.operationResultFeedback(null, feedBackMessage, null);
                }
                catch(Exception e1)
                {
                    ViewUtility.operationFailedStatusBarMessage(failureFeedBackMessage);
                    MBeanUtility.handleException(_mbean, e1);
                }

                refresh(_mbean);;
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
    
    private enum VhostOperations
    {
        DELETE_QUEUE,
        DELETE_EXCHANGE;
    }
    
    private void openMBean(Table table)
    {
        int selectionIndex = table.getSelectionIndex();

        if (selectionIndex == -1)
        {
            return;
        }
        
        ManagedBean selectedMBean = (ManagedBean) table.getItem(selectionIndex).getData();

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
