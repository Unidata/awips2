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
package org.apache.qpid.management.ui.views.type;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;

import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;

import static org.apache.qpid.management.ui.ApplicationRegistry.DATA_DIR;
import static org.apache.qpid.management.ui.Constants.QUEUE;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_NAME;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_ACTIVE_CONSUMER_COUNT;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_AUTODELETE;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_CONSUMER_COUNT;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_DURABLE;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_MAX_MSG_AGE;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_MAX_MSG_COUNT;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_MAX_MSG_SIZE;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_MAX_QUEUE_DEPTH;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_MSG_COUNT;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_OWNER;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_QUEUE_DEPTH;
import static org.apache.qpid.management.common.mbeans.ManagedQueue.ATTR_RCVD_MSG_COUNT;

import org.apache.qpid.management.common.mbeans.ManagedBroker;
import org.apache.qpid.management.common.mbeans.ManagedQueue;
import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.ManagedServer;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.views.MBeanView;
import org.apache.qpid.management.ui.views.NavigationView;
import org.apache.qpid.management.ui.views.ViewUtility;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class QueueTypeTabControl extends MBeanTypeTabControl
{   
    private MBeanServerConnection _mbsc;
    private ManagedBroker _vhmb;

    private List<String> _selectedAttributes;
    private PreferenceStore _preferences;
    private Semaphore _tableViewerSemaphore = new Semaphore(1);
    
    private static final String APP_DIR = ApplicationRegistry.DATA_DIR;
    private static final String INI_FILENAME = APP_DIR + File.separator + "qpidmc_queue_attributes.ini";
    private static final String INI_QUEUE_ATTRIBUES = "QueueAttributesSelection";
    
    private static final ArrayList<String> FALLBACK_ATTRIBUTES_LIST = new ArrayList<String>();
    static
    {
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_NAME);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_ACTIVE_CONSUMER_COUNT);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_AUTODELETE);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_CONSUMER_COUNT);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_DURABLE);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_MAX_MSG_AGE);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_MAX_MSG_COUNT);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_MAX_MSG_SIZE);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_MAX_QUEUE_DEPTH);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_MSG_COUNT);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_OWNER);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_QUEUE_DEPTH);
        FALLBACK_ATTRIBUTES_LIST.add(ATTR_RCVD_MSG_COUNT);
    }
    
    private static final Map<String, Integer> DEFAULT_COLUMN_WIDTHS = new HashMap<String,Integer>();
    static
    {
        DEFAULT_COLUMN_WIDTHS.put(ATTR_NAME, 215);
        DEFAULT_COLUMN_WIDTHS.put(ATTR_OWNER,125);
        DEFAULT_COLUMN_WIDTHS.put(ATTR_QUEUE_DEPTH,125);
    }
    
    public QueueTypeTabControl(TabFolder tabFolder, ManagedServer server, String virtualHost)
    {
        super(tabFolder,server,virtualHost,QUEUE);
        _mbsc = (MBeanServerConnection) _serverRegistry.getServerConnection();
        
        //create a proxy for the VirtualHostManager mbean to use in retrieving the attribute names/values
        _vhmb = MBeanServerInvocationHandler.newProxyInstance(_mbsc, 
                                _vhostMbean.getObjectName(), ManagedBroker.class, false);
        
    }
    
    @Override
    protected void init()
    {
        createIniFileIfNecessary();
        loadAttributePreferences();
    }
   
    /**
     * Create the ini file if it doesn't already exist.
     */
    public static void createIniFileIfNecessary()
    {
        File dir = new File(DATA_DIR);
        if (!dir.exists())
        {
            if(!dir.mkdir())
            {
                System.err.println("Could not create application data directory " + DATA_DIR);
                ViewUtility.popupErrorMessage("Error", "Fatal Error: Unable to create the application data directory: " + DATA_DIR);
                System.exit(1);
            }
        }

        File file = new File(INI_FILENAME);
        try
        {
            if (!file.exists())
            {
                file.createNewFile();
            }
        }
        catch (IOException ex)
        {
            System.err.println("Error creating the configuration file " + INI_FILENAME);
            ViewUtility.popupErrorMessage("Error", "Fatal Error: Unable to create the configuration file: " + INI_FILENAME);
            System.exit(1);
        }
    }
    
    private void loadAttributePreferences()
    {
        _preferences = new PreferenceStore(INI_FILENAME);
        List<String> attributesList = new ArrayList<String>();
       
        //ensure the name is present, and first
        attributesList.add(ManagedQueue.ATTR_NAME);
        
        //add any others from the file
        try
        {
            _preferences.load();
            
            String selectedAttributes = _preferences.getString(INI_QUEUE_ATTRIBUES);
            if (selectedAttributes.length() != 0)
            {
                String[] attributes = selectedAttributes.split(",");
                for (String attr : attributes)
                {
                    if(attr.equals(ManagedQueue.ATTR_NAME))
                    {
                        //the Name attribute is already present
                        continue;
                    }
                    
                    attributesList.add(attr);
                }
            }
        }
        catch (IOException e)
        {
            ViewUtility.popupErrorMessage("Error", "Unable to load previous attribute selections, defaulting to Name only");
            System.err.println(e);
        }
        
        _selectedAttributes = attributesList;
    }
    
    private void saveAttributePreferences()
    {
        String chosenAttributes = new String();
        
        for(String attr : _selectedAttributes)
        {
            chosenAttributes = chosenAttributes.concat(attr) + ",";
        }
        //cut off last ","
        int lastIndex = chosenAttributes.lastIndexOf(',');
        if (lastIndex != -1)
        {
            chosenAttributes = chosenAttributes.substring(0,lastIndex);
        }
        
        _preferences.putValue(INI_QUEUE_ATTRIBUES, chosenAttributes);
        
        try
        {
            _preferences.save();
        }
        catch (IOException e)
        {
            ViewUtility.popupErrorMessage("Error", "Unable to save the attribute selection, choices will be lost at shutdown");
            System.err.println(e);
        }
    }
    
    @Override
    public void refresh(ManagedBean mbean)
    {
        //Try locking. If we cant aquire the lock, dont bother getting new values.
        //Either the attributes are being changed and these values would be out of date,
        //or another thread is still in the process of refreshing
        if(_tableViewerSemaphore.tryAcquire())
        {
            try
            {
                List<List<Object>> values = null;

                if(_ApiVersion.greaterThanOrEqualTo(1, 3))
                {
                    //Qpid JMX API 1.3+, use this virtualhosts VirtualHostManager MBean
                    //to retrieve the attributes values requested for all queues at once
                    try
                    {
                        values = _vhmb.retrieveQueueAttributeValues(_selectedAttributes.toArray(new String[0]));              
                    }
                    catch(Exception e)
                    {
                        MBeanUtility.handleException(_vhostMbean, e);
                    }
                }
                else
                {
                    //Qpid JMX API 1.2 or below, use the local ManagedBeans and look
                    //up the attribute values for each queue individually
                    _mbeans = getMbeans();
                    values = MBeanUtility.getQueueAttributes(_mbeans, _selectedAttributes.toArray(new String[0]));
                }

                _tableViewer.setInput(values);
                layout();
            }
            finally
            {
                _tableViewerSemaphore.release();
            }
        }
        
    }
    
    @Override
    protected List<ManagedBean> getMbeans()
    {
        return _serverRegistry.getQueues(_virtualHost);
    }
    
    private void clearTableComposite()
    {
        ViewUtility.disposeChildren(_tableComposite);
    }

    @Override
    protected void createTable()
    {
        _table = new Table (_tableComposite, SWT.MULTI | SWT.SCROLL_LINE | SWT.BORDER | SWT.FULL_SELECTION);
        _table.setLinesVisible (true);
        _table.setHeaderVisible (true);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        _table.setLayoutData(data);
        
        _tableViewer = new TableViewer(_table);

        final QueueTableSorter tableSorter = new QueueTableSorter();
                
        for (int i = 0; i < _selectedAttributes.size(); i++) 
        {
            final int index = i;
            final TableViewerColumn viewerColumn = new TableViewerColumn(_tableViewer, SWT.NONE);
            final TableColumn column = viewerColumn.getColumn();

            String attrName = _selectedAttributes.get(i);
            column.setMoveable(true);
            column.setText(attrName);
            column.pack();
            if(DEFAULT_COLUMN_WIDTHS.containsKey(attrName))
            {
                //retrieve the desired default width
                column.setWidth(DEFAULT_COLUMN_WIDTHS.get(attrName));
            }
            else
            {
                //add padding for sort direction indicator
                column.setWidth(column.getWidth() + 15);
            }
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

        _tableViewer.setContentProvider(new QueueContentProviderImpl());
        _tableViewer.setLabelProvider(new QueueLabelProviderImpl());

        _tableViewer.setUseHashlookup(true);
        _tableViewer.setSorter(tableSorter);
        _table.setSortColumn(_table.getColumn(0));
        _table.setSortDirection(SWT.UP);
        
        addTableListeners();
    }
    
    protected void createLowerAreaButton(Composite parent)
    {
        Composite lowerButtonComposite = _toolkit.createComposite(parent, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        lowerButtonComposite.setLayoutData(gridData);
        lowerButtonComposite.setLayout(new GridLayout());
        
        final Button attributesButton = _toolkit.createButton(lowerButtonComposite, "Select Attributes ...", SWT.PUSH);
        gridData = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        attributesButton.setLayoutData(gridData);
        attributesButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                chooseAttributes(attributesButton.getShell());
            }
        });
    }
    
    private void chooseAttributes(final Shell parent)
    {

        List<String> availableAttributes;
        if(_ApiVersion.greaterThanOrEqualTo(1, 3))
        {
            //Qpid JMX API 1.3+, request the current queue attributes names from the broker
            try
            {
                availableAttributes = _vhmb.retrieveQueueAttributeNames();
            }
            catch (IOException e)
            {
                availableAttributes = new ArrayList<String>(FALLBACK_ATTRIBUTES_LIST);
            }
        }
        else
        {
            //Qpid JMX API 1.2 or below, use the falllback list of names.
            availableAttributes = new ArrayList<String>(FALLBACK_ATTRIBUTES_LIST);
        }
        
        
        final List<String> chosenAttributes = new ArrayList<String>();
        
        final Shell shell = ViewUtility.createModalDialogShell(parent, "Select Attributes");

        Composite attributesComposite = _toolkit.createComposite(shell, SWT.NONE);
        attributesComposite.setBackground(shell.getBackground());
        attributesComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        attributesComposite.setLayout(new GridLayout(2,false));
        
        //add a selected-but-disabled check box for the Name attribute (its a mandatory attribute)
        final Button nameCheckbox = new Button(attributesComposite, SWT.CHECK);
        nameCheckbox.setText(ManagedQueue.ATTR_NAME);
        nameCheckbox.setSelection(true);
        nameCheckbox.setEnabled(false);

        for(String attr : availableAttributes)
        {
            if(attr.equals(ManagedQueue.ATTR_NAME))
            {
                //Name attribute is mandatory and gets added to the front of the list later
                continue;
            }
                        
            final Button attrButton = new Button(attributesComposite, SWT.CHECK);
            attrButton.setText(attr);
            
            //if it was checked before, select it again now
            if(_selectedAttributes.contains(attr))
            {
                attrButton.setSelection(true);
                chosenAttributes.add(attr);
            }
            
            //add a selection listener to update the selected attribute list
            attrButton.addSelectionListener(new SelectionAdapter()
            {
                public void widgetSelected(SelectionEvent e)
                {
                    if(attrButton.getSelection())
                    {
                        chosenAttributes.add(attrButton.getText());
                    }
                    else
                    {
                        chosenAttributes.remove(attrButton.getText());
                    }
                }
            });
        }
        
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

                //The Name attribute is mandatory, add it now, also 
                //ensuring it is left-most by placing it first in the list
                List<String> newSelection = new ArrayList<String>();
                newSelection.add(ManagedQueue.ATTR_NAME);

                //now add all remaining choices in alphabetical order
                Collections.sort(chosenAttributes);
                newSelection.addAll(chosenAttributes);

                _tableViewerSemaphore.acquireUninterruptibly();
                try
                {
                    _selectedAttributes = newSelection;

                    clearTableComposite();
                    createTable();
                    saveAttributePreferences();
                }
                finally
                {
                    _tableViewerSemaphore.release();
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

    private String getQueueDepthString(Long value)
    {
        if(value == null)
        {
            return "-";
        }

        if (_ApiVersion.greaterThanOrEqualTo(1,2))
        {
            //Qpid JMX API 1.2 or above, returns Bytes
            return convertLongBytesToText(value);
        }
        else
        {
            //Qpid JMX API 1.1 or below, returns KB  
            double mb = 1024.0;

            if(value > mb) //MB
            {
                return String.format("%.3f", (Double)(value / mb)) + " MB";
            }
            else //KB
            {
                return value + " KB";
            }
        }
    }
    
    private String convertLongBytesToText(Long value)
    {
        if(value == null)
        {
            return "-";
        }

        double mb = 1024.0 * 1024.0;
        double kb = 1024.0;

        if(value >= mb) //MB
        {
            return String.format("%.3f", (Double)((double)value / mb)) + " MB";
        }
        else if (value >= kb) //KB
        {
            return String.format("%.3f", (Double)((double)value / kb)) + " KB";
        }
        else //Bytes
        {
            return value + " Bytes";
        }
    }

    /**
     * sorter class for the table viewer.
     *
     */
    private class QueueTableSorter extends ViewerSorter
    {
        protected int column;
        protected static final int ASCENDING = 0;
        protected static final int DESCENDING = 1;

        protected int direction;

        public QueueTableSorter()
        {
            this.column = 0;
            direction = ASCENDING;
        }

        public void setColumn(int column)
        {
            if(column == this.column)
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

        @SuppressWarnings("unchecked")
        @Override
        public int compare(Viewer viewer, Object e1, Object e2)
        {
            List<Object> queue1 = (List<Object>) e1;
            List<Object> queue2 = (List<Object>) e2;
            
            int comparison = 0;
            switch(column)
            {
                default:
                    if(queue1.get(column) instanceof Comparable)
                    {
                        comparison = ((Comparable)queue1.get(column)).compareTo((Comparable) queue2.get(column));
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

    /**
     * Content Provider class for the table viewer for Qpid JMX API 1.3 and above.
     */
    private class QueueContentProviderImpl  implements IStructuredContentProvider
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
            return ((List<List<Object>>) parent).toArray();
        }
    }
    
    /**
     * Label Provider class for the table viewer for for Qpid JMX API 1.3 and above.
     */
    private class QueueLabelProviderImpl extends LabelProvider implements ITableLabelProvider
    {
        @SuppressWarnings("unchecked")
        @Override
        public String getColumnText(Object element, int columnIndex)
        {
            List<Object> attributes = (List<Object>) element;
            
            switch (columnIndex)
            {
                default :
                    String attrName = _selectedAttributes.get(columnIndex);
                    
                    if(ATTR_QUEUE_DEPTH.equals(attrName))
                    {
                        return getQueueDepthString((Long) attributes.get(columnIndex));
                    }
                    else if(ATTR_MAX_QUEUE_DEPTH.equals(attrName) || ATTR_MAX_MSG_SIZE.equals(attrName))
                    {
                        Number val = (Number)attributes.get(columnIndex);
                        return convertLongBytesToText(val.longValue());
                    }
                    else if(ATTR_MAX_MSG_AGE.equals(attrName))
                    {
                        return String.valueOf(attributes.get(columnIndex) + "ms");
                    }
                    
                    return String.valueOf(attributes.get(columnIndex));
            }
        }
        
        @Override
        public Image getColumnImage(Object element, int columnIndex)
        {
            return null;
        }    
    }
    
    @SuppressWarnings("unchecked")
    @Override
    protected void addMBeanToFavourites()
    {
        int selectionIndex = _table.getSelectionIndex();

        if (selectionIndex == -1)
        {
            return;
        }

        int[] selectedIndices = _table.getSelectionIndices();
        
        ArrayList<ManagedBean> selectedMBeans = new ArrayList<ManagedBean>();

        //the entries are created from an List<Object> with the attribute values (name first)
        for(int index = 0; index < selectedIndices.length ; index++)
        {
            List<Object> queueEntry = (List<Object>) _table.getItem(selectedIndices[index]).getData();
            String queueName = (String) queueEntry.get(0);
            selectedMBeans.add(_serverRegistry.getQueue(queueName, _virtualHost));
        }

        IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow(); 
        NavigationView view = (NavigationView)window.getActivePage().findView(NavigationView.ID);
        
        ManagedBean bean = null;
        try
        {
            for(ManagedBean mbean: selectedMBeans)
            {
                bean = mbean;
                view.addManagedBean(mbean);
            }
        }
        catch (Exception ex)
        {
            MBeanUtility.handleException(bean, ex);
        }
    }
    
    @SuppressWarnings("unchecked")
    @Override
    protected void openMBean()
    {
        int selectionIndex = _table.getSelectionIndex();

        if (selectionIndex == -1)
        {
            return;
        }
        
        ManagedBean selectedMBean;

        //the entries are created from an List<Object> with the attribute values (name first)
        List<Object> queueEntry = (List<Object>) _table.getItem(selectionIndex).getData();
        String queueName = (String) queueEntry.get(0);
        selectedMBean = _serverRegistry.getQueue(queueName, _virtualHost);

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
