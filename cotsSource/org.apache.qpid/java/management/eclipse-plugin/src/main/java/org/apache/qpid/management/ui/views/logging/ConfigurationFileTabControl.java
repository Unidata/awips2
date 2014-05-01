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
package org.apache.qpid.management.ui.views.logging;

import java.util.ArrayList;
import java.util.HashMap;

import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularDataSupport;

import static org.apache.qpid.management.ui.Constants.FONT_BOLD;

import org.apache.qpid.management.ui.ApiVersion;
import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.common.mbeans.LoggingManagement;
import org.apache.qpid.management.ui.jmx.JMXManagedObject;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.views.TabControl;
import org.apache.qpid.management.ui.views.ViewUtility;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;


/**
 * Control class for the LoggingManagement mbean ConfigFile Options tab.
 */
public class ConfigurationFileTabControl extends TabControl
{
    private FormToolkit _toolkit;
    private ScrolledForm        _form;
    private Table _table = null;
    private TableViewer _tableViewer = null;
    private Composite _headerComposite = null;
    private Composite _paramsComposite = null;
        
    private Label _configFileRootLoggerLevelLabel = null;
    private Label _logWatchIntervalLabel = null;
    private String[] _availableLoggerLevels;
    private TabularDataSupport _configFileLoggerLevels = null;
    private LoggingManagement _lmmb;
    private ApiVersion _ApiVersion;
    
    static final String LOGGER_NAME = LoggingManagement.COMPOSITE_ITEM_NAMES[0];
    static final String LOGGER_LEVEL = LoggingManagement.COMPOSITE_ITEM_NAMES[1];
    
    public ConfigurationFileTabControl(TabFolder tabFolder, JMXManagedObject mbean, MBeanServerConnection mbsc)
    {
        super(tabFolder);
        _mbean = mbean;
        _lmmb = (LoggingManagement)
                MBeanServerInvocationHandler.newProxyInstance(mbsc, mbean.getObjectName(),
                                                            LoggingManagement.class, false);
        _ApiVersion = ApplicationRegistry.getServerRegistry(mbean).getManagementApiVersion();
        _toolkit = new FormToolkit(_tabFolder.getDisplay());
        _form = _toolkit.createScrolledForm(_tabFolder);
        _form.getBody().setLayout(new GridLayout());
        createComposites();
        createWidgets();
    }
    
    private void createComposites()
    {

        _headerComposite = _toolkit.createComposite(_form.getBody(), SWT.NONE);
        _headerComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        _headerComposite.setLayout(new GridLayout());

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
        String configFileRootLoggerLevel = "-";
        try
        {
            configFileRootLoggerLevel = _lmmb.getConfigFileRootLoggerLevel();
        }
        catch (Exception e1)
        {
            MBeanUtility.handleException(_mbean, e1);
        }
        
        int log4jLogWatchInterval = -1;
        try
        {
            log4jLogWatchInterval = _lmmb.getLog4jLogWatchInterval();
        }
        catch (Exception e2)
        {
            MBeanUtility.handleException(_mbean, e2);
        }

        _configFileLoggerLevels = null;
        try
        {
            _configFileLoggerLevels = (TabularDataSupport) _lmmb.viewConfigFileLoggerLevels();
        }
        catch (Exception e3)
        {
            MBeanUtility.handleException(_mbean, e3);
        }
        
        _configFileRootLoggerLevelLabel.setText(String.valueOf(configFileRootLoggerLevel));
        if (log4jLogWatchInterval == 0)
        {
            _logWatchIntervalLabel.setText("Disabled (0 sec)");
        }
        else
        {
            _logWatchIntervalLabel.setText(String.valueOf(log4jLogWatchInterval) + " seconds");
        }
        _tableViewer.setInput(_configFileLoggerLevels);

        layout();
    }
    
    public void layout()
    {
        _form.layout(true);
        _form.getBody().layout(true, true);
    }
    
    private void createWidgets()
    {
        try
        {
            _availableLoggerLevels = _lmmb.getAvailableLoggerLevels();
        }
        catch(Exception e)
        {
            _availableLoggerLevels = new String[]{"ALL","TRACE","DEBUG","INFO","WARN","ERROR","FATAL","OFF"};
        }
        
        Label noteLabel = _toolkit.createLabel(_headerComposite, 
                "NOTE: These options modify the configuration file. " +
                "Changes only take effect automatically if LogWatch is enabled.");
        Label noteLabel2 = _toolkit.createLabel(_headerComposite, 
                "A child Logger set to a non-inherited Level in the Runtime tab " +
                "will retain that value after the file is reloaded.");
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, false, true);
        noteLabel.setLayoutData(gridData);
        gridData = new GridData(SWT.FILL, SWT.FILL, false, true);
        noteLabel2.setLayoutData(gridData);
        
        Group configFileLoggerLevelsGroup = new Group(_paramsComposite, SWT.SHADOW_NONE);
        configFileLoggerLevelsGroup.setBackground(_paramsComposite.getBackground());
        configFileLoggerLevelsGroup.setText("Configuration File Logger Levels");
        configFileLoggerLevelsGroup.setLayout(new GridLayout());
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        configFileLoggerLevelsGroup.setLayoutData(gridData);
        
        Composite tableComposite = _toolkit.createComposite(configFileLoggerLevelsGroup);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.heightHint = 250;
        gridData.minimumHeight = 250;
        tableComposite.setLayoutData(gridData);
        GridLayout gridLayout = new GridLayout();

        tableComposite.setLayout(gridLayout);
        
        _table = new Table (tableComposite, SWT.MULTI | SWT.SCROLL_LINE | SWT.BORDER | SWT.FULL_SELECTION);
        _table.setLinesVisible (true);
        _table.setHeaderVisible (true);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        _table.setLayoutData(data);
        
        _tableViewer = new TableViewer(_table);
        final LoggingTableSorter tableSorter = new LoggingTableSorter();
        
        String[] titles = { LOGGER_NAME, LOGGER_LEVEL };
        int[] bounds = { 600, 75 };
        for (int i = 0; i < titles.length; i++) 
        {
            final int index = i;
            final TableViewerColumn viewerColumn = new TableViewerColumn(_tableViewer, SWT.NONE);
            final TableColumn column = viewerColumn.getColumn();

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
        
        _tableViewer.setContentProvider(new LoggingTableContentProvider());
        _tableViewer.setLabelProvider(new LoggingTableLabelProvider());
        _tableViewer.setSorter(tableSorter);
        _table.setSortColumn(_table.getColumn(0));
        _table.setSortDirection(SWT.UP);
        _table.addMouseListener(new MouseListener()
        {
            public void mouseDoubleClick(MouseEvent event)
            {
                editLoggerLevel(_table.getShell());
            }

            public void mouseDown(MouseEvent e){}
            public void mouseUp(MouseEvent e){}
        });
        
        final Button logLevelEditButton = _toolkit.createButton(configFileLoggerLevelsGroup, "Edit Selected Logger(s)...", SWT.PUSH);
        logLevelEditButton.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false));
        logLevelEditButton.setEnabled(false);
        logLevelEditButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                editLoggerLevel(logLevelEditButton.getShell());
            }
        });
        
        _tableViewer.addSelectionChangedListener(new ISelectionChangedListener(){
            public void selectionChanged(SelectionChangedEvent evt)
            {
                int selectionIndex = _table.getSelectionIndex();

                if(selectionIndex != -1)
                {
                    logLevelEditButton.setEnabled(true);
                }
                else
                {
                    logLevelEditButton.setEnabled(false);
                }
            }
        });
        
        
        Composite attributesComposite = _toolkit.createComposite(_paramsComposite);
        gridData = new GridData(SWT.LEFT, SWT.FILL, false, true);
        attributesComposite.setLayoutData(gridData);
        gridLayout = new GridLayout(3,false);
        attributesComposite.setLayout(gridLayout);
        
        Group configFileRootLoggerLevelGroup = new Group(attributesComposite, SWT.SHADOW_NONE);
        configFileRootLoggerLevelGroup.setBackground(attributesComposite.getBackground());
        configFileRootLoggerLevelGroup.setText("Config File RootLogger Level");
        gridData = new GridData(SWT.LEFT, SWT.TOP, true, false);
        configFileRootLoggerLevelGroup.setLayoutData(gridData);
        configFileRootLoggerLevelGroup.setLayout(new GridLayout(2,false));
        
        _configFileRootLoggerLevelLabel = _toolkit.createLabel(configFileRootLoggerLevelGroup, "-");
        _configFileRootLoggerLevelLabel.setFont(ApplicationRegistry.getFont(FONT_BOLD));
        _configFileRootLoggerLevelLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        
        final Button configFileRootLoggerLevelButton = _toolkit.createButton(configFileRootLoggerLevelGroup, "Edit ...", SWT.PUSH);
        configFileRootLoggerLevelButton.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, false));
        configFileRootLoggerLevelButton.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                editRootLoggerLevel(configFileRootLoggerLevelButton.getShell());
            }
        });
        
        
        Group logWatchIntervalGroup = new Group(attributesComposite, SWT.SHADOW_NONE);
        logWatchIntervalGroup.setBackground(attributesComposite.getBackground());
        logWatchIntervalGroup.setText("LogWatch Interval");
        gridData = new GridData(SWT.LEFT, SWT.FILL, true, false);
        logWatchIntervalGroup.setLayoutData(gridData);
        logWatchIntervalGroup.setLayout(new GridLayout());

        _logWatchIntervalLabel = _toolkit.createLabel(logWatchIntervalGroup, "-");
        _logWatchIntervalLabel.setFont(ApplicationRegistry.getFont(FONT_BOLD));
        _logWatchIntervalLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, true));
        
        if(_ApiVersion.greaterThanOrEqualTo(1, 4))
        {
            Group reloadConfigFileGroup = new Group(attributesComposite, SWT.SHADOW_NONE);
            reloadConfigFileGroup.setBackground(attributesComposite.getBackground());
            reloadConfigFileGroup.setText("Reload Configuration File");
            gridData = new GridData(SWT.LEFT, SWT.TOP, true, false);
            reloadConfigFileGroup.setLayoutData(gridData);
            reloadConfigFileGroup.setLayout(new GridLayout());

            final Button reloadConfigFileButton = _toolkit.createButton(reloadConfigFileGroup, "Reload Config File", SWT.PUSH);
            reloadConfigFileButton.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));
            reloadConfigFileButton.addSelectionListener(new SelectionAdapter()
            {
                public void widgetSelected(SelectionEvent e)
                {
                    int response = ViewUtility.popupOkCancelConfirmationMessage("Reload", 
                                                                            "Reload Logging Configuration File?");
                    if (response == SWT.OK)
                    {
                        try
                        {
                            _lmmb.reloadConfigFile();
                            ViewUtility.operationResultFeedback(null, "Reloaded Logging Configuration File", null);

                        }
                        catch (Exception ex)
                        {
                            ViewUtility.operationFailedStatusBarMessage("Error Reloading Logging Configuration File");
                            MBeanUtility.handleException(_mbean, ex);
                        }

                        refresh(_mbean);;
                    }
                }
            });
        }
    }


    
    private void editLoggerLevel(Shell parent)
    {
        int selectionIndex = _table.getSelectionIndex();

        if (selectionIndex != -1)
        {
            int[] selectedIndices = _table.getSelectionIndices();
            
            final ArrayList<String> selectedLoggers = new ArrayList<String>();
            
            for(int index = 0; index < selectedIndices.length ; index++)
            {
                CompositeData selectedLogger = (CompositeData)_table.getItem(selectedIndices[index]).getData();
                String user = (String) selectedLogger.get(LOGGER_NAME);
                selectedLoggers.add(user);
            }

            final Shell shell = ViewUtility.createModalDialogShell(parent, "Set Config File Logger Level(s)");
            
            _toolkit.createLabel(shell, "Logger(s): ").setBackground(shell.getBackground());

            final Text headerText = new Text(shell, SWT.WRAP | SWT.V_SCROLL |  SWT.BORDER );
            headerText.setEditable(false);
            GridData data = new GridData(SWT.FILL, SWT.FILL, false, false);
            data.minimumHeight = 125;
            data.heightHint = 125;
            data.minimumWidth = 575;
            data.widthHint = 575;
            headerText.setLayoutData(data);

            String lineSeperator = System.getProperty("line.separator");
            for(String loggerName : selectedLoggers)
            {
                headerText.append(loggerName + lineSeperator);
            }
            headerText.setSelection(0);

            Composite levelComp = _toolkit.createComposite(shell);
            levelComp.setBackground(shell.getBackground());
            levelComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
            levelComp.setLayout(new GridLayout(2,false));
            
            _toolkit.createLabel(levelComp,"Level: ").setBackground(levelComp.getBackground());
            final Combo levelCombo = new Combo (levelComp, SWT.READ_ONLY );
            levelCombo.setItems(_availableLoggerLevels);
            levelCombo.select(0);

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
                    String level = levelCombo.getItem(levelCombo.getSelectionIndex()).toString();
                    
                    shell.close();

                    try
                    {
                        HashMap<String,Boolean> results = new HashMap<String,Boolean>();
                        
                        //perform the updates, save the results.
                        for(String logger : selectedLoggers)
                        {
                            boolean result = _lmmb.setConfigFileLoggerLevel(logger, level);
                            results.put(logger, result);
                        }
                        
                        //categorise the overall result
                        boolean overallResult = true;
                        for(boolean result : results.values())
                        {
                            if (!result)
                            {
                                overallResult = false;
                            }
                        }
                        
                        //output the result to status bar if all succeed, and dialogue if not
                        if(overallResult)
                        {
                            ViewUtility.operationResultFeedback(overallResult, "Updated ConfigFile Logger Level(s)", null);
                        }
                        else
                        {
                            String failedToSetLevelOfLoggers = "";
                            for(String logger : results.keySet())
                            {
                                if(!results.get(logger))
                                {
                                    failedToSetLevelOfLoggers = failedToSetLevelOfLoggers.concat(logger + ", ");
                                }
                            }
                            
                            //cut off last ", "
                            int lastIndex = failedToSetLevelOfLoggers.lastIndexOf(',');
                            if (lastIndex != -1)
                            {
                                failedToSetLevelOfLoggers = failedToSetLevelOfLoggers.substring(0, lastIndex);
                            }
                            
                            ViewUtility.operationResultFeedback(overallResult, null, 
                                                                "Failed to update ConfigFile Logger Level(s): "
                                                                + failedToSetLevelOfLoggers);
                        }
                    }
                    catch(Exception e4)
                    {
                        ViewUtility.operationFailedStatusBarMessage("Error updating Config File Logger Level(s)");
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
    }
    
    private void editRootLoggerLevel(Shell parent)
    {
        final Shell shell = ViewUtility.createModalDialogShell(parent, "ConfigFile RootLogger Level");

        Composite levelComp = _toolkit.createComposite(shell);
        levelComp.setBackground(shell.getBackground());
        levelComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        levelComp.setLayout(new GridLayout(2,false));
        
        _toolkit.createLabel(levelComp,"RootLogger level: ").setBackground(levelComp.getBackground());
        final Combo levelCombo = new Combo (levelComp, SWT.READ_ONLY );
        levelCombo.setItems(_availableLoggerLevels);
        levelCombo.select(0);

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
                String selection = levelCombo.getItem(levelCombo.getSelectionIndex()).toString();
                shell.dispose();
                try
                {
                    boolean result = _lmmb.setConfigFileRootLoggerLevel(selection);
                    ViewUtility.operationResultFeedback(result, 
                            "Updated ConfigFile RootLogger Level", "Failed to update ConfigFile RootLogger Level");
                }
                catch (Exception e5)
                {
                    ViewUtility.operationFailedStatusBarMessage("Error updating ConfigFile RootLogger Level");
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
}
