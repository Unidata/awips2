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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularDataSupport;

import static org.apache.qpid.management.ui.Constants.*;

import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.model.OperationData;
import org.apache.qpid.management.ui.model.ParameterData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;


/**
 * Control class for the MBean operations tab. It creates the required widgets
 * for the selected MBean.
 */
public class OperationTabControl extends TabControl
{
    private static final int heightForAParameter = 30;
    private static final int labelWidth = 30;
    private static final int valueWidth = labelWidth + 25;
    
    private FormToolkit _toolkit;
    private Form        _form;
    private OperationData _opData;
    
    private SelectionListener operationExecutionListener = new OperationExecutionListener(); 
    private SelectionListener refreshListener = new RefreshListener(); 
    private SelectionListener parameterSelectionListener = new ParameterSelectionListener();
    private SelectionListener booleanSelectionListener = new BooleanSelectionListener();
    private VerifyListener    verifyListener = new VerifyListenerImpl();
    private KeyListener       keyListener = new KeyListenerImpl();
    private KeyListener       headerBindingListener = new HeaderBindingKeyListener();
    
    private Composite _headerComposite = null;
    private Composite _paramsComposite = null;
    private Composite _resultsComposite = null;
    private Button _executionButton = null;
    
    // for customized method in header exchange
    private HashMap<Text, Text> headerBindingHashMap = null;
    private String _virtualHostName = null;
    
    public OperationTabControl(TabFolder tabFolder, OperationData opData)
    {
        super(tabFolder);
        _toolkit = new FormToolkit(_tabFolder.getDisplay());
        _form = _toolkit.createForm(_tabFolder);
        _form.getBody().setLayout(new GridLayout());
        _opData = opData;
        createComposites();
        setHeader();
    }
    
    /**
     * Form area is devided in four parts:
     * Header composite - displays operaiton information
     * Patameters composite - displays parameters if there
     * Button - operation execution button
     * Results composite - displays results for operations, which have 
     *                     no parameters but have some return value
     */
    private void createComposites()
    {
        // 
        _headerComposite = _toolkit.createComposite(_form.getBody(), SWT.NONE);
        _headerComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        List<ParameterData> params = _opData.getParameters();
        if (params != null && !params.isEmpty())
        {
            _paramsComposite = _toolkit.createComposite(_form.getBody(), SWT.NONE);
            _paramsComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        }
        _executionButton = _toolkit.createButton(_form.getBody(), BUTTON_EXECUTE, SWT.PUSH | SWT.CENTER);
        _executionButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        GridData layoutData = new GridData(SWT.CENTER, SWT.TOP, true, false);
        layoutData.verticalIndent = 20;
        _executionButton.setLayoutData(layoutData);
        
        _resultsComposite = _toolkit.createComposite(_form.getBody(), SWT.NONE);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.verticalIndent = 20;
        _resultsComposite.setLayoutData(layoutData);
        _resultsComposite.setLayout(new GridLayout());
    }
    
    /**
     * @see TabControl#getControl()
     */
    public Control getControl()
    {
        return _form;
    }
    
    @Override
    public void refresh(ManagedBean mbean)
    {
        _mbean = mbean;
        _virtualHostName = _mbean.getVirtualHostName();
        
        // Setting the form to be invisible. Just in case the mbean server connection
        // is done and it takes time in getting the response, then the ui should be blank
        // instead of having half the widgets displayed.
        _form.setVisible(false);
        
        ViewUtility.disposeChildren(_paramsComposite);
        createParameterWidgets();
        
        // Set button text and add appropriate listener to button.
        // If there are no parameters and it is info operation, then operation gets executed
        // and result is displayed
        List<ParameterData> params = _opData.getParameters();
        if (params != null && !params.isEmpty())
        {            
            setButton(BUTTON_EXECUTE);
        }
        else if (_opData.getImpact() == OPERATION_IMPACT_ACTION)
        {
            setButton(BUTTON_EXECUTE);
        }
        else if (_opData.getImpact() == OPERATION_IMPACT_INFO)
        {
            setButton(BUTTON_REFRESH);
            executeAndShowResults();
        }
        
        _form.setVisible(true);
        layout();
    }
    
    public void layout()
    {
        _form.layout(true);
        _form.getBody().layout(true, true);
    }
    
    /**
     * populates the header composite, containing the operation name and description.
     */
    private void setHeader()
    {
        _form.setText(ViewUtility.getDisplayText(_opData.getName()));
        _headerComposite.setLayout(new GridLayout(2, false));
        //operation description
        Label label = _toolkit.createLabel(_headerComposite,  DESCRIPTION + " : ");
        label.setFont(ApplicationRegistry.getFont(FONT_BOLD));
        label.setLayoutData(new GridData(SWT.LEAD, SWT.TOP, false, false));
        
        label = _toolkit.createLabel(_headerComposite,  _opData.getDescription());
        label.setFont(ApplicationRegistry.getFont(FONT_NORMAL));
        label.setLayoutData(new GridData(SWT.LEAD, SWT.TOP, true, false));
        
        _headerComposite.layout();
    }
    
    /**
     * Creates the widgets for operation parameters if there are any
     */
    private void createParameterWidgets()
    {
        List<ParameterData> params = _opData.getParameters();
        if (params == null || params.isEmpty())
        {
            return;
        }
        
        // Customised parameter widgets        
        if (_mbean.isExchange() &&
            DEFAULT_EXCHANGE_TYPE_VALUES[2].equals(_mbean.getProperty(EXCHANGE_TYPE)) &&
            _opData.getName().equalsIgnoreCase(OPERATION_CREATE_BINDING))
        {                                  
            customCreateNewBinding(); 
            return;
        }
        // end of Customised parameter widgets       
        
        _paramsComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        _paramsComposite.setLayout(new FormLayout());
        int parameterPositionOffset = 0;
        for (ParameterData param : params)
        {            
            boolean valueInCombo = false;
            Label label = _toolkit.createLabel(_paramsComposite, ViewUtility.getDisplayText(param.getName()));
            FormData formData = new FormData();
            if (params.indexOf(param) == 0)
            {
                parameterPositionOffset = 0;
            }
            else
            {
                parameterPositionOffset += heightForAParameter;
            }
            formData.top = new FormAttachment(0, parameterPositionOffset + 2);
            formData.right = new FormAttachment(labelWidth);
            label.setLayoutData(formData);
            label.setToolTipText(param.getDescription());
            
            formData = new FormData();
            formData.top = new FormAttachment(0, parameterPositionOffset);
            formData.left = new FormAttachment(label, 5);
            formData.right = new FormAttachment(valueWidth);
            // this will contain the list of items, if the list is to be made available to choose from
            // e.g. the list of exchanges
            String[] items = null;
            if (param.getName().equals(QUEUE))
            {
                List<String> qList = ApplicationRegistry.getServerRegistry(_mbean).getQueueNames(_virtualHostName);
                // Customization for AMQQueueMBean method OPERATION_MOVE_MESSAGES
                if (_opData.getName().equals(OPERATION_MOVE_MESSAGES))
                {
                    qList.remove(_mbean.getName());    
                }
                // End of Customization
                items = qList.toArray(new String[0]);
            }
            else if (param.getName().equals(EXCHANGE))
            {
                items = ApplicationRegistry.getServerRegistry(_mbean).getExchangeNames(_virtualHostName);
            }
            else if (param.getName().equals(EXCHANGE_TYPE))
            {
                items = DEFAULT_EXCHANGE_TYPE_VALUES;
            }
            else if (isUserListParameter(param))
            {
                List<String> list = ApplicationRegistry.getServerRegistry(_mbean).getUsernames();
                if (list != null && !list.isEmpty())
                {
                    items = list.toArray(new String[0]);
                }
            }
            
            if (items != null)
            {
                org.eclipse.swt.widgets.List _list = new org.eclipse.swt.widgets.List(_paramsComposite, SWT.BORDER | SWT.V_SCROLL);
                int listSize = _form.getClientArea().height * 2 / 3;
                int itemsHeight = items.length * (_list.getItemHeight() + 2);
                // Set a min height for the list widget (set it to min 4 items)
                if (items.length < 4)
                {
                    itemsHeight = 4 * (_list.getItemHeight() + 2);
                }
                
                listSize = (listSize > itemsHeight) ? itemsHeight : listSize;
                parameterPositionOffset = parameterPositionOffset + listSize;
                formData.bottom = new FormAttachment(0, parameterPositionOffset);
                _list.setLayoutData(formData);
                _list.setData(param);
                _list.setItems(items);
                _list.addSelectionListener(parameterSelectionListener);
                valueInCombo = true;
            }
            else if (param.isBoolean())
            {
                Button booleanButton = _toolkit.createButton(_paramsComposite, "", SWT.CHECK);
                booleanButton.setLayoutData(formData);
                booleanButton.setData(param);
                booleanButton.addSelectionListener(booleanSelectionListener);
                valueInCombo = true;                
            }
            else
            {
                int style = SWT.NONE;
                if (PASSWORD.equalsIgnoreCase(param.getName()))
                {
                    style = SWT.PASSWORD;
                }
                Text text = _toolkit.createText(_paramsComposite, "", style);
                formData = new FormData();
                formData.top = new FormAttachment(0, parameterPositionOffset);
                formData.left = new FormAttachment(label, 5);
                formData.right = new FormAttachment(valueWidth);
                text.setLayoutData(formData);
                // Listener to assign value to the parameter
                text.addKeyListener(keyListener);
                // Listener to verify if the entered key is valid
                text.addVerifyListener(verifyListener);
                text.setData(param);
            }
            
            // display the parameter data type next to the text field
            if (valueInCombo)
            {
                label = _toolkit.createLabel(_paramsComposite, "");
            }
            else if (PASSWORD.equalsIgnoreCase(param.getName()))
            {
                label = _toolkit.createLabel(_paramsComposite, "(String)");
            }
            else
            {
                String str = param.getType();
                
                if (param.getType().lastIndexOf(".") != -1)
                    str = param.getType().substring(1 + param.getType().lastIndexOf("."));
                
                label = _toolkit.createLabel(_paramsComposite, "(" + str + ")");
            }
            formData = new FormData();
            formData.top = new FormAttachment(0, parameterPositionOffset);
            formData.left = new FormAttachment(valueWidth, 5);
            label.setLayoutData(formData);
        }
    }
    
    private boolean isUserListParameter(ParameterData param)
    {
        if (_mbean.isAdmin() && param.getName().equals(OPERATION_PARAM_USERNAME)
                && !_opData.getName().equals(OPERATION_CREATEUSER))
        {
            return true;
        }
        
        return false;
    }
    
    /**
     * Creates customized dispaly for a method "CreateNewBinding" for Headers exchange
     *
     */
    private void customCreateNewBinding()
    {
        headerBindingHashMap = new HashMap<Text, Text>();
 
        _paramsComposite.setLayout(new GridLayout());
        _paramsComposite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, true));
        final ScrolledComposite scrolledComposite = new ScrolledComposite(_paramsComposite, SWT.BORDER | SWT.V_SCROLL);
        scrolledComposite.setExpandHorizontal(true);
        scrolledComposite.setExpandVertical(true);   
        GridData layoutData = new GridData(SWT.FILL, SWT.TOP, true, true);
        scrolledComposite.setLayoutData(layoutData);
        scrolledComposite.setLayout(new GridLayout());
        
        final Composite composite = _toolkit.createComposite(scrolledComposite, SWT.NONE);
        scrolledComposite.setContent(composite);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);            
        layoutData.verticalIndent = 20;
        composite.setLayoutData(layoutData);
        composite.setLayout(new FormLayout());
        
        List<ParameterData> params = _opData.getParameters();
        ParameterData param = params.get(0);
        // Queue selection widget
        Label label = _toolkit.createLabel(composite, ViewUtility.getDisplayText(param.getName()));
        FormData formData = new FormData();
        formData.top = new FormAttachment(0, 2);
        formData.right = new FormAttachment(labelWidth);
        label.setLayoutData(formData);
        label.setToolTipText(param.getDescription());
        
        formData = new FormData();
        formData.top = new FormAttachment(0);
        formData.left = new FormAttachment(label, 5);
        formData.right = new FormAttachment(valueWidth);

        Combo combo = new Combo(composite, SWT.READ_ONLY | SWT.DROP_DOWN);
        List<String> qList = ApplicationRegistry.getServerRegistry(_mbean).getQueueNames(_virtualHostName);
        combo.setItems(qList.toArray(new String[0]));
        combo.add("Select Queue", 0); 
        combo.select(0);
        combo.setLayoutData(formData);
        combo.setData(param);
        combo.addSelectionListener(parameterSelectionListener);

        // Binding creation widgets
        createARowForCreatingHeadersBinding(composite, 1);
        createARowForCreatingHeadersBinding(composite, 2);
        createARowForCreatingHeadersBinding(composite, 3);
        createARowForCreatingHeadersBinding(composite, 4);
        createARowForCreatingHeadersBinding(composite, 5);
        createARowForCreatingHeadersBinding(composite, 6);
        createARowForCreatingHeadersBinding(composite, 7);
        createARowForCreatingHeadersBinding(composite, 8);
        
        final Button addMoreButton = _toolkit.createButton(composite, "Add More", SWT.PUSH);
        formData = new FormData();
        formData.top = new FormAttachment(0, heightForAParameter);
        formData.left = new FormAttachment(70, 5);
        addMoreButton.setLayoutData(formData);
        addMoreButton.setData("rowCount", 8);
        addMoreButton.addSelectionListener(new SelectionAdapter()
            {
                public void widgetSelected(SelectionEvent e)
                {
                    int count = Integer.parseInt(addMoreButton.getData("rowCount").toString());
                    createARowForCreatingHeadersBinding(composite, ++count);
                    addMoreButton.setData("rowCount", count);
                    scrolledComposite.setMinSize(composite.computeSize(SWT.DEFAULT, SWT.DEFAULT));
                    composite.layout();
                    _form.layout();
                }
            });
          
        scrolledComposite.setMinSize(composite.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        composite.layout();
    }
    
    /**
     * Adds a row for adding a binding for Headers Exchange. Used by the method, which creates the customized
     * layout and widgest for Header's exchange method createNewBinding.
     * @param parent composite
     * @param rowCount - row number
     */
    private void createARowForCreatingHeadersBinding(Composite parent, int rowCount)
    {  
        Label key = _toolkit.createLabel(parent, "Name");
        FormData formData = new FormData();
        formData.top = new FormAttachment(0, rowCount * heightForAParameter + 2);
        formData.right = new FormAttachment(15);
        key.setLayoutData(formData);
        
        Text keyText = _toolkit.createText(parent, "", SWT.NONE);
        formData = new FormData();
        formData.top = new FormAttachment(0, rowCount * heightForAParameter);
        formData.left = new FormAttachment(key, 5);
        formData.right = new FormAttachment(40);
        keyText.setLayoutData(formData);
        keyText.addKeyListener(headerBindingListener);
        
        Label value = _toolkit.createLabel(parent, "Value");
        formData = new FormData();
        formData.top = new FormAttachment(0, rowCount * heightForAParameter + 2);
        formData.right = new FormAttachment(45);
        value.setLayoutData(formData);
        
        Text valueText = _toolkit.createText(parent, "", SWT.NONE);
        formData = new FormData();
        formData.top = new FormAttachment(0, rowCount * heightForAParameter);
        formData.left = new FormAttachment(value, 5);
        formData.right = new FormAttachment(70);
        valueText.setLayoutData(formData);
        valueText.addKeyListener(headerBindingListener);
        
        // Add these to the map, to retrieve the values while setting the parameter value
        headerBindingHashMap.put(keyText, valueText);
    }
    
    /**
     * Sets text and listener for the operation execution button
     * @param text
     */
    private void setButton(String text)
    {
        _executionButton.setText(text);
        _executionButton.removeSelectionListener(refreshListener);
        _executionButton.removeSelectionListener(operationExecutionListener);
        
        if (BUTTON_EXECUTE.equals(text))
        {
            _executionButton.addSelectionListener(operationExecutionListener);    
        }
        else
        {
            _executionButton.addSelectionListener(refreshListener);
        }
    }   

    /**
     * displays the operation result in a pop-up window
     * @param result
     */
    private void populateResults(Object result)
    {
        Display display = Display.getCurrent();
        int width = 610;
        int height = 400;
        Shell shell = ViewUtility.createPopupShell(RESULT, width, height);
        shell.setImage(ApplicationRegistry.getImage(CONSOLE_IMAGE));
        ViewUtility.populateCompositeWithData(_toolkit, shell, result);
        
        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        shell.dispose();
    }
    
    /**
     * Clears the parameter values of the operation
     */
    private void clearParameters()
    {
        List<ParameterData> params = _opData.getParameters();
        if (params != null && !params.isEmpty())
        {
            for (ParameterData param : params)
            {
                param.setDefaultValue();
            }
        }
    }
    
    /**
     * Clears the values entered by the user from parameter value widgets
     * @param control
     */
    private void clearParameterValues(Composite control)
    {
        if (control == null || (control.isDisposed()))
            return;
        
        Control[] controls = control.getChildren();
        if (controls == null || controls.length == 0)
            return;
        
        for (int i = 0; i < controls.length; i++)
        {
            if (controls[i] instanceof Combo)
                ((Combo)controls[i]).select(0);
            if (controls[i] instanceof org.eclipse.swt.widgets.List)
                ((org.eclipse.swt.widgets.List)controls[i]).deselectAll();
            else if (controls[i] instanceof Text)
                ((Text)controls[i]).setText("");
            else if (controls[i] instanceof Button)
                ((Button)controls[i]).setSelection(false);
            else if (controls[i] instanceof Composite)
                clearParameterValues((Composite)controls[i]);
        }
    }
    
    /**
     * Listener class for operation execution events
     */
    private class OperationExecutionListener extends SelectionAdapter
    {
        public void widgetSelected(SelectionEvent e)
        {
            List<ParameterData> params = _opData.getParameters();
            if (params != null && !params.isEmpty())
            {
                for (ParameterData param : params)
                { 
                    if (param.getValue() == null || param.getValue().toString().length() == 0)
                    {
                        // Customized check, because for this parameter null is allowed
                        if (param.getName().equals(ATTRIBUTE_QUEUE_OWNER) &&
                            _opData.getName().equals(OPERATION_CREATE_QUEUE))
                        {
                            continue;
                        }
                        // End of custom code
                        
                        ViewUtility.popupInfoMessage(_form.getText(), "Please select the " + ViewUtility.getDisplayText(param.getName()));                       
                        return;
                    }
                    
                    //Custom handling for the PASSWORD field
                    if (param.getName().equalsIgnoreCase(PASSWORD))
                    {
                        //Convert the String value to a character array if that is what is required.
                        if (param.getType().equals("[C"))
                        {
                            // Retreive the mBean type and version.
                            // If we have a version 1 UserManagement class mbean then it expects the password
                            // to be sent as the hashed version.
                            if (_mbean.getType().equals("UserManagement") && _mbean.getVersion() == 1)
                            {
                                try
                                {
                                    param.setValue(ViewUtility.getHash((String) param.getValue()));
                                }
                                catch (Exception hashException)
                                {
                                    ViewUtility.popupErrorMessage(_form.getText(),
                                            "Unable to calculate hash for Password:"
                                            + hashException.getMessage());
                                    return;
                                }
                            }
                            else
                            {
                                param.setValue(((String) param.getValue()).toCharArray());
                            }
                        }
                    }
                    // end of customization

                }
            }
            
            if (_opData.getImpact() == OPERATION_IMPACT_ACTION)
            {
                String bean = _mbean.getName() == null ? _mbean.getType() : _mbean.getName();
                int response = ViewUtility.popupConfirmationMessage(bean, "Do you want to " + _form.getText()+ " ?");
                if (response == SWT.YES)
                {
                    executeAndShowResults();
                }            
            }
            else
            {
                executeAndShowResults();
            }
            
            if (_mbean.isAdmin() && _opData.getName().equals(OPERATION_DELETEUSER))
            {
                refresh(_mbean);
            }
            else
            {
                clearParameters();
                clearParameterValues(_paramsComposite);
            }
        }
    }
    
    // Listener for the "Refresh" execution button
    private class RefreshListener extends SelectionAdapter
    {
        public void widgetSelected(SelectionEvent e)
        {
            executeAndShowResults();
        }
    }
    
    /**
     * Executres the operation, gets the result from server and displays to the user
     */
    private void executeAndShowResults()
    {
        Object result = null;
        try
        {
            result = MBeanUtility.execute(_mbean, _opData);     
        }
        catch(Exception ex)
        {
            MBeanUtility.handleException(_mbean, ex);
            return;
        }
        
        // Custom code for Admin mbean operation
        /* These custome codes here are to make the GUI look more user friendly. 
         * Here we are adding the users to a list, which will be used to list username to be selected on
         * pages like "delete user", "set password" instead of typing the username
        */
        if (_mbean.isAdmin())
        {
            if (_opData.getName().equals(OPERATION_VIEWUSERS))
            {
                ApplicationRegistry.getServerRegistry(_mbean).setUserList(extractUserList(result));
            }
            else if (_opData.getName().equals(OPERATION_DELETEUSER))
            {
                List<String> list = ApplicationRegistry.getServerRegistry(_mbean).getUsernames();
                Object userName = _opData.getParameterValue(OPERATION_PARAM_USERNAME);
                if ((list != null) && !list.isEmpty() && (userName != null))
                {
                    list.remove(userName);
                    ApplicationRegistry.getServerRegistry(_mbean).setUserList(list);
                }                
            }
            else if (_opData.getName().equals(OPERATION_CREATEUSER))
            {
                List<String> list = ApplicationRegistry.getServerRegistry(_mbean).getUsernames();
                Object userName = _opData.getParameterValue(OPERATION_PARAM_USERNAME);
                if ((list != null) && !list.isEmpty() && (userName != null))
                {
                    list.add(userName.toString());
                    ApplicationRegistry.getServerRegistry(_mbean).setUserList(list);
                }                
            }
        }
        // end of custom code
        
        // Some mbeans have only "type" and no "name".
        String title = _mbean.getType();
        if (_mbean.getName() != null && _mbean.getName().length() != 0)
        {
            title = _mbean.getName();
        }
        
        if (_opData.isReturnTypeVoid())
        {
            ViewUtility.popupInfoMessage(title, OPERATION_SUCCESSFUL);
        }
        else if (_opData.isReturnTypeBoolean())
        {
            boolean success = Boolean.parseBoolean(result.toString());
            String message = success ? OPERATION_SUCCESSFUL : OPERATION_UNSUCCESSFUL;
            if(success)
            {
                ViewUtility.popupInfoMessage(title, message);
            }
            else
            {
                ViewUtility.popupErrorMessage(title, message);
            }
        }
        else if (_opData.getParameters() != null && !_opData.getParameters().isEmpty())
        {
            populateResults(result);
        }
        else
        {
            ViewUtility.disposeChildren(_resultsComposite);
            ViewUtility.populateCompositeWithData(_toolkit, _resultsComposite, result);
            _resultsComposite.layout();
            _form.layout();
        }

    }
    
    private List<String> extractUserList(Object result)
    {
        if (!(result instanceof TabularDataSupport))
        {
            return null;
        }
        
        TabularDataSupport tabularData = (TabularDataSupport)result;
        Collection<Object> records = tabularData.values();
        List<String> list = new ArrayList<String>();
        for (Object o : records)
        {
            CompositeData data = (CompositeData) o;
            if (data.containsKey(USERNAME))
            {
                list.add(data.get(USERNAME).toString());
            }
        }
        
        return list;
    }
    
    /**
     * Listener class for the operation parameters widget
     */
    private class ParameterSelectionListener extends SelectionAdapter
    {
        public void widgetSelected(SelectionEvent e)
        {
            ParameterData parameter = (ParameterData)e.widget.getData();
            parameter.setValue(null);
            if (e.widget instanceof Combo)
            {
                Combo combo = (Combo)e.widget;
                if (combo.getSelectionIndex() > 0)
                {
                    String item = combo.getItem(combo.getSelectionIndex());                
                    parameter.setValueFromString(item);
                }
            }
            else if (e.widget instanceof org.eclipse.swt.widgets.List)
            {
                org.eclipse.swt.widgets.List list = (org.eclipse.swt.widgets.List)e.widget;
                String[] selectedItems = list.getSelection();
                if (selectedItems.length > 0)
                {
                    parameter.setValueFromString(selectedItems[0]);
                }
            }
        }
    }
    
    /**
     * Listener class for boolean parameter widgets
     */
    private class BooleanSelectionListener extends SelectionAdapter
    {
        public void widgetSelected(SelectionEvent e)
        {
            ParameterData parameter = (ParameterData)(e.widget.getData());
            if (e.widget instanceof Button)
            {
                Button button = (Button)e.widget;
                parameter.setValue(button.getSelection());
            }
            else if (e.widget instanceof Combo)
            {
                Combo combo = (Combo)e.widget;
                String item = combo.getItem(combo.getSelectionIndex());                
                parameter.setValueFromString(item);
            }
        }
    }
    
    /**
     * Listener class for the operation parameter value widget (Text field)
     */
    private class KeyListenerImpl extends KeyAdapter
    {
        public void keyReleased(KeyEvent e) 
        {
            if (!(e.widget instanceof Text))
                return;
            
            Text text = (Text)e.widget;
            // Get the parameters widget and assign the text to the parameter
            String strValue = text.getText();
            ParameterData parameter = (ParameterData)text.getData();
            try
            {
                parameter.setValueFromString(strValue);
            }
            catch(Exception ex)
            {
                // Exception occured in setting parameter value. 
                // ignore it. The value will not be assigned to the parameter
            }
        }
    }
    
    /**
     * Listener class for HeaderExchange's new binding widgets. Used when the new bindings are 
     * being created for Header's Exchange
     */
    private class HeaderBindingKeyListener extends KeyAdapter
    {
        public void keyReleased(KeyEvent e) 
        {
            ParameterData param = _opData.getParameters().get(1);
            StringBuffer paramValue = new StringBuffer();
            for (Entry<Text, Text> entry : headerBindingHashMap.entrySet())
            {
                
                Text nameText = entry.getKey();
                String name = nameText.getText();
                Text valueText = entry.getValue();
                String value = valueText.getText();
                if ((name != null) && (name.length() != 0) && (value != null) && (value.length() != 0))
                {
                    if (paramValue.length() != 0)
                    {
                        paramValue.append(",");
                    }
                    paramValue.append(name + "=" + value);
                }
            }
            
            param.setValue(paramValue.toString());
        }
    }
    
    /**
     * Listener class for verifying the user input with parameter type
     */
    private class VerifyListenerImpl implements VerifyListener
    {
        public void verifyText(VerifyEvent event)
        {
            ParameterData parameter = (ParameterData)event.widget.getData();
            String text = event.text;
            char [] chars = new char [text.length ()];
            text.getChars(0, chars.length, chars, 0);           
            String type = parameter.getType();
            if (type.equals("int") || type.equals("java.lang.Integer") ||
                type.equals("long") || type.equals("java.lang.Long"))
            {
                for (int i=0; i<chars.length; i++)
                {
                    if (!('0' <= chars [i] && chars [i] <= '9'))
                    {
                        event.doit = false;
                        return;
                    }
                }
                
            }
        }
    }
    
}
