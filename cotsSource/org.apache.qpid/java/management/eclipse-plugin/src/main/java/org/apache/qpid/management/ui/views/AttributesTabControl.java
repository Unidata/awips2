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

import static org.apache.qpid.management.ui.Constants.*;

import org.apache.qpid.management.ui.ApplicationRegistry;
import org.apache.qpid.management.ui.ManagedBean;
import org.apache.qpid.management.ui.jmx.JMXServerRegistry;
import org.apache.qpid.management.ui.jmx.MBeanUtility;
import org.apache.qpid.management.ui.model.AttributeData;
import org.apache.qpid.management.ui.model.ManagedAttributeModel;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;


/**
 * Creates controller composite for the attribute's tab. 
 * @author Bhupendra Bhardwaj
 */
public class AttributesTabControl extends TabControl
{    
    private FormToolkit  _toolkit;
    private ScrolledForm _form;    
    private Table _table = null;
    private TableViewer _tableViewer = null;
    private int[] tableWidths = new int[] {275, 275};
    
    private Composite _tableComposite = null;
    private Composite _buttonsComposite = null;
    
    private DisposeListener tableDisposeListener = new DisposeListenerImpl();
    final Image image;
    private Button _detailsButton  = null;
    private Button _editButton  = null;
    private Button _graphButton = null;
    private boolean disableEditing = false;
    
    private static final String MAX_VALUE = "MaxValue";
    private static final String GRAPH_VALUES = "GraphValues";
    private int GRAPH_WIDTH = 700;
    private int GRAPH_HEIGHT = 450;
    private int GRAPH_ITEM_GAP = 100;
    private int startX = 80;
    private int startY = 60;
    
    public AttributesTabControl(TabFolder tabFolder)
    {
        super(tabFolder);
        _toolkit = new FormToolkit(_tabFolder.getDisplay());
        _form = _toolkit.createScrolledForm(_tabFolder);
        GridLayout gridLayout = new GridLayout(2, false);      
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;       
        _form.getBody().setLayout(gridLayout);
        _tableComposite = _toolkit.createComposite(_form.getBody());
        _tableComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        _tableComposite.setLayout(new GridLayout());
        _buttonsComposite = _toolkit.createComposite(_form.getBody());
        _buttonsComposite.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, true));
        _buttonsComposite.setLayout(new GridLayout());
        
        image = Display.getCurrent().getSystemImage(SWT.ICON_INFORMATION);
        createWidgets();         
    }
    
    /**
     * @see TabControl#getControl()
     */
    public Control getControl()
    {
        return _form;
    }
    
    /**
     * Creates required widgets for Attribute's tab
     */
    protected void createWidgets()
    {
        createTable();
        createTableViewer();                
        createButtons();        
        addTableListeners();        
    }
    
    /**
     * Creates table for listing the MBean attributes
     */
    private void createTable()
    {  
        _table = _toolkit.createTable(_tableComposite,  SWT.FULL_SELECTION); 
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        _table.setLayoutData(gridData);
        
        for (int i = 0; i < ATTRIBUTE_TABLE_TITLES.length; ++i)
        {
            final TableColumn column = new TableColumn(_table, SWT.NONE);
            column.setText(ATTRIBUTE_TABLE_TITLES[i]);
            column.setWidth(tableWidths[i]);
            column.setResizable(true);
        }
        
        _table.setLinesVisible (true);
        _table.setHeaderVisible (true);
    }
    
    /**
     * Creates tableviewer for the attribute's table
     */
    private void createTableViewer()
    {
        _tableViewer = new TableViewer(_table);
        _tableViewer.setUseHashlookup(true);
        _tableViewer.setColumnProperties(ATTRIBUTE_TABLE_TITLES);
        _tableViewer.setContentProvider(new ContentProviderImpl());
        _tableViewer.setLabelProvider(new LabelProviderImpl());
        _tableViewer.setSorter(new ViewerSorterImpl());
    }
    
    private void createButtons()
    {
        addDetailsButton();
        addEditButton();
        addGraphButton();
    }
      
    private void addDetailsButton()
    {
        // Create and configure the button for attribute details
        _detailsButton = _toolkit.createButton(_buttonsComposite, BUTTON_DETAILS, SWT.PUSH | SWT.CENTER);
        _detailsButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        GridData gridData = new GridData(SWT.FILL, SWT.TOP, false, false);
        gridData.widthHint = 80;
        _detailsButton.setLayoutData(gridData);
        _detailsButton.addSelectionListener(new SelectionAdapter()
            {
                public void widgetSelected(SelectionEvent e)
                {
                    disableEditing = true;
                    int index = _table.getSelectionIndex();
                    TableItem item = _table.getItem(index);                   
                    createDetailsPopup((AttributeData)item.getData());
                    disableEditing = false;
                    setFocus();
                }
            });
    }
    
    /**
     * Creates the button for editing attributes.
     */
    private void addEditButton()
    {
        // Create and configure the button for editing attribute
        _editButton = _toolkit.createButton(_buttonsComposite, BUTTON_EDIT_ATTRIBUTE, SWT.PUSH | SWT.CENTER);
        _editButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        GridData gridData = new GridData(SWT.FILL, SWT.TOP, false, false);
        gridData.widthHint = 80;
        _editButton.setLayoutData(gridData);
        _editButton.addSelectionListener(new SelectionAdapter()
            {
                public void widgetSelected(SelectionEvent e)
                {
                    int index = _table.getSelectionIndex();
                    TableItem item = _table.getItem(index);
                    createDetailsPopup((AttributeData)item.getData());
                    setFocus();
                }
            });
    }
    
    /**
     * Creates the button for viewing Graphs
     */ 
    private void addGraphButton()
    {
        _graphButton = _toolkit.createButton(_buttonsComposite, BUTTON_GRAPH, SWT.PUSH | SWT.CENTER);
        _graphButton.setFont(ApplicationRegistry.getFont(FONT_BUTTON));
        GridData gridData = new GridData(SWT.FILL, SWT.TOP, false, false);
        gridData.widthHint = 80;
        _graphButton.setLayoutData(gridData);
        _graphButton.addSelectionListener(new SelectionAdapter()
            {
                public void widgetSelected(SelectionEvent event)
                {
                    int selectionIndex = _table.getSelectionIndex();
                    AttributeData data = (AttributeData)_table.getItem(selectionIndex).getData();
                    createGraph(data);
                    setFocus();
                }
            });
    }

    private void addTableListeners()
    {
        _tableViewer.addSelectionChangedListener(new ISelectionChangedListener(){
            public void selectionChanged(SelectionChangedEvent evt)
            {
                IStructuredSelection ss = (IStructuredSelection)evt.getSelection();
                checkForEnablingButtons((AttributeData)ss.getFirstElement());
            }
        });
        
        MouseListenerImpl listener = new MouseListenerImpl();
        _tableViewer.getTable().addMouseTrackListener(listener);
        _tableViewer.getTable().addMouseMoveListener(listener);
        _tableViewer.getTable().addMouseListener(listener);
        
        _table.addDisposeListener(tableDisposeListener);
        
        // _table is equal to _tableViewer.getControl()
        _table.addListener(SWT.MeasureItem, new Listener() {  
            public void handleEvent(Event event)
            {    
                event.height = event.gc.getFontMetrics().getHeight()  * 3/2;
            }  
        }); 
    }
    
    /**
     * Listeners implementation class for showing table tooltip
     * @author Bhupendra Bhardwaj
     */
    private class MouseListenerImpl implements MouseTrackListener, MouseMoveListener, KeyListener, MouseListener                                              
    {
        Shell tooltipShell = null;
        Label tooltipLabel = null;
        public void mouseHover(MouseEvent event)
        {
            TableItem item = _table.getItem (new Point (event.x, event.y));
            
            if (item != null)
            {
                AttributeData data = (AttributeData)item.getData();
                if (tooltipShell != null  && !tooltipShell.isDisposed ()) tooltipShell.dispose ();
                tooltipShell = new Shell(_table.getShell(), SWT.ON_TOP | SWT.NO_FOCUS | SWT.TOOL);
                tooltipShell.setBackground(event.display.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
                FillLayout layout = new FillLayout();
                layout.marginWidth = 2;
                tooltipShell.setLayout(layout);
                tooltipLabel = new Label(tooltipShell, SWT.NONE);
                tooltipLabel.setForeground(event.display.getSystemColor(SWT.COLOR_INFO_FOREGROUND));
                tooltipLabel.setBackground(event.display.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
                tooltipLabel.setText(data.getDescription());
                tooltipLabel.setData("_TABLEITEM", item);
                tooltipLabel.addListener(SWT.MouseExit, tooltipLabelListener);
                tooltipLabel.addListener(SWT.MouseDown, tooltipLabelListener);
                Point size = tooltipShell.computeSize(SWT.DEFAULT, SWT.DEFAULT);
                Rectangle rect = item.getBounds(0);
                Point pt = _table.toDisplay(rect.x, rect.y);
                tooltipShell.setBounds(pt.x, pt.y, size.x, size.y);
                tooltipShell.setVisible(true);
            }
        }
        public void mouseEnter(MouseEvent e)
        {
        }
        public void mouseExit(MouseEvent e)
        {
        }
        
        // MouseMoveListener implementation
        public void mouseMove(MouseEvent event)
        {
            if (tooltipShell == null)
                return;
            
            tooltipShell.dispose();
            tooltipShell = null;
            tooltipLabel = null;
        }
        
        // KeyListener implementation
        public void keyPressed(KeyEvent e)
        {
            if (tooltipShell == null)
                return;
            
            tooltipShell.dispose();
            tooltipShell = null;
            tooltipLabel = null;
        }     
        public void keyReleased(KeyEvent e)
        {
            
        }
        
        // MouseListener implementation
        public void mouseDoubleClick(MouseEvent event)
        {
            if (tooltipShell != null)
            {
                tooltipShell.dispose();
                tooltipShell = null;
                tooltipLabel = null;
            }
            Table table = (Table)event.getSource();
            int selectionIndex = table.getSelectionIndex();
            AttributeData data = (AttributeData)table.getItem(selectionIndex).getData();
            createDetailsPopup(data);
        }
        public void mouseDown(MouseEvent e)
        {
            if (tooltipShell != null)
            {
                tooltipShell.dispose();
                tooltipShell = null;
                tooltipLabel = null;
            }
        }
        public void mouseUp(MouseEvent e)
        {
            
        }
    } // end of MouseListenerImpl
    
    /**
     * Creates pop-up window for showing attribute details
     * @param data - Selectes attribute
     */
    public void createDetailsPopup(AttributeData data)
    {
        int width = 500;
        int height = 250;
        if (!isSimpleType(data.getValue()))
        {
            width = 650;
            height = 450;
        }
        
        Display display = Display.getCurrent();
        Shell shell = ViewUtility.createPopupShell(ATTRIBUTE, width, height);
        createDetailsPopupContents(shell, data);

        shell.open();
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch())
            {
                display.sleep();
            }
        }
        shell.dispose();
    }
    
    /**
     * Listener class for table tooltip label
     */
    final Listener tooltipLabelListener = new Listener ()
    {
        public void handleEvent (Event event)
        {
            Label label = (Label)event.widget;
            Shell shell = label.getShell();
            switch (event.type)
            {
                case SWT.MouseDown:
                    Event e = new Event();
                    e.item = (TableItem)label.getData ("_TABLEITEM");
                    _table.setSelection(new TableItem[] {(TableItem)e.item});
                    shell.dispose();
                    _table.setFocus();
                    break;
                case SWT.MouseExit:
                    shell.dispose();
                    break;
            }
        }
    };
    
    
    /**
     * Create the contents for the attribute details window pop-up
     * @param shell - The shell that will be filled with details.
     * @param attribute - Selected attribute
     */
    private void createDetailsPopupContents(Composite shell, AttributeData attribute)
    {
        GridLayout layout = new GridLayout(2, false);
        layout.horizontalSpacing = 10;
        layout.verticalSpacing = 10;
        layout.marginHeight = 20;
        layout.marginWidth = 20;
        
        Composite parent = _toolkit.createComposite(shell, SWT.NONE);
        parent.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        parent.setLayout(layout);

        // Name
        Label label = _toolkit.createLabel(parent, ATTRIBUTE_TABLE_TITLES[0], SWT.NONE);       
        GridData layoutData = new GridData(SWT.TRAIL, SWT.TOP, false, false);
        label.setLayoutData(layoutData);
        int textStyle = SWT.BEGINNING | SWT.BORDER |SWT.READ_ONLY;
        Text  value = _toolkit.createText(parent, ViewUtility.getDisplayText(attribute.getName()), textStyle);
        value.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

        
        // Description
        label = _toolkit.createLabel(parent, DESCRIPTION, SWT.NONE);
        label.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, false, false));
        value = _toolkit.createText(parent, attribute.getDescription(), textStyle);
        value.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        
        // value
        label = _toolkit.createLabel(parent, ATTRIBUTE_TABLE_TITLES[1], SWT.NONE);
        label.setLayoutData(new GridData(SWT.TRAIL, SWT.TOP, false, false));
        
        if (!attribute.isReadable())
        {
            value = _toolkit.createText(parent, "", textStyle);
            value.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
        }
        else
        {
            if (!isSimpleType(attribute.getValue()))
            {
                if (attribute.getValue() instanceof String[])
                {
                    String result = new String("");
                    for(String val : (String[]) attribute.getValue()){
                        result = result.concat(val+ "; ");
                    }
                    value = _toolkit.createText(parent, "", textStyle);
                    
                    value.setText(result);
                    value.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
                }
                else
                {
                    Composite composite = new Composite(parent, SWT.BORDER);
                    composite.setLayout(new GridLayout());
                    composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
                    ViewUtility.populateCompositeWithData(_toolkit, composite, attribute.getValue());
                }
            }
            else
            {
                if (attribute.isWritable())
                {
                    value = _toolkit.createText(parent, "", SWT.BEGINNING | SWT.BORDER);
                    value.addVerifyListener(new NumberVerifyListener());
                    
                    // set data to access in the listener
                    parent.setData(attribute);
                }
                else
                {
                    value = _toolkit.createText(parent, "", textStyle);
                }
                
                value.setText(attribute.getValue().toString());
                value.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
            }
        }  
        
        
        // Update button
        Button updateButton = addUpdateButton(parent);
        updateButton.setData(value);
        if (!attribute.isWritable())
        {
            updateButton.setVisible(false);
        }
        
        if (disableEditing)
        {
            value.setEditable(false);
            updateButton.setVisible(false);
        }
    }
    
    /**
     * Create the button for updating attributes.  This should be enabled for writable attribute
     */
    private Button addUpdateButton(Composite parent)
    {
        final Button updateButton = new Button(parent, SWT.PUSH | SWT.CENTER);
        // set the data to access in the listener
        parent.setData(BUTTON_UPDATE, updateButton);
        
        updateButton.setText(BUTTON_UPDATE);
        GridData gridData = new GridData (SWT.CENTER, SWT.BOTTOM, true, true, 2, 1);
        gridData.widthHint = 100;
        updateButton.setLayoutData(gridData);
        updateButton.addSelectionListener(new SelectionAdapter()
            {
                public void widgetSelected(SelectionEvent event)
                {
                    try
                    {
                        Button button = (Button)event.widget;
                        Text text = (Text)button.getData();
                        AttributeData data = (AttributeData)button.getParent().getData();
                        MBeanUtility.updateAttribute(_mbean, data, text.getText());
                        button.getShell().close();
                        refresh(_mbean);
                    }
                    catch (Exception ex)
                    {
                        MBeanUtility.handleException(_mbean, ex);
                    }
                }
            });
        
        return updateButton;
    }    

    /**
     * Refreshes the attribute tab by querying the mbean server for latest values
     */ 
    @Override
    public void refresh(ManagedBean mbean) 
    {
        _mbean = mbean;        
        if (_mbean == null)
        {
            _tableViewer.setInput(null);
            return;
        }
        ManagedAttributeModel attributesList = null;
        try
        {
            attributesList = MBeanUtility.getAttributes(mbean);
        }
        catch(Exception ex)
        {
            MBeanUtility.handleException(_mbean, ex);
        }
        _tableViewer.setInput(attributesList);
        checkForEnablingButtons(getSelectionAttribute());
        
        _form.layout(true);
        _form.getBody().layout(true, true);
    }
    
    /**
     * @see TabControl#setFocus()
     */
    public void setFocus()
    {
        _table.setFocus();
    }
    
    /**
     * Checks which buttons are to be enabled or disabled. The graph button will be enabled only
     * for readable number attributes.  Editing is enabled for writeable attribtues.
     * @param attribute
     */
    private void checkForEnablingButtons(AttributeData attribute)
    {
        if (attribute == null)
        {
            _detailsButton.setEnabled(false);
            _editButton.setEnabled(false);
            _graphButton.setEnabled(false);
            return;
        }
        
        _detailsButton.setEnabled(true);
        if (attribute.isWritable())
        {
            _editButton.setEnabled(true);
            _graphButton.setEnabled(false);
        }
        else
        {
            _editButton.setEnabled(false);
            // Currently only Queues are having attributes, which are suitable for a graph
            if (attribute.isNumber() && _mbean.isQueue())
            {
                _graphButton.setEnabled(true);
            }
            else
            {
                _graphButton.setEnabled(false);
            }
        }
    }
    
    /**
     * Creates graph in a pop-up window for given attribute.
     * @param data
     */
    private void createGraph(final AttributeData data)
    {       
        Display display = Display.getCurrent();        
        Shell shell = new Shell(display, SWT.BORDER | SWT.CLOSE | SWT.MIN | SWT.MAX);
        shell.setText(_mbean.getName());
        int x = display.getBounds().width;
        int y = display.getBounds().height;
        shell.setBounds(x/4, y/4, GRAPH_WIDTH, GRAPH_HEIGHT);
        shell.setLayout(new FillLayout());
        
        final Canvas canvas = new Canvas(shell, SWT.NONE);
        long currentValue = Long.parseLong(data.getValue().toString());
        long mValue = getGraphMaxValue(currentValue);
        canvas.setData(MAX_VALUE, mValue); 
        canvas.setData(GRAPH_VALUES, new long[] {0,0,0,0,0,currentValue});
        
        canvas.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));
        canvas.addPaintListener(new PaintListener()
            {
                public void paintControl(PaintEvent event)
                {
                    Canvas canvas = (Canvas)event.widget;
                    int maxX = canvas.getSize().x;
                    int maxY = canvas.getSize().y;
                    event.gc.fillRectangle(canvas.getBounds());
                    event.gc.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));
                    event.gc.setLineWidth(4);
                    
                    Object canvasData = canvas.getData(MAX_VALUE);
                    String str = canvasData.toString();
                    long maxValue = Long.parseLong(str);
                    // Set the graph dimensions
                    event.gc.drawText("0", startX - 40, maxY - startY - 10);
                    event.gc.drawText("" + maxValue/2, startX - 40, maxY/2);
                    event.gc.drawText("" + maxValue, startX - 40, startY);
                    
                    // horizontal line
                    event.gc.drawLine(startX, maxY - startY, maxX - 60, maxY - startY);
                    // vertical line
                    event.gc.drawLine(startX, maxY - startY, startX, startY);
                    // set graph text
                    event.gc.drawText(data.getName(), startX - 40, startY - 40);
                    event.gc.drawText("25 sec", startX, maxY - startY + 10);
                    event.gc.drawText("20 sec", startX + GRAPH_ITEM_GAP, maxY - startY + 10);
                    event.gc.drawText("15 sec", startX + GRAPH_ITEM_GAP * 2, maxY - startY + 10);
                    event.gc.drawText("10 sec", startX + GRAPH_ITEM_GAP * 3, maxY - startY + 10);
                    event.gc.drawText(" 5 sec", startX + GRAPH_ITEM_GAP * 4, maxY - startY + 10);
                    event.gc.drawText(" 0 sec", startX + GRAPH_ITEM_GAP * 5, maxY - startY + 10);
                    
                    // plot the graph now for values
                    event.gc.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));
                    canvasData = canvas.getData(GRAPH_VALUES);
                    long[] graphValues = (long[]) canvasData;
                    for (int i = 0; i < graphValues.length; i++)
                    {
                        int x = startX + i * GRAPH_ITEM_GAP;
                        int yTotalLength = (maxY - 2 * startY);
                        float ratio = ((float)graphValues[i]/(float)maxValue);
                        int itemlength = (int)(yTotalLength *  ratio);
                        int y = maxY - startY - itemlength;
                        event.gc.drawLine(x, maxY- startY, x, y);
                        event.gc.drawText(String.valueOf(graphValues[i]), x, y - 20);
                    }
                }            
            });
        
        shell.open();
        
        // Set up the timer for the animation
        Runnable runnable = new Runnable()
        {
          public void run()
          {
            try
            {
                animate(canvas, data);
                Display.getCurrent().timerExec(TIMER_INTERVAL, this);
            }
            catch(Exception ex)
            {
                MBeanUtility.handleException(ex);
            }
          }
        };

        // Launch the timer
        display.timerExec(TIMER_INTERVAL, runnable);
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch())
            {
                display.sleep();
            }
        }
        
        // Kill the timer
        display.timerExec(-1, runnable);
        shell.dispose();
    }
    
    /**
     * @return selected attribute in the table
     */
    public AttributeData getSelectionAttribute()
    {
        int index = _table.getSelectionIndex();
        if (index == -1)
            return null;
        
        return (AttributeData)_table.getItem(index).getData();
    }
    
    /**
     * checks for newer values of selected attribute to update the graph
     * @param canvas
     * @param data
     * @throws Exception
     */
    private void animate(Canvas canvas, AttributeData data) throws Exception
    {
        String attribute = data.getName();
        Object valueObj = MBeanUtility.refreshAttribute(_mbean, attribute);
        int value = Integer.parseInt(String.valueOf(valueObj));
        Object canvasData = canvas.getData(GRAPH_VALUES);
        long[] graphValues = (long[]) canvasData;
        
        for (int i = 0; i < graphValues.length -1; i++)
        {
            graphValues[i] = graphValues[i + 1];            
        }
        graphValues[graphValues.length - 1] = value;
        
        canvasData = canvas.getData(MAX_VALUE);
        long maxValue = Long.parseLong(String.valueOf(canvasData));
        if (maxValue < value)
        {
            maxValue = getGraphMaxValue(value);
            canvas.setData(MAX_VALUE, maxValue);
        }
        
        canvas.redraw();
    }
    
    /**
     * @param maxAttributeValue
     * @return dynamically calculated value for y-axis on the graph
     */
    private long getGraphMaxValue(long maxAttributeValue)
    {
        long maxGraphValue = 100;
        long temp = maxAttributeValue * 3/2;
        if (temp > maxGraphValue)
        {
            long modulus = temp % 100;
            maxGraphValue = temp + ( 100 - modulus);
        } 
        
        return maxGraphValue;
    }
    
    /**
     * Content Provider class for the table viewer
     * @author Bhupendra Bhardwaj
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
            return ((ManagedAttributeModel)parent).getAttributes();
        }
    }
    
    /**
     * Label Provider class for the table viewer
     * @author Bhupendra Bhardwaj
     */
    private class LabelProviderImpl extends LabelProvider implements ITableLabelProvider, 
                                                                     IFontProvider,
                                                                     IColorProvider
    {
        AttributeData attribute = null;
        public String getColumnText(Object element, int columnIndex)
        {
            String result = "";
            attribute = (AttributeData) element;
            
            switch (columnIndex)
            {
                case 0 : // attribute name column 
                    result = ViewUtility.getDisplayText(attribute.getName());
                    break;
                case 1 : // attribute value column 
                    if (attribute.getValue() != null)
                        if (attribute.getValue() instanceof String[])
                        {
                            for(String val : (String[]) attribute.getValue()){
                                result = result.concat(val+ "; ");
                            }
                        }
                        else
                        {
                            result = String.valueOf(attribute.getValue());
                        }
                    break;
                default :
                    result = "";
            }
            
            return result;
        }
        
        public Image getColumnImage(Object element, int columnIndex)
        {
            return null;
        }
        
        public Font getFont(Object element)
        {
            return ApplicationRegistry.getFont(FONT_TABLE_CELL);
        }
        
        public Color getForeground(Object element)
        {
            attribute = (AttributeData) element;
            if (attribute.isWritable())
                return Display.getCurrent().getSystemColor(SWT.COLOR_BLUE);
            else
                return Display.getCurrent().getSystemColor(SWT.COLOR_BLACK);
        }
        public Color getBackground(Object element)
        {
            return _form.getBackground();
        }
    }
    
    private class DisposeListenerImpl implements DisposeListener
    {
        public void widgetDisposed(DisposeEvent e)
        {
            
        }
    }

    /**
     * Sorter class for the table viewer. It sorts the table for according to attribute name.
     * @author Bhupendra Bhardwaj
     *
     */
    private class ViewerSorterImpl extends ViewerSorter
    {
        public int compare(Viewer viewer, Object o1, Object o2) 
        {
            AttributeData attribtue1 = (AttributeData)o1;
            AttributeData attribtue2 = (AttributeData)o2;
            
            return collator.compare(attribtue1.getName(), attribtue2.getName());
        }
    }
}
