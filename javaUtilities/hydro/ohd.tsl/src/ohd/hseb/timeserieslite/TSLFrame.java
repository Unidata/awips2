package ohd.hseb.timeserieslite;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;


import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.border.Border;

import ohd.hseb.gui.util.ScreenCaptureActionListener;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.util.ValueMapper;
import ohd.hseb.util.gui.DialogHelper;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.drawing.TsPaintableCanvas;
//------------------------------------------------------------------------------


public class TSLFrame extends JFrame
{
    //------------------------------------------------------------------------------
    private static final int MILLIS_PER_HOUR = 1000 * 60 * 60;  
    private static final int HOURS_TO_ROUND = 6;
 
    //window sizing variables
    private Dimension _minSize = new Dimension(250, 250);
    private Dimension _maxSize = new Dimension(1000, 1000);
    private Dimension _initialSize = new Dimension(500, 500);

    
    private JMenuBar _menuBar = null;  
    private JPanel _mainPanel = null;
    
    private TsPaintableCanvas _canvas = null;
    private ValueMapper _rightAxisValueMapper = null;
    
    private boolean _exitOnClose = false;
    
    //------------------------------------------------------------------------------
    
    public TSLFrame()
    { 
        initGui();
        
        return;
    }
   
    // ---------------------------------------------------------------------------------
    
    public void setTimeWindow(long startTime, long endTime)
    {
        _canvas.setTimeWindow(startTime, endTime);
        
        return;
    }
    
    //------------------------------------------------------------------------------
    public TsPaintableCanvas getCanvas()
    {
     
        return _canvas;
        
    }
    
    //------------------------------------------------------------------------------
    
    public void setExitOnClose(boolean exitOnClose)
    {
        _exitOnClose = exitOnClose;    
        
        return;
    }
    //------------------------------------------------------------------------------
       
    private void initGui()
    {
        setAlwaysOnTop(true);
        
        // canvas needs to be initialized before the menu bar,
        // because of the screen capturer
        initCanvas(0, 0,
                _initialSize.width, 
                _initialSize.height);
 
        
        this.setTitle("TimeSeriesLite");
        
        initMenuBar();
       
        //limit the range of the resizing
        // this object can be local because it remains in memory as a listener
        WindowResizingManager manager = new WindowResizingManager(this, _minSize, _maxSize);
             
        _mainPanel = new JPanel();
        _mainPanel.setBackground(Color.red);
 
        setLayout();
                       
        addListeners();
        
        this.getContentPane().add(_mainPanel);
        this.pack();
        
    }
    
    //-----------------------------------------------------------------
    private void setLayout()
    {
        _mainPanel.setLayout(new GridBagLayout());
        GridBagConstraints mainPanelGbc = new GridBagConstraints();
        mainPanelGbc.fill = GridBagConstraints.BOTH;
        
//                                                           col, row   numCols numRows  Wcol wrow        
  
        if (_canvas == null)
        {
            System.out.println("Canvas is null");    
        }
        addComponent(_mainPanel,  _canvas,  mainPanelGbc,    0,    0,   1,      1,       1,   1); 
 
        return;
    }
    
    //-----------------------------------------------------------------
    private void addComponent(Container container,
                              Component component,
                              GridBagConstraints gbc,
                              int column, int row,
                              int columnCells, int rowCells,
                              int weightX, int weightY)
    {

        // how much it can grow in the X and Y directions   
        gbc.weightx = weightX;
        gbc.weighty  = weightY;

        // what row and column it starts in
        gbc.gridx = column;
        gbc.gridy = row;

        //the number of columns and rows it takes up
        gbc.gridwidth = columnCells;
        gbc.gridheight = rowCells;
        
        container.add(component, gbc);
        
        return;
    } 
    
    //-----------------------------------------------------------------
    
    
    private void initMenuBar()
    {
         _menuBar = new JMenuBar();
         
         ScreenCaptureActionListener screenCaptureActionListener = 
                     new  ScreenCaptureActionListener(this, _canvas);
         
          
        JMenu menu = null;
        JMenuItem menuItem = null;
  
        //File Menu
        menu = new JMenu("File");
        menu.setMnemonic(KeyEvent.VK_F);
        menu.getAccessibleContext().setAccessibleDescription(
                         "Access File Menus");
        _menuBar.add(menu);
        
            
        menuItem = new JMenuItem("Close Window");
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_C, ActionEvent.ALT_MASK));
        menuItem.getAccessibleContext().setAccessibleDescription(
                "Close this Window.");
        menuItem.addActionListener(new CloseListener());
        menu.add(menuItem); 
        
        
        // create a button directly on the menuBar
        
        Color menuBarColor = _menuBar.getBackground();
        Border invisibleBorder = BorderFactory.createLineBorder(menuBarColor);

        JButton saveButton = new JButton("   Save Screen  ");
        saveButton.addActionListener(screenCaptureActionListener);
        saveButton.setBorder(invisibleBorder);
        saveButton.setSize(60, 30);
        String saveButtonTooltipText = "Capture the screen to a file.";
        saveButton.setToolTipText(saveButtonTooltipText);
        _menuBar.add(saveButton);
        
       
        HelpActionListener helpActionListener = new HelpActionListener();
        JButton helpButton = new JButton("Help");
        helpButton.addActionListener(helpActionListener);
        helpButton.setBorder(invisibleBorder);
        helpButton.setSize(60, 30);
        _menuBar.add(helpButton);
        
        
        
        //add to the Frame
        setJMenuBar(_menuBar);
    }
   
    //-----------------------------------------------------------------  
      
    private void addListeners()
    {
        
        //allow the frame to close when the user presses the close-box
        addWindowListener(new FrameCloseWindowListener());
        
    }
    //-----------------------------------------------------------------\
    
    private void close()
    {
        dispose();
        
        if (_exitOnClose)
        {
           // System.exit(0);
        }
    }
    
    //-----------------------------------------------------------------
    
    private class CloseListener implements ActionListener
    { 
        public void actionPerformed(ActionEvent evt)
        {
            close();
        }
    }
    
    //-----------------------------------------------------------------

    
    private class FrameCloseWindowListener extends WindowAdapter
    {
        public void windowClosing(WindowEvent evt)
        {
            close();
        }

    }      
    // ------------------------------------------------------------------------------------
    
    private class HelpActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent evt)
        {
            String message = "The yellow time series is observed.\n" +
                             "The green time series is forecast. \n" +
                             "The vertical yellow line is current time.\n" +
                             "The horizontal yellow and red lines are action and flood levels,\n" +
                             "respectively (if applicable).";
           
            DialogHelper.displayMessageDialog(TSLFrame.this, message, "TimeSeriesLite Help");
        }

    }      
    // ------------------------------------------------------------------------------------
    private void initCanvas(int x, int y,
                            int width, int height)
    
    {
        
        MeasuringUnit measuringUnit = MeasuringUnit.feet;
        
        _canvas = new TsPaintableCanvas(
                                        measuringUnit,
                                        x, y, 
                                        width, height);
          
        _canvas.setPreferredSize(_initialSize);
        _canvas.setMinimumSize(_minSize);
        
       } //end initCanvas
    
    
    // ---------------------------------------------------------------------------------
   
 
}
