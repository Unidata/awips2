/*
 * Created on Nov 12, 2004
 *
 * 
 */
package ohd.hseb.sshp.window;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;

import javax.swing.JPanel;

import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import ohd.hseb.model.sacsma.SacSmaParameters;
import ohd.hseb.model.sacsma.SacSmaState;
import ohd.hseb.util.gui.LabeledSlider;
import ohd.hseb.util.gui.WindowResizingManager;


/**
 * @author Chip Gobs
 *
 * 
 */
public class LiveSacStateEditor extends JDialog
{
    
    private SacSmaState _state = null;
    private SacSmaParameters _params = null;


    // window sizing data
    private Dimension _maxDimension = new Dimension(700, 300);
    private Dimension _initialDimension = new Dimension(700, 300);
    private Dimension _minDimension = new Dimension(300, 300);
 
    
    private JPanel _mainPanel = null;
    private Border _panelBorder = BorderFactory.createLineBorder(Color.black);  
	private JButton _closeButton = null;
		
	private LabeledSlider _uztwcLabeledSlider = null;
	private LabeledSlider _uzfwcLabeledSlider = null;
	
	private LabeledSlider _lztwcLabeledSlider = null;
	private LabeledSlider _lzfscLabeledSlider = null;
	private LabeledSlider _lzfpcLabeledSlider = null;
	
	private LabeledSlider _adimcLabeledSlider = null; // 0.0 <= ADIMC <= UZTWM + LZTWM
	
	
	private ActionListener _updateActionListener = null;
    
    // -----------------------------------------------------------------------------------
    public LiveSacStateEditor(Frame owner, 
                              SacSmaState state,
                              SacSmaParameters params,
                              boolean isModal, 
                              ActionListener listener)
    {
        super( owner, isModal );
        
        _state = state;
        _params = params;
        
        _updateActionListener = listener;
        
        initGui();
        
    }
   
    // -----------------------------------------------------------------------------------
    public SacSmaState getCurrentSacSmaState()
    {
        return _state;
    }
    
    // -----------------------------------------------------------------------------------
    
    private void initGui()
    {
        
        setTitle("LiveSacStateEditor Dialog");
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = new Insets(2, 2, 2, 2); 
        gbc.weightx = 1;
        gbc.weighty = 1;
        
        JPanel verticalSeparatorPanel = new JPanel();
        verticalSeparatorPanel.setPreferredSize(new Dimension(30, 30));
        
        JPanel horizontalSeparatorPanel = new JPanel();
        horizontalSeparatorPanel.setPreferredSize(new Dimension(30, 30));
      
      
        JPanel horizontalSeparatorPanel2 = new JPanel();
        horizontalSeparatorPanel2.setPreferredSize(new Dimension(30, 30));
      
        JPanel horizontalSeparatorPanel3 = new JPanel();
        horizontalSeparatorPanel3.setPreferredSize(new Dimension(30, 30));
      
        
        _mainPanel = new JPanel();
        JPanel panel = _mainPanel;
        
        _closeButton = new JButton("Close");
        _closeButton.setPreferredSize(new Dimension(50, 30));
        
        _mainPanel.setLayout( new GridBagLayout() );
        
        initSliders();
        
        
        //arrange the layout
        
        											     //    	X,     Y,  #Col,  #Row
          
        addComponent(panel, _uztwcLabeledSlider, 	 gbc,       0,     0,  3,      1,       1,   0);
        addComponent(panel, _uzfwcLabeledSlider, 	 gbc,       0,     1,  3,      1,       1,   0);
        addComponent(panel, _lztwcLabeledSlider, 	 gbc,       0,     2,  3,      1,       1,   0);
        addComponent(panel, _lzfscLabeledSlider, 	 gbc,       0,     3,  3,      1,       1,   0);
        addComponent(panel, _lzfpcLabeledSlider, 	 gbc,       0,     4,  3,      1,       1,   0); 
        addComponent(panel, _adimcLabeledSlider, 	 gbc,       0,     5,  3,      1,       1,   0);
        
        addComponent(panel, horizontalSeparatorPanel,gbc,       1,     6,  1,      1,       1,   0);
         
        
        addComponent(panel, horizontalSeparatorPanel2,  gbc,    0,     7,  1,      1,       1,   0);
        
        addComponent(panel, _closeButton,            gbc,       1,     7,  1,      1,       0,   0);
        addComponent(panel, horizontalSeparatorPanel3, gbc,     2,     7,  1,      1,       1,   0);
        
        
        
        //set up initial bounds limitations
        //see WindowResize listener for the minimum setting

        Rectangle maxBoundsRectangle = new Rectangle(_maxDimension);
       // this.setMaximizedBounds(maxBoundsRectangle);

         //hook up the mainPanel to the contentPane
      
        this.getContentPane().add(_mainPanel);
        this.pack();

        Rectangle initialBoundsRectangle = new Rectangle(_initialDimension);
        this.setBounds(initialBoundsRectangle);

        
       
        addListeners();
    }
    // -----------------------------------------------------------------------------------
    private void setSacSlidersFromModel()
    {
        
        SacSmaState state = _state;
        
        _uztwcLabeledSlider.setModelValue(state.getUztwc());
        _uzfwcLabeledSlider.setModelValue(state.getUzfwc());
        
        _lztwcLabeledSlider.setModelValue( state.getLztwc());
        _lzfscLabeledSlider.setModelValue( state.getLzfsc());
        _lzfpcLabeledSlider.setModelValue( state.getLzfpc());
        
        _adimcLabeledSlider.setModelValue( state.getAdimc());
        
        
    }  
    
    // -----------------------------------------------------------------------------------
  
    private void initSliders()
    {
        
        int width = 150;
        int height = 40;
    
       //UZTWC adjustment controls
        // note: the slide values go from 0 to 100 and are mapped to
        // a double
        
        SacSmaState state = getCurrentSacSmaState();
        SacSmaParameters params = _params;
  
        
        _uztwcLabeledSlider  = new LabeledSlider("UZTWC:", 
                state.getUztwc(),
                0.0,
                params.getUztwm(),
                width,
                height );
        
        
        _uzfwcLabeledSlider  = new LabeledSlider("UZFWC:", 
                state.getUzfwc(),
                0.0,
                params.getUzfwm(),
                width,
                height );
        
        
        _lztwcLabeledSlider  = new LabeledSlider("LZTWC:", 
                state.getLztwc(),
                0.0,
                params.getLztwm(),
                width,
                height );
        
        _lzfscLabeledSlider  = new LabeledSlider("LZFSC:", 
                state.getLzfsc(),
                0.0,
                params.getLzfsm(),
                width,
                height );
        
        _lzfpcLabeledSlider  = new LabeledSlider("LZFPC:", 
                state.getLzfpc(),
                0.0,
                params.getLzfpm(),
                width,
                height );
        
        
        _adimcLabeledSlider  = new LabeledSlider("ADIMC:", 
                state.getAdimc(),
                0.0,
                params.getUztwm() + params.getLztwm(),
                width,
                height );
    
       
    }
    
    // -----------------------------------------------------------------------------------
      
    private void addListeners()
    {
        WindowCloseListener windowCloser = new WindowCloseListener();
		_closeButton.addActionListener( windowCloser );
		addWindowListener( windowCloser );
		
		// this adds its own listener internally
		WindowResizingManager resizingMgr = 
		    new WindowResizingManager(LiveSacStateEditor.this, 
		            				  _minDimension,
		            				  _maxDimension);
		
		
		//slider listeners
		
	    _uztwcLabeledSlider.addChangeListener(new ValueChangeListener("UZTWC"));
	    _uzfwcLabeledSlider.addChangeListener(new ValueChangeListener("UZFWC"));
	    
	    _lztwcLabeledSlider.addChangeListener(new ValueChangeListener("LZTWC"));  
	    _lzfscLabeledSlider.addChangeListener(new ValueChangeListener("LZFSC"));
	    _lzfpcLabeledSlider.addChangeListener(new ValueChangeListener("LZFPC"));
	    
	    _adimcLabeledSlider.addChangeListener(new ValueChangeListener("ADIMC"));


	    return;
		   
    }
    
    // -----------------------------------------------------------------------------------
    
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
    // ------------------------------------------------------------
    
    private void closeWindow()
	{
		this.dispose();
	}
    // ------------------------------------------------------------
       
    private void notifyListeners(String eventMessage)
    {
        ActionEvent event = new ActionEvent(LiveSacStateEditor.this, 0, eventMessage);
        _updateActionListener.actionPerformed(event);
    }
    // ------------------------------------------------------------
     
    
    // Inner classes----------------------------------------------------------------------
    // 
    private class WindowCloseListener extends WindowAdapter implements ActionListener
    { 
        public void actionPerformed(ActionEvent evt)
        {
           closeWindow();
        }
        
        public void windowClosing(WindowEvent evt)
        {
            closeWindow();
        }
        
    }
    
    // -----------------------------------------------------------------------------------
    private class ValueChangeListener implements ChangeListener
    {
        private String _valueName = null;
        
        public ValueChangeListener(String valueName)
        {
            _valueName = valueName;
        }
        
        public void stateChanged(ChangeEvent e)
        {
                    
             
            _state.setUztwc(_uztwcLabeledSlider.getModelValue());
            _state.setUzfwc(_uzfwcLabeledSlider.getModelValue());
            
            _state.setLztwc(_lztwcLabeledSlider.getModelValue());
            _state.setLzfsc(_lzfscLabeledSlider.getModelValue());
            _state.setLzfpc(_lzfpcLabeledSlider.getModelValue());
            
            _state.setAdimc(_adimcLabeledSlider.getModelValue());
            
            
            setSacSlidersFromModel();
    
            notifyListeners(_valueName + " changed");
        }
    }
    
  
 
    // -----------------------------------------------------------------------------------
    
  
    
    
}
