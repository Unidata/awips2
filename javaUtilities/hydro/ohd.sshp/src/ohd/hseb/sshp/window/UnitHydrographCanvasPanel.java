/*
 * Created on Aug 23, 2004
 *
 * 
 */
package ohd.hseb.sshp.window;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import ohd.hseb.model.UnitHydrographDescriptor;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.TableColumnComparator;
import ohd.hseb.util.gui.drawing.DataPointCanvas;

/**
 * @author GobsC
 * This class owns and controls the UnitHydrographCanvasAdapter and the
 * UnitHydrographDescriptorTableAdapter, and so, by delegation,
 * owns their Canvas and Table.  It also is the messenger between the
 * rest of the UnitHydrographEditor and its components.
 * 
 */
public class UnitHydrographCanvasPanel extends JPanel
{
	private List _descriptorList = null;
	
	//This is needed by the UnitHydrographDataPointCanvasAdapter
	//and the UnitHydrographDescriptorTableAdapter, so 
	//this class will manage it.
	private List _displayedDescriptorList = null;

	private UnitHydrographDescriptorTableAdapter _tableAdapter = null;
	//private JPanel _canvasButtonPanel =			    new JPanel();
	
	

	private UnitHydrographDataPointCanvasAdapter _canvasAdapter = null;
	private DataPointCanvas _canvas = null;
	
	public static final String AREA_ID = 			"Area ID";
	public static final String MODEL = 				"Model";
	public static final String DURATION = 			"Duration";


    public UnitHydrographCanvasPanel(List unitHydrographEntryList)
    {
        Dimension minCanvasSize = new Dimension( 700, 750 );
        Dimension minScrollPaneSize = new Dimension( 700, 250 );
        Dimension minCanvasPanelSize = new Dimension( 700, 1000 );
    	
        
        Dimension prefCanvasSize = new Dimension( 700, 750 );		    
        Dimension prefScrollPaneSize = new Dimension( 700, 250 );
        Dimension prefCanvasPanelSize = new Dimension( 700, 1000 );

    	
         
        //  
        this.setLayout( new GridBagLayout() );					
        this.setPreferredSize( prefCanvasPanelSize );
        this.setMinimumSize(minCanvasPanelSize);
       // this.setBackground(Color.blue);

        _descriptorList = 
            UnitHydrographDescriptor.createUnitHydrographDescriptorList(
                    				 unitHydrographEntryList);
        
        _displayedDescriptorList = new ArrayList();
        
        //objects for the drawing
        _canvasAdapter = new UnitHydrographDataPointCanvasAdapter();
        _canvas = _canvasAdapter.initCanvas(_descriptorList);
        _canvas.setPreferredSize(prefCanvasSize );
        _canvas.setMinimumSize(minCanvasSize );
       
        _tableAdapter = createDescriptorTableAdapter(_descriptorList);
    	JScrollPane tableScrollPane = _tableAdapter.getScrollPane();
    	tableScrollPane.setPreferredSize(prefScrollPaneSize);
    	tableScrollPane.setMinimumSize(minScrollPaneSize);
    	
    	//select the first descriptor in the list
    	
    	if (_tableAdapter.getDescriptorCount() > -1)
    	{
    	    _tableAdapter.setSelectedIndex(0);
    	    UnitHydrographDescriptor descriptor = _tableAdapter.getDescriptorByIndex(0);
    	    _displayedDescriptorList.add(descriptor);
    	}
    
    	  //button panel for the canvas
     //   _canvasButtonPanel.setLayout(new GridBagLayout()  );
       // _canvasButtonPanel.setLayout(new FlowLayout()  );
    //    _canvasButtonPanel.setPreferredSize(prefCanvasButtonPanelSize  );
    //    _canvasButtonPanel.setMinimumSize(minCanvasButtonPanelSize );
        
        //separators for appearance
    	JPanel horizontalSeparatorPanel = new JPanel();
    	horizontalSeparatorPanel.setPreferredSize( new Dimension( 50, 20 ) );
    	
    	JPanel verticalSeparatorPanel = new JPanel();
    	verticalSeparatorPanel.setPreferredSize( new Dimension( 50, 20 ) );
    	
    	GridBagConstraints gbc = new GridBagConstraints();
    	//gbc.insets = new Insets(20, 0, 20, 0);

    	
        gbc.fill = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;
        
    	//add components to the canvasPanel		
        ComponentHelper.addPanelComponent( this, _canvas,          gbc,           0,  0,   1,     10,  GridBagConstraints.BOTH );
    	
        
        gbc.anchor = GridBagConstraints.NORTH;
        ComponentHelper.addPanelComponent( this, tableScrollPane , gbc,           0, 11,   1,     8,   GridBagConstraints.BOTH );
    	  
        
        addListeners();
      
    }
    
    public void addListeners()
    {
        _tableAdapter.getListSelectionModel().addListSelectionListener( new DescriptorRowSelectionListener() );       
    }
	

	
	private UnitHydrographDescriptorTableAdapter createDescriptorTableAdapter(List descriptorList)
	{
	    JTable table = null;
		String[] columnNameArray = 	{ AREA_ID, MODEL, DURATION };
		UnitHydrographDescriptorTableAdapter tableAdapter = null;
		
		TableColumnComparator comparator = new UnitHydrographDescriptorComparator();
	    
		
		tableAdapter = new UnitHydrographDescriptorTableAdapter(comparator, columnNameArray, descriptorList );
	      
	    return tableAdapter;
	
	}
    // -------------------------------------------------------------------------------------

	public void refreshData(List unitHydrographEntryList)
    {
        //CodeTracer tracer = new CodeTracer();
        //tracer.trace();

        _descriptorList = UnitHydrographDescriptor.createUnitHydrographDescriptorList(unitHydrographEntryList);

        //save off this list, since the refreshTableModel command will call the
        //listener, which will overwrite the contents of _displayDiscriptorList
        List tempDisplayDescriptorList = new ArrayList(_displayedDescriptorList);
        
        
        _tableAdapter.refreshTableModel(_descriptorList);
        
        //pull out the origin displayDescriptorList
        
//      creates own descriptorList, for coding reasons, from the unitHydrographEntryList
        _canvasAdapter.refreshCanvas(unitHydrographEntryList); 
        
        _displayedDescriptorList = tempDisplayDescriptorList;
        
        _tableAdapter.selectItems(_displayedDescriptorList);
    }

    // -------------------------------------------------------------------------------------
	
	  // -------------------------------------------------------------------------------------
    private class DescriptorRowSelectionListener implements
            ListSelectionListener
    {

        public void valueChanged(ListSelectionEvent event)
        {
             
            String header = "DescriptorRowSelectionListener.valueChanged(): ";
            UnitHydrographDescriptor descriptor = null;
            JTable table = _tableAdapter.getTable();

            // for each from in the table, reset the shouldPaints in the
            // canvasAdapter
            
            // clear out the displayed Descriptor list
            _displayedDescriptorList.clear();
            
            for (int i = 0; i < table.getRowCount(); i++)
            {
                boolean isSelected = table.isRowSelected(i);

                descriptor = _tableAdapter.getDescriptorByIndex(i);
          
                if ( descriptor != null )
                {
                    //maintain the list of selected items
                    if (isSelected)
                    {
                         _displayedDescriptorList.add(descriptor);
                    }
                    
                    _canvasAdapter.setPainterShouldPaint(descriptor, isSelected);
    
                   // System.out.println(header + "descriptor =  " + descriptor
                   //         + " shouldPaint = " + isSelected);
                }
            }

            _canvasAdapter.redrawCanvas();
            

        }
    }
    // -------------------------------------------------------------------------------------
}
