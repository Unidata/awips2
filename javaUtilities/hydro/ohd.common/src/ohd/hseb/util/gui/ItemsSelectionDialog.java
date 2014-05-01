package ohd.hseb.util.gui;

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.awt.*;
import java.awt.event.*;


import javax.swing.*;


/**
 * @author RajaramV
 *  @modifiedBy CGobs
 */
public class ItemsSelectionDialog extends JDialog
{
	private String _selectedItems[];
	private String _allowedItems[];
	private String _orgSelectedItems[];
	private boolean _isSelectionCancelled = false;
	private JList _allowedItemsJList;
	private JList _selectedItemsJList;
	private Container _container;
	private JButton _addButton;
	private JButton _removeButton ;
	private JButton _applyButton ;
	private JButton _moveUpButton;
	private JButton _moveDownButton;
	private JButton _closeButton;
	private JLabel _allowedColLabel ;
	private JLabel _selectedColLabel ;
	private String _stringForPrototypeCellValue;
	private String[] _newItemsAfterAdditionOrRemoval; 
	private boolean _isAnyItemSelectedForMovedUpOrDown = false;

    
    //------------------------------------------------------------------------------------------------------------
    
	public ItemsSelectionDialog(JFrame owner,String title, String[] itemsAllowed,
			String[] itemsToBePresentInSelectedList)
	{
		super(owner, true);
		_allowedItems=itemsAllowed;
		Arrays.sort(_allowedItems, 0, _allowedItems.length);

		_selectedItems = itemsToBePresentInSelectedList;
		_orgSelectedItems = _selectedItems;
		setStringForPrototypeCellValue();
		if(title == null)
			this.setTitle("Items Selection Dialog");
		else
			this.setTitle(title);
		initGui();
	}

    //------------------------------------------------------------------------------------------------------------
      
	private void setStringForPrototypeCellValue()
	{
		String string = null; 
		if( _allowedItems.length > 0 )
		{
			string = _allowedItems[0];
			for(int i = 1; i < _allowedItems.length; i++)
			{
				if ( _allowedItems[i].length() > string.length())
					string = _allowedItems[i];
			}
		}
		_stringForPrototypeCellValue = string;
	}

    //------------------------------------------------------------------------------------------------------------
     
	private void initGui()
	{
	
	    _container = getContentPane();
	     _container.setLayout(new GridBagLayout());
         GridBagConstraints gbc = new GridBagConstraints();
        _container.setPreferredSize(new Dimension(490, 490));
  
        //create the top part of the gui
        JPanel mainPanel = initMainPanel();    
                
        //add bottom button panel
        _applyButton = new JButton("Apply");
        _closeButton = new JButton("Close");
      
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(_applyButton);
        buttonPanel.add(_closeButton);

                                                          /*           col, row, width,  height, weightx weighty fill(opt) */
        addComponent(_container,   mainPanel,    gbc,                    0,  0,    4,       10,       1,    1,    1);
        addComponent(_container,   buttonPanel,  gbc,                    0,  10,   4,       1,        0,    0);
        

        if(_isAnyItemSelectedForMovedUpOrDown)
        {
            _selectedItemsJList.clearSelection();
            _isAnyItemSelectedForMovedUpOrDown = false;
        }

    //    mainPanel.addComponentListener(new WindowResizeListener(mainPanel));
        
        addListeners();
        
		_newItemsAfterAdditionOrRemoval = new String[_selectedItems.length];
		System.arraycopy(_selectedItems, 0, _newItemsAfterAdditionOrRemoval, 0, _selectedItems.length);
		this.pack();
		this.setVisible(true);
	}
    
    //------------------------------------------------------------------------------------------------------------
    private JButton initArrowButton(String iconType)
    {
        Dimension dimension = new Dimension(15, 20);
        
        Color buttonColor = this.getBackground();
        Color imageColor = Color.BLUE;
    
        ImageIcon arrowIcon = StandardImageIcons.getImageIcon(iconType, 
                                      imageColor, buttonColor, dimension);
        
        
        JButton button = new JButton(arrowIcon);
        
        button.setPreferredSize(dimension);
  
        return button;
        
    }
      
    //------------------------------------------------------------------------------------------------------------
      
    private JPanel initMainPanel()
    {
        final int visibleRowCount = 30;

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new GridBagLayout());           
        //mainPanel.setBackground(Color.RED);
        
        GridBagConstraints gbc = new GridBagConstraints();

        _addButton = new JButton("Add >>");
        _removeButton = new JButton("Remove <<");
        
        _moveUpButton = initArrowButton(StandardImageIcons.UP_ARROW);
        _moveDownButton = initArrowButton(StandardImageIcons.DOWN_ARROW);
        
        //set tooltips
        _moveUpButton.setToolTipText("Move selected items up in the list.");
        _moveDownButton.setToolTipText("Move selected items down in the list.");
        

      //  _moveUpButton = new JButton("^");
      //  _moveDownButton = new JButton("v");
         
        _allowedColLabel = new JLabel("Allowed Items");
        _selectedColLabel = new JLabel("Selected Items");

        _allowedItemsJList = new JList(_allowedItems);
        _allowedItemsJList.setVisibleRowCount(visibleRowCount);
        _allowedItemsJList.setSelectionMode(DefaultListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        if(_stringForPrototypeCellValue.length() < ("Allowed Items").length())
        {
            _allowedItemsJList.setPrototypeCellValue(("Allowed Items"));
        }
        _allowedItemsJList.setAutoscrolls(true);

        JPanel allowedListPanel = new JPanel();
        JScrollPane allowedScrollPane = new JScrollPane(_allowedItemsJList);
        allowedScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        allowedScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        allowedListPanel.setLayout(new GridLayout(1,1));
        allowedListPanel.add(allowedScrollPane);
   
        
        if(_selectedItems !=null)
            _selectedItemsJList = new JList(_selectedItems);
        else
            _selectedItemsJList = new JList();

        _selectedItemsJList.setVisibleRowCount(visibleRowCount);
        _selectedItemsJList.setSelectionMode(DefaultListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        if(_stringForPrototypeCellValue.length() < ("Selected Items").length())
        {
            _selectedItemsJList.setPrototypeCellValue(("Selected Items"));
        }
        _selectedItemsJList.setAutoscrolls(true);   
        
        JPanel selectedListPanel = new JPanel();
        JScrollPane selectedScrollPane = new JScrollPane(_selectedItemsJList);
        selectedScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        selectedScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        selectedListPanel.setLayout(new GridLayout(1,1));
        selectedListPanel.add(selectedScrollPane);

        
        JPanel addAndRemoveButtonPanel = new JPanel();
   //     addandRemoveButtonsPanel.setLayout(new GridLayout(2,1,15,15));
        
       // addAndRemoveButtonPanel.setBackground(Color.PINK);
        addAndRemoveButtonPanel.setLayout(new GridLayout(2, 1, 0, 15));
        addAndRemoveButtonPanel.add(_addButton);
        addAndRemoveButtonPanel.add(_removeButton);

        JPanel moveButtonsPanel = new JPanel();
        moveButtonsPanel.setLayout(new GridLayout(2, 1, 0, 15));
        moveButtonsPanel.add(_moveUpButton);
        moveButtonsPanel.add(_moveDownButton);
               
        JPanel verticalFillerPanel = new JPanel();
        verticalFillerPanel.setPreferredSize(new Dimension(50, 50));
       // verticalFillerPanel.setBackground(Color.BLUE);
    
        JPanel verticalFillerPanel2 = new JPanel();
        verticalFillerPanel2.setPreferredSize(new Dimension(50, 50));
       // verticalFillerPanel2.setBackground(Color.GREEN);
    
        gbc.insets = new Insets(5,5,5,5);
                                                            /*           col, row, width,  height, weightx weighty     fill(opt) */
          
        addComponent(mainPanel,  _allowedColLabel,  gbc,                  1,  0,    1,      1,        0,      0);
        addComponent(mainPanel, allowedListPanel,    gbc,                 1,  1,    1,      5,        1,      1,          1);
        addComponent(mainPanel, addAndRemoveButtonPanel, gbc,
                                                                          2,  3,    1,      1,        0,      0);
       
        addComponent(mainPanel, verticalFillerPanel, gbc,                 2,  0,    1,      2,        0,      1,          1);
        addComponent(mainPanel, verticalFillerPanel2, gbc,                2,  3,    1,      2,        0,      1,          1);
        
        
        addComponent(mainPanel, _selectedColLabel, gbc,                   3,  0,    1,      1,        0,      0);
        addComponent(mainPanel, selectedListPanel,  gbc,                  3,  1,    1,      5,        1,      1,          1);
    
        addComponent(mainPanel, moveButtonsPanel,   gbc,                  4,  3,    1,      1,        0,      0);
          
        
        return mainPanel;
        
    }
    
    //------------------------------------------------------------------------------------------------------------
    
    private void addComponent(Container container,
            Component component,
            GridBagConstraints gbc,
            int column, int row,
            int columnCells, int rowCells,
            int weightX, int weightY)
    {
        addComponent(container, component, gbc, column, row, columnCells, rowCells, weightX, weightY, 0);
    }
    
    //------------------------------------------------------------------------------------------------------------
    
	private void addComponent(Container container,
	        Component component,
	        GridBagConstraints gbc,
	        int column, int row,
	        int columnCells, int rowCells,
	        int weightX, int weightY, int fill)
	{

//	    how much it can grow in the X and Y directions   
	    gbc.weightx = weightX;
	    gbc.weighty  = weightY;

//	    what row and column it starts in
	    gbc.gridx = column;
	    gbc.gridy = row;

//	    the number of columns and rows it takes up
	    gbc.gridwidth = columnCells;
	    gbc.gridheight = rowCells;
	    
        gbc.fill = fill;
        
	    container.add(component, gbc);

	    return;
	} 
    
    //------------------------------------------------------------------------------------------------------------
      
    
    private void addListeners()
    {
        _addButton.addActionListener( new AddButtonListener());
        _removeButton.addActionListener(new RemoveButtonListener());
        _closeButton.addActionListener(new CloseButtonListener());
        _moveUpButton.addActionListener(new MoveUpButtonListener());
        _moveDownButton.addActionListener(new MoveDownButtonListener());
        
        _applyButton.addActionListener(new ConfirmSelectionButtonListener());
        
        //allows user to remove items with the delete or backspace keys
        _selectedItemsJList.addKeyListener(new SelectedListKeyListener());
        _selectedItemsJList.addMouseListener(new DoubleClickOnSelectedListener());
        
        _allowedItemsJList.addKeyListener(new AllowedListKeyListener());
        _allowedItemsJList.addMouseListener(new DoubleClickOnAllowedListener());
        return;
    }
   
    //  ------------------------------------------------------------------------------------------------------------
    public String[] getSelectedItems()
	{
		String [] returnedSelectedItems = null ;
		if(!_isSelectionCancelled)
		{
			if(_selectedItems != null)
			{
				returnedSelectedItems = new String[_selectedItems.length];

				System.arraycopy(_selectedItems, 0,
						returnedSelectedItems, 0,
						_selectedItems.length );
			}
		}
		else
        {
			returnedSelectedItems = _orgSelectedItems;
        }
		return returnedSelectedItems;		
	}
    
    //  ------------------------------------------------------------------------------------------------------------
    private class DoubleClickOnAllowedListener extends MouseAdapter
    {
        public void mouseClicked(MouseEvent e)
        {
            if( e.getClickCount() == 2)
            {
                addSelectedItems();
            }
            
        }

    }
    
    //  ------------------------------------------------------------------------------------------------------------
    
    private class DoubleClickOnSelectedListener extends MouseAdapter
    {
        public void mouseClicked(MouseEvent e)
        {
            if( e.getClickCount() == 2)
            {
                removeSelectedItems();
            }
            
        }

    }

	private class AddButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{		
			addSelectedItems();	
		}
	}
    //  ------------------------------------------------------------------------------------------------------------

	private class CloseButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			_isSelectionCancelled = true;
			closeItemsSelectionDialog();   	
		}
	}
    //  ------------------------------------------------------------------------------------------------------------
    private void addSelectedItems()
    {
        int[] indicesOfNewItemsSelectedFromAllowedList =_allowedItemsJList.getSelectedIndices();
        int numOfNewItemsSelectedFromAllowedList = indicesOfNewItemsSelectedFromAllowedList.length;
        int numOfExistingItemsInNewList = _selectedItemsJList.getModel().getSize();

        // All the items in allowed list are already present in new List
        if(numOfExistingItemsInNewList == _allowedItemsJList.getModel().getSize())
        {

        }
        //New List is empty, so add all the item selected from allowed list
        else if (numOfExistingItemsInNewList == 0)
        {
            _selectedItemsJList.setListData(_allowedItemsJList.getSelectedValues());
        }
        else  //Compare existing new list and items newly selected from allowed list for any match then add
        {
            int cntOfSameItems=0;
            for(int i =0; i< numOfNewItemsSelectedFromAllowedList; i++)
            {
                int index = indicesOfNewItemsSelectedFromAllowedList[i];
                String itemFromAllowed = _allowedItemsJList.getModel().getElementAt(index).toString();
                for(int j = 0; j < numOfExistingItemsInNewList; j++)
                {
                    String itemFromExistingNewList = _selectedItemsJList.getModel().getElementAt(j).toString();
                    if(itemFromExistingNewList.compareTo(itemFromAllowed) == 0)
                    {
                        cntOfSameItems++;
                    }
                }
            }


            int numOfFinalItemsAfterAddition = (numOfNewItemsSelectedFromAllowedList +
                    numOfExistingItemsInNewList) -  cntOfSameItems;

            _newItemsAfterAdditionOrRemoval = new String[numOfFinalItemsAfterAddition];

            int cnt=0;
            for(cnt=0; cnt<numOfExistingItemsInNewList; cnt++)
            {
                _newItemsAfterAdditionOrRemoval[cnt]=_selectedItemsJList.getModel().getElementAt(cnt).toString();
            }


            for(int i =0; i< numOfNewItemsSelectedFromAllowedList; i++)
            {
                int index = indicesOfNewItemsSelectedFromAllowedList[i];
                String itemFromAllowed = _allowedItemsJList.getModel().getElementAt(index).toString();
                boolean found = false;
                for(int j = 0; j < numOfExistingItemsInNewList; j++)
                {
                    String itemFromExistingNewList = _selectedItemsJList.getModel().getElementAt(j).toString();
                    if(itemFromExistingNewList.compareTo(itemFromAllowed) == 0)
                    {
                        found=true;
                        break;
                    }
                }
                if (found == false)
                {
                    if(cnt < numOfFinalItemsAfterAddition )
                    {
                        _newItemsAfterAdditionOrRemoval[cnt++] = itemFromAllowed;
                    }
                }

            }

            _selectedItemsJList.setListData(_newItemsAfterAdditionOrRemoval);

            _allowedItemsJList.clearSelection();
        }
    }
    
    // ------------------------------------------------------------------------
	private void removeSelectedItems()
    {
        int[] removeItemsListIndices =_selectedItemsJList.getSelectedIndices();

        int sizeOfRemoveItemsListIndices = removeItemsListIndices.length;

        int sizeOfExistingNewItemsList = _selectedItemsJList.getModel().getSize();

        _newItemsAfterAdditionOrRemoval = new String[sizeOfExistingNewItemsList - sizeOfRemoveItemsListIndices];

        if(sizeOfExistingNewItemsList == removeItemsListIndices.length)
        {
            _selectedItemsJList.setListData(_newItemsAfterAdditionOrRemoval);
        }
        else
        {
            int cnt=0;
            for(int i=0; i< sizeOfExistingNewItemsList; i++)
            {
                boolean found = false;
                for(int j=0;j<removeItemsListIndices.length;j++)
                {
                    if(i==removeItemsListIndices[j])
                    {
                        found=true;
                        break;
                    }
                }
                if(found == false)
                {
                    _newItemsAfterAdditionOrRemoval[cnt++]=_selectedItemsJList.getModel().getElementAt(i).toString();
                }
            }

            _selectedItemsJList.setListData(_newItemsAfterAdditionOrRemoval);
        }
    }
    
    //  ------------------------------------------------------------------------------------------------------------

	private class RemoveButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
            removeSelectedItems();
		}	
	}
    //  ------------------------------------------------------------------------------------------------------------

	private class MoveUpButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			_isAnyItemSelectedForMovedUpOrDown = true;
			int[] moveUpItemsIndices =_selectedItemsJList.getSelectedIndices();
			Object[] itemsToHighLight = _selectedItemsJList.getSelectedValues();
			List contiguousBlockList = createListOfContiguousBlockLists(moveUpItemsIndices);
			createFinalMovedList(contiguousBlockList, true);
			int[] indices = new int[itemsToHighLight.length];
			for(int i=0; i < itemsToHighLight.length; i++)
			{
				if(moveUpItemsIndices[i] != 0)
					indices[i] = moveUpItemsIndices[i] -1;
				else
					indices[i] = moveUpItemsIndices[i];
			}
			_selectedItemsJList.setSelectedIndices(indices);
		}
	}
    //  ------------------------------------------------------------------------------------------------------------

	private class MoveDownButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			_isAnyItemSelectedForMovedUpOrDown = true;
			int[] moveDownItemsIndices =_selectedItemsJList.getSelectedIndices();
			Object[] itemsToHighLight = _selectedItemsJList.getSelectedValues();

			List contiguousBlockList = createListOfContiguousBlockLists(moveDownItemsIndices);
			createFinalMovedList(contiguousBlockList, false);
			int[] indices = new int[itemsToHighLight.length];
			for(int i=0; i < itemsToHighLight.length; i++)
			{
				if(moveDownItemsIndices[i] != _selectedItems.length)
					indices[i] = moveDownItemsIndices[i] +1;
				else
					indices[i] = moveDownItemsIndices[i];
			}
			_selectedItemsJList.setSelectedIndices(indices);
		}
	}
    //  ------------------------------------------------------------------------------------------------------------

	private void setFinalSelectedItemsArray()
	{
		int sizeOfSelectedItemsArray = _selectedItemsJList.getModel().getSize();
		if(sizeOfSelectedItemsArray != 0)
		{
			_selectedItems = new String[sizeOfSelectedItemsArray];
			for(int i=0; i< _selectedItems.length; i++)
			{
				_selectedItems[i] = _selectedItemsJList.getModel().getElementAt(i).toString();
			}
		}
		else
		{
			_selectedItems = null;
		}
	}
    //  ------------------------------------------------------------------------------------------------------------

    private List createListOfContiguousBlockLists(int[] movedItemsIndices)
	{
		setFinalSelectedItemsArray();

		List contiguousBlockList =  new ArrayList();
		List indicesBlockList = new ArrayList();
		if(movedItemsIndices.length == 1)
		{
			indicesBlockList.add(new Integer(movedItemsIndices[0]));
			contiguousBlockList.add(indicesBlockList);
		}
		else
		{
			for(int i=0; i < movedItemsIndices.length; i++)
			{
				if(indicesBlockList.size() == 0)
				{
					indicesBlockList.add(new Integer(movedItemsIndices[i]));
					continue;
				}
				else
				{
					if(movedItemsIndices[i] == (movedItemsIndices[i-1]+1))
					{
						indicesBlockList.add(new Integer(movedItemsIndices[i]));
					}
					else
					{
						contiguousBlockList.add(indicesBlockList);
						indicesBlockList = new ArrayList();
						indicesBlockList.add(new Integer(movedItemsIndices[i]));
					}
					if(i== movedItemsIndices.length-1)
						contiguousBlockList.add(indicesBlockList);
					else
						continue;
				}
			}
		}

		return contiguousBlockList;
	}
    //  ------------------------------------------------------------------------------------------------------------

	private void createFinalMovedList(List contiguousBlockList, boolean isMoveUp)
	{
		String finalSelectedItems[] = new String[_newItemsAfterAdditionOrRemoval.length];	
		System.arraycopy(_newItemsAfterAdditionOrRemoval, 0, finalSelectedItems, 0, _newItemsAfterAdditionOrRemoval.length);
		for(int i=0; i < contiguousBlockList.size(); i++)
		{
			List blockList = (List) contiguousBlockList.get(i);
			if(isMoveUp)
			{
				int indexOfFirstItemInBlockList = new Integer(blockList.get(0).toString()).intValue();
				if(indexOfFirstItemInBlockList == 0)
					continue;
				for(int j=0; j < blockList.size(); j++)
				{
					int indexInBlock = new Integer(blockList.get(j).toString()).intValue();
					String temp = finalSelectedItems[indexInBlock-1] ;
					finalSelectedItems[indexInBlock-1] = finalSelectedItems[indexInBlock];
					finalSelectedItems[indexInBlock] = temp; 
				}
			}
			else //MoveDown
			{
				int indexOfLastItemInBlockList = new Integer(blockList.get(blockList.size()-1).toString()).intValue();
				if(indexOfLastItemInBlockList == finalSelectedItems.length-1)
					continue;
				for(int j=blockList.size()-1; j >=0; j--)
				{
					int indexInBlock = new Integer(blockList.get(j).toString()).intValue();
					String temp = finalSelectedItems[indexInBlock+1] ;
					finalSelectedItems[indexInBlock+1] = finalSelectedItems[indexInBlock];
					finalSelectedItems[indexInBlock] = temp; 
				}
			}
		}
		System.arraycopy(finalSelectedItems, 0, _newItemsAfterAdditionOrRemoval, 0, _newItemsAfterAdditionOrRemoval.length);
		System.arraycopy(finalSelectedItems, 0, _selectedItems, 0, _selectedItems.length);

		_selectedItemsJList.setListData(_selectedItems);

	}
    //  ------------------------------------------------------------------------------------------------------------

	private void closeItemsSelectionDialog()
	{
		this.dispose();
	}
    
    //  ------------------------------------------------------------------------------------------------------------

	private class ConfirmSelectionButtonListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			setFinalSelectedItemsArray();
			closeItemsSelectionDialog();
		}
	}
    //  ------------------------------------------------------------------------------------------------------------
   
    private class SelectedListKeyListener implements KeyListener
    {
        public void keyPressed(KeyEvent e)
        {
            int keyCode = e.getKeyCode();
      
            if ( ( keyCode == KeyEvent.VK_DELETE) || (keyCode == KeyEvent.VK_BACK_SPACE) )
            {
                removeSelectedItems();
            }
     
        }
        
        public void keyReleased(KeyEvent e)
        {
        
        }

        public void keyTyped(KeyEvent e)
        {
           
            
        }
    }
    
    
    //  ------------------------------------------------------------------------------------------------------------
    private class AllowedListKeyListener implements KeyListener
    {
        public void keyPressed(KeyEvent e)
        {
            int keyCode = e.getKeyCode();
   
            if (keyCode == KeyEvent.VK_ENTER) 
            {
                  addSelectedItems();
            }
        }

        public void keyReleased(KeyEvent e)
        {
          
        }

        public void keyTyped(KeyEvent e)
        {
           
        }
    }
    
    
    //  ------------------------------------------------------------------------------------------------------------
     
    private static String[] initBigItemsArray(int count)
    {
        String [] itemStringArray = new String[count];
        
        for (int i = 0; i < count; i++)
        {
            itemStringArray[i] = "Item" + i;
        }
        
        return itemStringArray;
    }
    //  ------------------------------------------------------------------------------------------------------------
  
	public static void main(String[] args)
	{
	    JFrame frame = new JFrame();

	    frame.setSize(new Dimension(250, 250));
	    frame.setVisible(true);
        
        FrameCloseWindowListener.addFrameCloseWindowListener(frame);
    
	    String titleString = "Test ColumnSelection";

	    String[] initialSelectedList = {"Item2", "Item4"};
        
        String[] bigItemsAllowed = initBigItemsArray(100);
        
        new  ItemsSelectionDialog(frame, titleString, bigItemsAllowed, initialSelectedList);
        
        frame.dispose();

    }
}

// ------------------------------------------------------------------------------------

