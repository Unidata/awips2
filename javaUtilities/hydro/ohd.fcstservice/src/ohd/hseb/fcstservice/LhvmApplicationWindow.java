package ohd.hseb.fcstservice;   

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import ohd.hseb.ihfsdb.generated.ServiceTableViewRecord;
import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.util.gui.*;

/**
 * @author varmaR
 *
 */

public class LhvmApplicationWindow implements Observer
{	
	private DataServicesGlobals dataServicesGlobals = null;
	private java.util.List lhvmRowDataList = null;
	private LhvmDataManager lhvmDataManager = null;
	private LhvmLogger lhvmLogger = null;
	private JTableManager jtm = null;
	
	private ServiceTableViewViewJTableRowData selectedServiceTableViewViewJTableRowData = null;
	private ArrayList selectedServiceTableViewViewJTableRowsDataList = null;
	
	private JTextField servicesTableTextField = null;
	private JTextField deterministicTableTextField = null;
	private JTextField watsupTableTextField = null;
	private JTextField ensembleTableTextField = null;
	
	private JButton deleteButton = null;
	private JButton closeButton = null;
	private JButton refreshButton = null;
	
	private JTextField stationSearchTextField = null;
	
	private JScrollPane tableScrollPane = null;
	private JPanel tableScrollPanePanel = null;
	
	private JMenuItem fcstPtServiceItem = null;
	private JMenuItem fcstPtWatsUpItem = null;
	private JMenuItem fcstPtDetermItem = null;
	private JMenuItem fcstPtESPItem = null;
	private JMenuItem aboutItem = null;
	private JMenuItem closeItem = null;
	private JMenuItem fcstRefTablesItem = null;
	private JMenu setupTableMenu = null;
	
	private LhvmApplicationWindow lhvmAppWindow = null;
	
	private JFrame lhvm = null;
	private int numRows;
	private JLabel numRowsLabel = null;
	private JTextArea consoleTextArea = null;
	
	private static String jdbcUrl = null;
	
	public void update(Subject sub)
	{
		if(selectedServiceTableViewViewJTableRowsDataList != null && selectedServiceTableViewViewJTableRowsDataList.size() > 0)
		{
			selectedServiceTableViewViewJTableRowData = (ServiceTableViewViewJTableRowData) selectedServiceTableViewViewJTableRowsDataList.get(0);
			showCurrentRowData();
		}
		else if(selectedServiceTableViewViewJTableRowsDataList == null)
		{
    		lhvmRowDataList = lhvmDataManager.readDataFromPtServiceView();
    		jtm.setChangedAllRowDataList(lhvmRowDataList);
    		jtm.refreshDisplay();
    		deActivateDeleteButton();
    		clearForm();
			numRows = lhvmRowDataList.size();
			numRowsLabel.setText(numRows + " Rows");
		}
	}
	
	private void appendToConsoleMessage(String lid)
	{
		String valueToBeSet = new String(consoleTextArea.getText() + "\n" + "Exception getting number of entries from database for lid: " + lid + " ");
		consoleTextArea.insert("", 0);
		consoleTextArea.insert(valueToBeSet, 0);
	}
	
	public LhvmApplicationWindow()
	{		 		
		lhvmAppWindow = this;
		dataServicesGlobals = DataServicesGlobals.getSingleInstanceOfDataServicesGlobals();
		dataServicesGlobals.incrementCount();
		
		lhvm = new JFrame("Forecast Services");
	    lhvm.setSize(1100, 780);
	    lhvm.setLocation(20,50);
	    
	    //Fix the dimensions of this window i.e. dis-allow window resizing
	    Dimension d = new Dimension(1195, 775);
	    new WindowResizingManager(lhvm, d, d);
	     
	    lhvm.setResizable(false);
	    //lhvm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	    
	    GridBagLayout gbl = new GridBagLayout();
	    lhvm.setLayout(gbl);
	    GridBagConstraints c = new GridBagConstraints();
	    c.fill = GridBagConstraints.HORIZONTAL;
	
	
	    JMenuBar menuBar = new JMenuBar();
	
	    JMenu fileMenu = new JMenu("File");
	    menuBar.add(fileMenu);
	    //JMenuItem importItem = new JMenuItem("Import from file");
	    //importItem.setEnabled(false);
	    //JMenuItem exportItem = new JMenuItem("Export to file");
	    //exportItem.setEnabled(false);
	    closeItem = new JMenuItem("Close Application");
	    //closeItem.addActionListener(ActionListener);
	    //fileMenu.add(importItem);
	    //fileMenu.add(exportItem);
	    fileMenu.add(closeItem);
	    closeItem.addActionListener(new MenuSelectActionListener());
	
	    setupTableMenu = new JMenu("Setup");
	    fcstRefTablesItem = new JMenuItem("RefTables");
	    fcstRefTablesItem.addActionListener(new MenuSelectActionListener());
	    //stationsFilterOptionsItem.addActionListener(ActionListener);
	    setupTableMenu.add(fcstRefTablesItem);
	
	    JMenu tablesMenu = new JMenu("ServiceTypes");
	    menuBar.add(tablesMenu);
	    menuBar.add(setupTableMenu);
	    fcstPtServiceItem = new JMenuItem("Data Service");
	    fcstPtWatsUpItem = new JMenuItem("Water Supply Forecast");
	    fcstPtDetermItem = new JMenuItem("Deterministic Forecast");
	    fcstPtESPItem = new JMenuItem("Ensemble Forecast");
	    fcstPtServiceItem.addActionListener(new MenuSelectActionListener());
	    fcstPtDetermItem.addActionListener(new MenuSelectActionListener());
	    fcstPtESPItem.addActionListener(new MenuSelectActionListener());
	    fcstPtWatsUpItem.addActionListener(new MenuSelectActionListener());
	    //stationsFilterOptionsItem.addActionListener(ActionListener);
	    tablesMenu.add(fcstPtServiceItem);
	    tablesMenu.add(fcstPtWatsUpItem);
	    tablesMenu.add(fcstPtDetermItem);
	    tablesMenu.add(fcstPtESPItem);
	    
	    JMenu aboutMenu = new JMenu("Help");
	    menuBar.add(aboutMenu);
	    aboutItem = new JMenuItem("About Forecast Services");
	    aboutMenu.add(aboutItem);
	    aboutItem.addActionListener(new MenuSelectActionListener());
	
	
	    // Install the menu bar in the frame
	    lhvm.setJMenuBar(menuBar);
	
	
	
	
	
	    JLabel consoleLabel = new JLabel("-----Database Messages-----");
	    consoleLabel.setForeground(Color.red);
	    consoleTextArea = new JTextArea(2,50);
	
	    //Font font = new Font("Serif", Font.BOLD, 20);
	    //consoleTextArea.setFont(font);
	    consoleTextArea.setForeground(Color.green);
	    consoleTextArea.setBackground(Color.black);
	    consoleTextArea.setCaretColor(Color.red);
	    consoleTextArea.setBorder(BorderFactory.createEtchedBorder() );
	    consoleTextArea.setLineWrap(true);

	    
	    String[] columnsToBeDisplayed = {"Location ID","Name", "Stream", "State","County","Hsa"};
	    lhvmLogger = new LhvmLogger();
	    lhvmDataManager = LhvmDataManager.getSingleInstanceOfDataManager(jdbcUrl, lhvmLogger,dataServicesGlobals.getMissingRepresentation());
	    java.util.List lhvmColumnDescriptorList = new ArrayList();
	    lhvmRowDataList = lhvmDataManager.readDataFromPtServiceView();
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Location ID", true, 150, "center"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Name", true, 250, "center"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Stream", true, 200, "center"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("State", true, 125, "center"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("County", true, 200, "center"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Hsa", true, 125, "center"));
	    jtm = new ComplexJTableManager(lhvmColumnDescriptorList, lhvmRowDataList);
	    jtm.setDisplayableColumns(columnsToBeDisplayed, false, true);
	    jtm.setPreferredSizeForJScrollPane(new Dimension(1100, 500));
	    jtm.addTableListener(new ServiceTableViewListSelectListener());
	    tableScrollPane = jtm.getJScrollPane();
	    //jtm.setTableToolTipText("str");
	    tableScrollPanePanel = new JPanel();
		tableScrollPanePanel.add(tableScrollPane);
	    
	    String tempStr = null;
	    if(LhvmDataManager.displayedMessageLhvmWindow != null)
	    {
	    	tempStr = new String(LhvmDataManager.displayedMessageLhvmWindow);
	    }
	    LhvmDataManager.displayedMessageLhvmWindow = new StringBuffer();
	    LhvmDataManager.displayedMessageLhvmWindow.append(LhvmDataManager.databaseMessage);
	    if(tempStr != null)
	    {
	    	LhvmDataManager.displayedMessageLhvmWindow.append(tempStr);
	    }
	    consoleTextArea.insert(new String(LhvmDataManager.displayedMessageLhvmWindow), 0);
		
		// Get paragraph element
	    Element paragraph = consoleTextArea.getDocument().getDefaultRootElement();
	
	    // Get number of content elements
	    int contentCount = paragraph.getElementCount();
	
	    // Get index ranges for each content element.
	    // Each content element represents one line.
	    // Each line includes the terminating newline.
	    for (int i=0; i<contentCount; i++) {
	        Element e = paragraph.getElement(i);
	        int rangeStart = e.getStartOffset();
	        int rangeEnd = e.getEndOffset();
	        try 
	        {
	            String line = consoleTextArea.getText(rangeStart,rangeEnd-rangeStart);
	        } catch (BadLocationException ex) {}
	    }
	    JScrollPane scrollableTextAreaForConsole = new JScrollPane(consoleTextArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	    
	    JLabel emptyLabel1 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel2 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel3 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel4 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel5 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel6 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel7 = new JLabel("      ");//6 spaces
	    JLabel emptyLabel8 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel9 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel10 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel11 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel12 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel13 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel14 = new JLabel("                                                  ");//25 spaces
	    JLabel emptyLabel15 = new JLabel("                                                  ");//25 spaces
	
	    numRows = lhvmRowDataList.size();
	    numRowsLabel = new JLabel(numRows + " Rows");
	    c.gridwidth = 1;
	    c.gridx = 0;
	    c.gridy = 0;
	    lhvm.getContentPane ().add(numRowsLabel, c);
	    c.gridx++;
	    //lhvm.getContentPane ().add(emptyLabel8, c);

	    JLabel stationSearchLabel = new JLabel("                         Station Search:");//25 spaces to adjust alignment
	    c.gridx++;
	    lhvm.getContentPane ().add(stationSearchLabel, c);
	    
	    stationSearchTextField = new JTextField(10);
	    stationSearchTextField.setFocusable(true);
	    Font fnt = stationSearchTextField.getFont();
	    fnt = fnt.deriveFont(Font.BOLD, 1.2F*fnt.getSize2D());
	    stationSearchTextField.setForeground(new Color(50,150,50));
	    stationSearchTextField.setBackground(Color.yellow);
	    stationSearchTextField.setFont(fnt);
	    stationSearchTextField.setCaretColor(Color.red);
	    stationSearchTextField.addKeyListener(new SearchTextTypedActionListener());
	    c.gridx++;
	    lhvm.getContentPane ().add(stationSearchTextField, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(emptyLabel1, c);
		refreshButton = new JButton("Refresh");
		refreshButton.addActionListener(new RefreshActionListener());
	    c.gridx++;
	    lhvm.getContentPane ().add(refreshButton, c);	
	    
	    JButton filterOptionsButton = new JButton("Stations Filter Options");
	    filterOptionsButton.setEnabled(false);
	    c.gridx++;
	    //lhvm.getContentPane ().add(filterOptionsButton, c);
	    lhvm.getContentPane ().add(emptyLabel6, c);
	    
	    c.gridy++;  
	    lhvm.getContentPane().add(emptyLabel9, c);
	    c.gridwidth = 0;
	    c.gridx = 0;
	    c.gridy++;
	    lhvm.getContentPane().add(tableScrollPanePanel,c);
	 
	    //int[] selectedRowIndices = jtm.getSelectedRowIndices();
	    
		
	    JPanel editPanel = new JPanel();
		editPanel.setLayout(new GridLayout(2,8));
		editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Entries for the selected location in each table"));
		editPanel.setBackground(new Color(0,150,255));
		
		c.gridy++;
		
		JLabel servicesTableLabel = new JLabel("Data Services");
		JLabel deterministicTableLabel = new JLabel("Deterministic Services");
		JLabel watsupTableLabel = new JLabel("Water Supply Services");
		JLabel ensembleTableLabel = new JLabel("Ensemble Services");
		
		servicesTableTextField = new JTextField();
		servicesTableTextField.setBackground(Color.yellow);
		servicesTableTextField.setForeground(Color.red);
		servicesTableTextField.setFont(fnt);
		servicesTableTextField.setCaretColor(Color.red);
		servicesTableTextField.setSize(15,15);
		servicesTableTextField.setEditable(false);
		deterministicTableTextField = new JTextField();
		deterministicTableTextField.setBackground(Color.yellow);
		deterministicTableTextField.setForeground(Color.red);
		deterministicTableTextField.setFont(fnt);
		deterministicTableTextField.setCaretColor(Color.red);
		deterministicTableTextField.setSize(15,15);
		deterministicTableTextField.setEditable(false);
		watsupTableTextField = new JTextField();
		watsupTableTextField.setBackground(Color.yellow);
		watsupTableTextField.setForeground(Color.red);
		watsupTableTextField.setFont(fnt);
		watsupTableTextField.setCaretColor(Color.red);
		watsupTableTextField.setSize(15,15);
		watsupTableTextField.setEditable(false);
		ensembleTableTextField = new JTextField();
		ensembleTableTextField.setBackground(Color.yellow);
		ensembleTableTextField.setForeground(Color.red);
		ensembleTableTextField.setFont(fnt);
		ensembleTableTextField.setCaretColor(Color.red);
		ensembleTableTextField.setSize(15,15);
		ensembleTableTextField.setEditable(false);
		
		editPanel.add(servicesTableLabel);
		editPanel.add(deterministicTableLabel);
		editPanel.add(watsupTableLabel);
		editPanel.add(ensembleTableLabel);
		
		editPanel.add(servicesTableTextField);
		editPanel.add(deterministicTableTextField);
		editPanel.add(watsupTableTextField);
		editPanel.add(ensembleTableTextField);
		
		lhvm.getContentPane().add(editPanel,c);		
	    
	    deleteButton = new JButton("Delete Record");
	    deleteButton.addActionListener(new DeleteActionListener());
	    
		closeButton = new JButton("Close Window");
		closeButton.addActionListener(new CloseActionListener());
	    
		deleteButton.setEnabled(false);		
		closeButton.setEnabled(true);
		refreshButton.setEnabled(true);
	    
	    c.gridwidth = 1;
	    c.gridx = 0;
	    c.gridy++;
	    //lhvm.getContentPane ().add(deleteButton, c);
	    lhvm.getContentPane ().add(emptyLabel7, c);
	    //c.gridx++;
	    //lhvm.getContentPane ().add(refreshButton, c);	
	    c.gridx++;
	    lhvm.getContentPane ().add(emptyLabel3, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(emptyLabel13, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(emptyLabel4, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(emptyLabel5, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(closeButton, c);
	    
	    c.gridwidth = 0;
	    c.gridx = 0;
	    c.gridy++;
	    lhvm.getContentPane ().add(emptyLabel15, c);
	    c.gridy++;
	    
	    c.gridy++;   
	    lhvm.getContentPane().add(emptyLabel2, c);
	    c.gridy++;   
	    
	    lhvm.getContentPane ().add(consoleLabel, c);
	    c.gridy++;
	    lhvm.getContentPane().add(scrollableTextAreaForConsole, c);
	
	    lhvm.addWindowListener(new ServicesDbWindowAdapter());
	    
	    lhvm.pack();
	    //Show the JFrame
	    lhvm.setVisible (true);
	 }
	
	private void showCurrentRowData()
	{
		if(selectedServiceTableViewViewJTableRowData != null)
		{
			String lid = selectedServiceTableViewViewJTableRowData.getLid();
			int numRows = lhvmDataManager.entriesPerLidInDeterm(lid);
			if(numRows != -1)
			{
				deterministicTableTextField.setText(Integer.toString(numRows));
			}
			else
			{
				appendToConsoleMessage(lid);
				JOptionPane.showMessageDialog(null, "Oops: There was an exception retrieving number of rows from database, please check console");
			}
			
			numRows = lhvmDataManager.entriesPerLidInService(lid);
			if(numRows != -1)
			{
				servicesTableTextField.setText(Integer.toString(numRows));
			}
			else
			{
				appendToConsoleMessage(lid);
				JOptionPane.showMessageDialog(null, "Oops: There was an exception retrieving number of rows from database, please check console");
			}
			
			numRows = lhvmDataManager.entriesPerLidInWatsup(lid);
			if(numRows != -1)
			{
				watsupTableTextField.setText(Integer.toString(numRows));
			}
			else
			{
				appendToConsoleMessage(lid);
				JOptionPane.showMessageDialog(null, "Oops: There was an exception retrieving number of rows from database, please check console");
			}
			
			numRows = lhvmDataManager.entriesPerLidInEnsemble(lid);
			if(numRows != -1)
			{
				ensembleTableTextField.setText(Integer.toString(numRows));
			}
			else
			{
				appendToConsoleMessage(lid);
				JOptionPane.showMessageDialog(null, "Oops: There was an exception retrieving number of rows from database, please check console");
			}
			
			selectedServiceTableViewViewJTableRowData = null;			
			activateDeleteButton();
		}
		else
		{
			clearForm();
		}
	} 
	
	private void deActivateDeleteButton()
	{
		deleteButton.setEnabled(false);
	}
	private void activateDeleteButton()
	{
		//deleteButton.setEnabled(true);
	}
	
	private void removeHighlightOfAllRows()
	{
		int indices[] = new int[lhvmRowDataList.size()];
		for(int i=0;i<lhvmRowDataList.size();i++)
		{
			indices[i] = i;
		}
		jtm.deselectRows(indices);
	}
	
	private void clearForm()
	{
		servicesTableTextField.setText("");
		deterministicTableTextField.setText("");
		watsupTableTextField.setText("");
		ensembleTableTextField.setText("");
	} 
	
	public void buttonsActivateWhenSearched()
	{	
		if(selectedServiceTableViewViewJTableRowsDataList != null && selectedServiceTableViewViewJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			clearForm();
		}
		else if(selectedServiceTableViewViewJTableRowsDataList != null && selectedServiceTableViewViewJTableRowsDataList.size() == 1)
		{
			selectedServiceTableViewViewJTableRowData = (ServiceTableViewViewJTableRowData) selectedServiceTableViewViewJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
		}
		else if(selectedServiceTableViewViewJTableRowsDataList != null && selectedServiceTableViewViewJTableRowsDataList.size() > 1)
		{
			activateDeleteButton();
			clearForm();
		}
	}
	
	public void buttonsActivateWhenSelected()
	{
		getSelectedRows();	
		if(selectedServiceTableViewViewJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			clearForm();
		}
		else if(selectedServiceTableViewViewJTableRowsDataList.size() == 1)
		{
			selectedServiceTableViewViewJTableRowData = (ServiceTableViewViewJTableRowData) selectedServiceTableViewViewJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
		}
		else if(selectedServiceTableViewViewJTableRowsDataList.size() > 1)
		{
			activateDeleteButton();
			clearForm();
		}
	}
    
	private void getSelectedRows()
	{
		selectedServiceTableViewViewJTableRowsDataList = (ArrayList) jtm.getSelectedRowsData();
	}
	
	private void operationCloseWindow()
	{
		if(dataServicesGlobals.getCount() <= 1)
		{
			System.exit(0);
		}
		else
		{
			dataServicesGlobals.decrementCount();
			lhvm.dispose();
		}
	}
    
    private class MenuSelectActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		if(e.getSource().equals(fcstPtServiceItem))
    		{
    			FcstDataServiceWindow fcstDataServiceWindow = new FcstDataServiceWindow(jdbcUrl);
    			dataServicesGlobals.incrementCount();
    			fcstDataServiceWindow.addObserver(lhvmAppWindow);
    		} 
    		else if(e.getSource().equals(fcstPtWatsUpItem))
    		{   	
    			FcstDataWatSupWindow fcstDataWatSupWindow = new FcstDataWatSupWindow(jdbcUrl);
    			dataServicesGlobals.incrementCount();
    			fcstDataWatSupWindow.addObserver(lhvmAppWindow);
    		}   	
    		else if(e.getSource().equals(fcstPtDetermItem))
    		{
    			FcstDataDetermWindow fcstDataDetermWindow = new FcstDataDetermWindow(jdbcUrl);
    			dataServicesGlobals.incrementCount();
    			fcstDataDetermWindow.addObserver(lhvmAppWindow);
    		}   	
    		else if(e.getSource().equals(fcstPtESPItem))
    		{
    			FcstDataEnsembleWindow fcstDataEnsembleWindow = new FcstDataEnsembleWindow(jdbcUrl);
    			dataServicesGlobals.incrementCount();
    			fcstDataEnsembleWindow.addObserver(lhvmAppWindow);
    		} 
    		else if(e.getSource().equals(fcstRefTablesItem))
    		{
    			new ReferenceTablesWindow(jdbcUrl);
    			dataServicesGlobals.incrementCount();
    		}
    		else if(e.getSource().equals(closeItem))
    		{
    			System.exit(0);
    		}
    		else if(e.getSource().equals(aboutItem))
    		{
    			JOptionPane.showMessageDialog(null, "Application:        Forecast Services\n\n" +
    					"Version:              Awips Release OB9.0\n\n" + "Date:                   May 1'st, 2008\n\n" + 
    					"Developed by:     National Weather Service,\n                           Office of Hydrologic development, \n                           Hydrology Laboratory\n\n", 
    					"Forecast Services", JOptionPane.INFORMATION_MESSAGE);   			
    		}
        }
    }
	
	private class DeleteActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
        }
    }
	
	private class RefreshActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		lhvmRowDataList = lhvmDataManager.readDataFromPtServiceView();
    		jtm.setChangedAllRowDataList(lhvmRowDataList);
    		jtm.refreshDisplay();
    		deActivateDeleteButton();
    		clearForm();
			numRows = lhvmRowDataList.size();
			numRowsLabel.setText(numRows + " Rows");
        }
    }
    
    private class CloseActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		operationCloseWindow();
        }
    }
    
    private class ServiceTableViewListSelectListener implements ListSelectionListener
	{
    	public void valueChanged(ListSelectionEvent e)
    	{
    		buttonsActivateWhenSelected();
    	}
	}
    
    private class SearchTextTypedActionListener implements KeyListener
    {	
    	String currentLidFromRowsInsideLoop;
    	int[] rowIndices;
    	int index;
    	
    	public void init()
    	{
    		index = -1;
    		rowIndices = new int[lhvmRowDataList.size()];
    		
			for(int i=0;i<lhvmRowDataList.size();i++)
			{
				rowIndices[i] = -1;
			}
    	}
    	
    	public void keyTyped(KeyEvent e) 
        {   		
        }
    	public void keyPressed(KeyEvent e)
    	{
    	}
    	public void keyReleased(KeyEvent e)
    	{
    		removeHighlightOfAllRows();
    		ArrayList searchList = new ArrayList();
    		String typedText = stationSearchTextField.getText();
    		init();
    		if(typedText.length() > 0 && lhvmRowDataList.size() > 0)
    		{
	    		int count = 0, i;
    			for(i=0;i<lhvmRowDataList.size();i++)
	    		{
    				currentLidFromRowsInsideLoop = ((ServiceTableViewViewJTableRowData)lhvmRowDataList.get(i)).getLid();
	    			if((typedText.length() <= currentLidFromRowsInsideLoop.length()) && typedText.equalsIgnoreCase(currentLidFromRowsInsideLoop.substring(0, typedText.length())))
	    			{
	    				searchList.add(lhvmRowDataList.get(i));
	    				rowIndices[count++] = i;
	    			}
	    		} 			
    		}
    		else
    		{
    			jtm.selectRows(0, 0);
        		clearForm();
        		removeHighlightOfAllRows();
        		deActivateDeleteButton();
    			return;
    		}
    		selectedServiceTableViewViewJTableRowsDataList = searchList;
    		buttonsActivateWhenSelected();
    		for(int i=0;i<rowIndices.length;i++)
    		{
    			if(rowIndices[i] == -1)
    			{
    				break;
    			}
    			index++;
    		}
    		if(index != -1)
    		{
    			//jtm.highlightRows(rowIndices[0], rowIndices[index]);
    			jtm.selectRows(rowIndices);
    		}
    	}
    }
    
    public static void main(String args[])
	{
		    try
		    {
		            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		    }catch(Exception e){}
		    jdbcUrl = args[0];
		    new LhvmApplicationWindow();
	 }
}
