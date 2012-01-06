package ohd.hseb.fcstservice;   

import java.util.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.FcstPtServiceRecord;
import ohd.hseb.ihfsdb.generated.FcstPtServiceTable;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.util.gui.*;

public class FcstDataServiceWindow implements Subject
{
	private DataServicesGlobals dataServicesGlobals = null;
	private java.util.List lhvmRowDataList = null;
	private LhvmDataManager lhvmDataManager = null;
	private LhvmLogger lhvmLogger = null;
	private JTableManager jtm = null;
	private JFrame lhvm = null;
	
	private FcstPtServiceJTableRowData selectedFcstPtServiceJTableRowData = null;
	private ArrayList selectedFcstPtServiceJTableRowsDataList = null;
	
	private JTextField locationTextField = null;
	private JTextField floodThersholdTextField = null;
	private JTextField exceedProbTextField = null;
	private JComboBox serviceTypeTextField = null;
	private JComboBox verifRespTypeTextField = null;
	private JTextField drainageAreaTextField = null;
	private DateTextField implDateTextField = null;	
	private DateTextField webDateTextField = null;
	private DateTextField startDateTextField = null;	
	private DateTextField endDateTextField = null;
	
	private JButton insertButton = null;
	private JButton deleteButton = null;
	private JButton clearButton = null;
	private JButton closeButton = null;
	private JButton refreshButton = null;
	
	private JTextField stationSearchTextField = null;
	
	private JScrollPane tableScrollPane = null;
	private JPanel tableScrollPanePanel = null;
	
	private int numRows;
	private JLabel numRowsLabel = null;
	private JTextArea consoleTextArea = null;
	private int origNumRows;
	
	private ArrayList observers = null;
	
	private java.util.List lhvmRowDataList1 = null;
	private java.util.List lhvmRowDataList2 = null;
	private String str1[] = null;
	private String str2[] = null;
	
	public void addObserver(Observer observer)
	{
		if(observers != null)
		{
			observers.add(observer);
		}
	}
	public void removeObserver(Observer observer)
	{
		if(observers != null)
		{
			observers.remove(observer);
		}
	}
	public void letKnowOfChanges()
	{
		if(observers != null)
		{
			for(int i=0;i<observers.size();i++)
			{
				if(observers.get(i) != null)
				{
					((Observer) observers.get(i)).update(this);
				}
			}
		}
	}
	
	private void setDatabaseMessage()
	{
		String tempStr = null;
	    if(LhvmDataManager.displayedMessageServiceWindow != null)
	    {
	    	tempStr = new String(LhvmDataManager.displayedMessageServiceWindow);
	    }
	    LhvmDataManager.displayedMessageServiceWindow = new StringBuffer();
	    if(tempStr != null)
	    {
	    	LhvmDataManager.displayedMessageServiceWindow.append(tempStr);
	    }
	    LhvmDataManager.displayedMessageServiceWindow.append("\n--------------\n");
	    LhvmDataManager.displayedMessageServiceWindow.append(LhvmDataManager.databaseMessage);
	    //consoleTextArea.insert(new String(LhvmDataManager.displayedMessageServiceWindow), 0);
	    consoleTextArea.append(new String(LhvmDataManager.displayedMessageServiceWindow));
	}
	
	private void appendToConsoleMessageDelete(String lid)
	{
		String valueToBeSet = new String("\n" + "Successfully deleted row with lid: " + lid + " ");
		//consoleTextArea.insert("", 0);
		//consoleTextArea.insert(valueToBeSet, 0);
		consoleTextArea.append(valueToBeSet);
	}
	
	private void appendToConsoleMessageInsert(String lid)
	{
		String valueToBeSet = new String("\n" + "Successfully inserted row with lid: " + lid.toUpperCase() + " ");
		//consoleTextArea.insert("", 0);
		//consoleTextArea.insert(valueToBeSet, 0);
		consoleTextArea.append(valueToBeSet);
	}
	
	public FcstDataServiceWindow(String jdbcUrl)
	{		 
		dataServicesGlobals = DataServicesGlobals.getSingleInstanceOfDataServicesGlobals();
		this.observers = DataServicesGlobals.dataServiceObservers;
		
		lhvm = new JFrame("Data Services");
	    lhvm.setSize(1000, 500);
	    lhvm.setLocation(20,50);
	    
	    //Fix the dimensions of this window i.e. dis-allow window resizing
	    Dimension d = new Dimension(1060, 825);
	    new WindowResizingManager(lhvm, d, d);
	    
	    lhvm.setResizable(false);
	    
	    GridBagLayout gbl = new GridBagLayout();
	    lhvm.setLayout(gbl);
	    GridBagConstraints c = new GridBagConstraints();
	    c.fill = GridBagConstraints.HORIZONTAL;
	
	    JLabel consoleLabel = new JLabel("-----Database Messages-----");
	    consoleLabel.setForeground(Color.red);
	    //consoleTextArea = new JTextArea(2,50);
	    consoleTextArea = new JTextArea(4,50);
	
	    //Font font = new Font("Serif", Font.BOLD, 20);
	    //consoleTextArea.setFont(font);
	    consoleTextArea.setForeground(Color.green);
	    consoleTextArea.setBackground(Color.black);
	    consoleTextArea.setCaretColor(Color.red);
	    consoleTextArea.setBorder(BorderFactory.createEtchedBorder() );
	    consoleTextArea.setLineWrap(true);

	    
	    String[] columnsToBeDisplayed = {"Location ID", "Flood Threshold", "Exceed Probability %", "Service Type", "Analysis Start Date", "Analysis End Date", "Implementation Date", "Web Date", "Verification Response Type", "Drainage Area"};
	    lhvmLogger = new LhvmLogger();
	    lhvmDataManager = LhvmDataManager.getSingleInstanceOfDataManager(jdbcUrl, lhvmLogger,"-");
	    java.util.List lhvmColumnDescriptorList = new ArrayList();
	    lhvmRowDataList = lhvmDataManager.readDataFromPtService();
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Location ID", true, 150, "center", "location id"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Flood Threshold", true, 150, "center", "flood threshold"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Exceed Probability %", true, 150, "center","exceed probability (%)"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Service Type", true, 150, "center", "service type (data or forecast)"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Analysis Start Date", true, 150, "center", "start date for analysis of the exceedance probability for a given flood threshold"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Analysis End Date", true, 150, "center", "end date for analysis of the exceedance probability for a given flood threshold"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Implementation Date", true, 150, "center", "date observed data first used as input to RFC hydrologic procedures"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Web Date", true, 150, "center", "date observed hydrograph first available on web"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Verification Response Type", true, 150, "center", ""));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Drainage Area", true, 150, "center", ""));
	    jtm = new ComplexJTableManager(lhvmColumnDescriptorList, lhvmRowDataList);
	    jtm.setDisplayableColumns(columnsToBeDisplayed, false, true);
	    jtm.setPreferredSizeForJScrollPane(new Dimension(1000, 500));
	    jtm.addTableListener(new ServiceTableViewListSelectListener());
	    tableScrollPane = jtm.getJScrollPane();
	    tableScrollPanePanel = new JPanel();
		tableScrollPanePanel.add(tableScrollPane);
	    
		setDatabaseMessage();
		
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
	    JLabel emptyLabel7 = new JLabel("                                                  ");//25 spaces
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
	    
	    JButton filterOptionsButton = new JButton("Stations Filter Options");
	    filterOptionsButton.setEnabled(false);
	    c.gridx++;
	    //lhvm.getContentPane ().add(filterOptionsButton, c);
	    lhvm.getContentPane ().add(emptyLabel5, c);
	    
	    c.gridy++;  
	    lhvm.getContentPane().add(emptyLabel9, c);
	    c.gridwidth = 0;
	    c.gridx = 0;
	    c.gridy++;
	    lhvm.getContentPane().add(tableScrollPanePanel,c);
	 
	    //int[] selectedRowIndices = jtm.getSelectedRowIndices();
	    
		
	    JPanel editPanel = new JPanel();
		editPanel.setLayout(new GridLayout(2,5));
		editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor"));
		editPanel.setBackground(new Color(0,150,255));
		
	    JPanel editPanel2 = new JPanel();
	    editPanel2.setLayout(new GridLayout(2,5));
		//editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Fields with red text are non-editable"));
		editPanel2.setBackground(new Color(0,150,255));
		
		c.gridy++;
		
		JLabel locationIdLabel = new JLabel("Location ID");
		JLabel floodThersholdLabel = new JLabel("Flood Threshold");
		JLabel exceedProbLabel = new JLabel("Exceed Probability %");
		JLabel serviceTypeLabel = new JLabel("Service Type");
		JLabel startDateLabel = new JLabel("Analysis Start Date");
		JLabel endDateLabel = new JLabel("Analysis End Date");
		JLabel implDateLabel = new JLabel("Implementation Date");
		JLabel webDateLabel = new JLabel("Web Date");
		JLabel verifRespLabel = new JLabel("Verification Response Type");
		JLabel drainageAreaLabel = new JLabel("Drainage Area");
		
		locationTextField = new JTextField();
		locationTextField.setBackground(new Color(255,175,75));
		locationTextField.setForeground(new Color(50,150,50));
		locationTextField.setFont(fnt);
		locationTextField.setCaretColor(Color.red);
		locationTextField.setSize(15,15);
		TextTypedActionListener textTypedActionListener = new TextTypedActionListener();
		locationTextField.addKeyListener(textTypedActionListener);
		floodThersholdTextField = new JTextField();
		floodThersholdTextField.setBackground(Color.yellow);
		floodThersholdTextField.setForeground(new Color(50,150,50));
		floodThersholdTextField.setFont(fnt);
		floodThersholdTextField.setCaretColor(Color.red);
		floodThersholdTextField.setSize(15,15);
		floodThersholdTextField.addKeyListener(textTypedActionListener);
		exceedProbTextField = new JTextField();
		exceedProbTextField.setBackground(Color.yellow);
		exceedProbTextField.setForeground(new Color(50,150,50));
		exceedProbTextField.setFont(fnt);
		exceedProbTextField.setCaretColor(Color.red);
		exceedProbTextField.setSize(15,15);
		exceedProbTextField.addKeyListener(textTypedActionListener);		
		implDateTextField = new DateTextField(0, lhvm, "Date Selector", 10, dataServicesGlobals.getMissingRepresentation());
		implDateTextField.setBackground(Color.yellow);
		implDateTextField.setForeground(new Color(50,150,50));
		implDateTextField.setFont(fnt);
		implDateTextField.setCaretColor(Color.red);
		implDateTextField.setSize(15,15);
		webDateTextField = new DateTextField(0, lhvm, "Date Selector", 10, dataServicesGlobals.getMissingRepresentation());
		webDateTextField.setBackground(Color.yellow);
		webDateTextField.setForeground(new Color(50,150,50));
		webDateTextField.setFont(fnt);
		webDateTextField.setCaretColor(Color.red);
		webDateTextField.setSize(15,15);
		endDateTextField = new DateTextField(0, lhvm, "Date Selector", 10, dataServicesGlobals.getMissingRepresentation());
		endDateTextField.setBackground(Color.yellow);
		endDateTextField.setForeground(new Color(50,150,50));
		endDateTextField.setFont(fnt);
		endDateTextField.setCaretColor(Color.red);
		endDateTextField.setSize(15,15);
		startDateTextField = new DateTextField(0, lhvm, "Date Selector", 10, dataServicesGlobals.getMissingRepresentation());
		startDateTextField.setBackground(Color.yellow);
		startDateTextField.setForeground(new Color(50,150,50));
		startDateTextField.setFont(fnt);
		startDateTextField.setCaretColor(Color.red);
		startDateTextField.setSize(15,15);
		drainageAreaTextField = new JTextField();
		drainageAreaTextField.setBackground(Color.yellow);
		drainageAreaTextField.setForeground(new Color(50,150,50));
		drainageAreaTextField.setFont(fnt);
		drainageAreaTextField.setCaretColor(Color.red);
		drainageAreaTextField.setSize(15,15);
		drainageAreaTextField.addKeyListener(textTypedActionListener);
		
		lhvmRowDataList1 = lhvmDataManager.readDataFromServiceType();
		str1 = new String[lhvmRowDataList1.size()];
		
		lhvmRowDataList2 = lhvmDataManager.readDataFromVerificationResponseType();
		str2 = new String[lhvmRowDataList2.size()];
		
		for(int i=0;i<lhvmRowDataList1.size();i++)
		{
			str1[i] = ((ServiceTypeJTableRowData) lhvmRowDataList1.get(i)).getServiceType();
		}
		
		for(int i=0;i<lhvmRowDataList2.size();i++)
		{
			str2[i] = ((VerificationResponseTypeJTableRowData) lhvmRowDataList2.get(i)).getVerifRespType();
		}
		
		serviceTypeTextField = new JComboBox(str1);
		serviceTypeTextField.setBackground(Color.yellow);
		serviceTypeTextField.setForeground(new Color(50,150,50));
		
		verifRespTypeTextField = new JComboBox(str2);
		verifRespTypeTextField.setBackground(Color.yellow);
		verifRespTypeTextField.setForeground(new Color(50,150,50));
		
		editPanel.add(locationIdLabel);		
		editPanel.add(floodThersholdLabel);
		editPanel.add(exceedProbLabel);
		editPanel.add(serviceTypeLabel);
		editPanel.add(startDateLabel);
				
		editPanel.add(locationTextField);		
		editPanel.add(floodThersholdTextField);
		editPanel.add(exceedProbTextField);
		editPanel.add(serviceTypeTextField);
		editPanel.add(startDateTextField);
		
		editPanel2.add(endDateLabel);		
		editPanel2.add(implDateLabel);
		editPanel2.add(webDateLabel);
		editPanel2.add(verifRespLabel);
		editPanel2.add(drainageAreaLabel);
		
		editPanel2.add(endDateTextField);
		editPanel2.add(implDateTextField);
		editPanel2.add(webDateTextField);
		editPanel2.add(verifRespTypeTextField);
		editPanel2.add(drainageAreaTextField);
		
		lhvm.getContentPane().add(editPanel,c);
		c.gridy++;
		c.gridy++;
		lhvm.getContentPane().add(editPanel2,c);		
	    
	    insertButton = new JButton("Save Record");	    
	    insertButton.addActionListener(new SaveActionListener());
	    
	    deleteButton = new JButton("Delete Record(s)");
	    deleteButton.addActionListener(new DeleteActionListener());
	    
	    clearButton = new JButton("Clear");
	    clearButton.addActionListener(new ClearActionListener());
	    
		closeButton = new JButton("Close Window");
		closeButton.addActionListener(new CloseActionListener());
		
		//refreshButton = new JButton("Refresh Entries");
		//refreshButton.addActionListener(new RefreshActionListener());
	    
		insertButton.setEnabled(false);
		deleteButton.setEnabled(false);
		clearButton.setEnabled(false);
		//refreshButton.setEnabled(true);		
		closeButton.setEnabled(true);
	    
	    c.gridwidth = 1;
	    c.gridx = 0;
	    c.gridy++;
	    lhvm.getContentPane ().add(insertButton, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(deleteButton, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(clearButton, c);
	    //c.gridx++;
	    //lhvm.getContentPane ().add(refreshButton, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(emptyLabel3, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(emptyLabel4, c);
	    //c.gridx++;
	    //lhvm.getContentPane ().add(emptyLabel5, c);
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
		if(selectedFcstPtServiceJTableRowData != null)
		{
			locationTextField.setText(selectedFcstPtServiceJTableRowData.getDataValue("Location ID"));
			floodThersholdTextField.setText(selectedFcstPtServiceJTableRowData.getDataValue("Flood Threshold"));
			drainageAreaTextField.setText(selectedFcstPtServiceJTableRowData.getDataValue("Drainage Area"));
			exceedProbTextField.setText(selectedFcstPtServiceJTableRowData.getDataValue("Exceed Probability %"));
			implDateTextField.setText(selectedFcstPtServiceJTableRowData.getDataValue("Implementation Date"));
			webDateTextField.setText(selectedFcstPtServiceJTableRowData.getDataValue("Web Date"));
			startDateTextField.setText(selectedFcstPtServiceJTableRowData.getDataValue("Analysis Start Date"));
			endDateTextField.setText(selectedFcstPtServiceJTableRowData.getDataValue("Analysis End Date"));
			serviceTypeTextField.setSelectedItem(selectedFcstPtServiceJTableRowData.getDataValue("Service Type"));
			verifRespTypeTextField.setSelectedItem(selectedFcstPtServiceJTableRowData.getDataValue("Verification Response Type"));
			selectedFcstPtServiceJTableRowData = null;
			
			activateSaveButton();
			activateClearButton();
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
	private void deActivateSaveButton()
	{
		insertButton.setEnabled(false);
	}
	private void deActivateClearButton()
	{
		clearButton.setEnabled(false);
	}
	private void activateDeleteButton()
	{
		deleteButton.setEnabled(true);
	}
	private void activateSaveButton()
	{
		insertButton.setEnabled(true);
	}
	private void activateClearButton()
	{
		clearButton.setEnabled(true);
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
		locationTextField.setText("");
		floodThersholdTextField.setText("");
		drainageAreaTextField.setText("");
		exceedProbTextField.setText("");
		implDateTextField.setText("yyyy-mm-dd");
		webDateTextField.setText("yyyy-mm-dd");
		startDateTextField.setText("yyyy-mm-dd");
		endDateTextField.setText("yyyy-mm-dd");
	} 
	
	public void buttonsActivateWhenKeysTyped()
    {
		int either = 0;
		
		if(floodThersholdTextField.getText().length() > 0 || drainageAreaTextField.getText().length() > 0 || exceedProbTextField.getText().length() > 0)
    	{
    		activateClearButton();
    		either++;
    	}
    	if(either == 0)
    	{
    		deActivateClearButton();
    	}
		
		if(locationTextField.getText().length() > 0)
    	{
    		activateSaveButton();
    		activateClearButton();
    	}
    	else
    	{
    		deActivateSaveButton();
    	}
    }
	
	public void buttonsActivateWhenSearched()
	{	
		if(selectedFcstPtServiceJTableRowsDataList != null && selectedFcstPtServiceJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
		else if(selectedFcstPtServiceJTableRowsDataList != null && selectedFcstPtServiceJTableRowsDataList.size() == 1)
		{
			selectedFcstPtServiceJTableRowData = (FcstPtServiceJTableRowData) selectedFcstPtServiceJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
			activateSaveButton();
			activateClearButton();
		}
		else if(selectedFcstPtServiceJTableRowsDataList != null && selectedFcstPtServiceJTableRowsDataList.size() > 1)
		{
			activateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
	}
	
	public void buttonsActivateWhenSelected()
	{
		getSelectedRows();	
		if(selectedFcstPtServiceJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
		else if(selectedFcstPtServiceJTableRowsDataList.size() == 1)
		{
			selectedFcstPtServiceJTableRowData = (FcstPtServiceJTableRowData) selectedFcstPtServiceJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
			activateSaveButton();
			activateClearButton();
		}
		else if(selectedFcstPtServiceJTableRowsDataList.size() > 1)
		{
			activateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
	}
    
	private void getSelectedRows()
	{
		selectedFcstPtServiceJTableRowsDataList = (ArrayList) jtm.getSelectedRowsData();
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
	
	private class SaveActionListener implements ActionListener
    {
        public void actionPerformed(ActionEvent e) 
        {
        	int[] rowIndex = new int[1];
        	String currentLidFromRowsInsideLoop = null;
        	int i, null_or_not = 0;
        	rowIndex[0] = -1;
        	
        	FcstPtServiceRecord record = new FcstPtServiceRecord();
			record.setLid(locationTextField.getText().toUpperCase());
			record.setService_type(str1[serviceTypeTextField.getSelectedIndex()]);
			record.setVerif_resp_type(str2[verifRespTypeTextField.getSelectedIndex()]);
			try
			{
				if(floodThersholdTextField.getText().length() > 0 && !floodThersholdTextField.getText().equalsIgnoreCase(dataServicesGlobals.getMissingRepresentation()))
				{
					record.setFlood_thres(Double.parseDouble(floodThersholdTextField.getText().trim()));
				}
				else
				{
					record.setFlood_thres(Double.POSITIVE_INFINITY);
					null_or_not = 1;
				}
			}
			catch(NumberFormatException ne)
			{
				JOptionPane.showMessageDialog(null, "Not a valid value in the Flood Threshold text box; It must be a number. Aborting insert.");
				return;
			}
			try
			{
				if(!drainageAreaTextField.getText().equalsIgnoreCase(dataServicesGlobals.getMissingRepresentation()))
				{
					record.setDrainage_area(Double.parseDouble(drainageAreaTextField.getText().trim()));
				}
				else
				{
					record.setDrainage_area(Double.POSITIVE_INFINITY);
					null_or_not = 1;
				}
			}
			catch(NumberFormatException ne)
			{
				JOptionPane.showMessageDialog(null, "Not a valid value in the Drainage Area text box; It must be a number. Aborting insert.");
				return;
			}
			try
			{
				if(exceedProbTextField.getText().length() > 0 && !exceedProbTextField.getText().equalsIgnoreCase(dataServicesGlobals.getMissingRepresentation()))
				{
					record.setExceed_prob(Short.parseShort(exceedProbTextField.getText().trim()));
				}
				else
				{
					record.setExceed_prob(Short.MIN_VALUE);
					null_or_not = 1;
				}
			}
			catch(NumberFormatException ne)
			{
				JOptionPane.showMessageDialog(null, "Not a valid value in the Exceed Probability text box; It must be a number. Aborting insert.");
				return;
			}
			Object[] options = { "YES", "NO" };
			if(!implDateTextField.getText().equalsIgnoreCase("yyyy-mm-dd") && !implDateTextField.getText().equalsIgnoreCase(dataServicesGlobals.getMissingRepresentation()))
			{
				record.setImpl_date(DbTimeHelper.getLongTimeFromDateString(implDateTextField.getText()));
			}
			else
			{
				null_or_not = 1;
				record.setImpl_date(Long.MIN_VALUE);
			}
			if(!webDateTextField.getText().equalsIgnoreCase("yyyy-mm-dd") && !webDateTextField.getText().equalsIgnoreCase(dataServicesGlobals.getMissingRepresentation()))
			{
				record.setWeb_date(DbTimeHelper.getLongTimeFromDateString(webDateTextField.getText()));
			}
			else
			{
				null_or_not = 1;
				record.setWeb_date(Long.MIN_VALUE);
			}
			if(!startDateTextField.getText().equalsIgnoreCase("yyyy-mm-dd") && !startDateTextField.getText().equalsIgnoreCase(dataServicesGlobals.getMissingRepresentation()))
			{
				record.setAnal_start_date(DbTimeHelper.getLongTimeFromDateString(startDateTextField.getText()));
			}
			else
			{
				null_or_not = 1;
				record.setAnal_start_date(Long.MIN_VALUE);
			}
			if(!endDateTextField.getText().equalsIgnoreCase("yyyy-mm-dd") && !endDateTextField.getText().equalsIgnoreCase(dataServicesGlobals.getMissingRepresentation()))
			{
				record.setAnal_end_date(DbTimeHelper.getLongTimeFromDateString(endDateTextField.getText()));
			}
			else
			{
				null_or_not = 1;
				record.setAnal_end_date(Long.MIN_VALUE);
			}	
			if(null_or_not == 1)
			{
				int yes_or_no = JOptionPane.showOptionDialog(null, "At least one field has no value defined...continue??", "Not All Fields Defined!", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE,null, options, options[1]);
				if(yes_or_no == JOptionPane.NO_OPTION)
				{
					return;
				}
				null_or_not = 0;
			}
//			lhvmLogger.log(" Delete record"+ record.toString());
			LhvmDataManager.databaseMessage = null;
			int success = lhvmDataManager.insertOrUpdateServiceTable(record);
			if(success != -1)
			{
				appendToConsoleMessageInsert(locationTextField.getText());
			}
			else
			{
				JOptionPane.showMessageDialog(null, "Oops: Problem inserting, please check console");
				setDatabaseMessage();
			}
			lhvmRowDataList = lhvmDataManager.readDataFromPtService();
			jtm.setChangedAllRowDataList(lhvmRowDataList);
			numRows = lhvmRowDataList.size();
			numRowsLabel.setText(numRows + " Rows");
			jtm.refreshDisplay();
			for(i=0;i<lhvmRowDataList.size();i++)
    		{
				currentLidFromRowsInsideLoop = ((FcstPtServiceJTableRowData) lhvmRowDataList.get(i)).getLid();
    			if(locationTextField.getText().equalsIgnoreCase(currentLidFromRowsInsideLoop))
    			{
    				rowIndex[0] = i;
    				break;
    			}
    		}
			if(rowIndex[0] >= 0)
			{
				jtm.selectRows(rowIndex[0], rowIndex[0]);
			}
			letKnowOfChanges();
        }
    }
 
    
    private class DeleteActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		int i, size;
    		origNumRows = lhvmRowDataList.size();
    		size = selectedFcstPtServiceJTableRowsDataList.size();
    		Object[] options = { "YES", "NO" };
    		int yes_or_no = JOptionPane.showOptionDialog(null, "Are you sure to delete row(s)?", "About To Delete Rows!", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE,null, options, options[1]);
    		if(yes_or_no == JOptionPane.NO_OPTION)
    		{
    			return;
    		}
    		else
    		{
	    		for(i=0;i<size;i++)
	    		{
	    			FcstPtServiceRecord record = new FcstPtServiceRecord();
	    			String lid = ((FcstPtServiceJTableRowData)selectedFcstPtServiceJTableRowsDataList.get(i)).getLid();
	    			record.setLid(lid);
	    			//lhvmLogger.log(" Delete record"+ record.toString());
	    			LhvmDataManager.databaseMessage = null;
	    			int success = lhvmDataManager.deleteFromServiceTable(record);
	    			if(success != -1)
	    			{
	    				appendToConsoleMessageDelete(lid);
	    			}
	    			else
	    			{
	    				JOptionPane.showMessageDialog(null, "Oops: Some or all of the rows could not be deleted, please check console");
	    				setDatabaseMessage();
	    			}
	    		}
    		}
    		
    		lhvmRowDataList = lhvmDataManager.readDataFromPtService();
    		jtm.setChangedAllRowDataList(lhvmRowDataList);
    		jtm.refreshDisplay();
			deActivateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
			numRows = lhvmRowDataList.size();
			numRowsLabel.setText(numRows + " Rows");
			if(numRows < origNumRows)
			{
				//JOptionPane.showMessageDialog(null, "Rows deleted, please check console");
			}
			origNumRows = numRows;
			letKnowOfChanges();
        }
    }
    
    private class CloseActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		operationCloseWindow();
        }
    }
    
	private class RefreshActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		lhvmRowDataList = lhvmDataManager.readDataFromPtService();
    		jtm.setChangedAllRowDataList(lhvmRowDataList);
    		jtm.refreshDisplay();
    		deActivateDeleteButton();
    		deActivateClearButton();
    		deActivateSaveButton();
    		clearForm();
			numRows = lhvmRowDataList.size();
			numRowsLabel.setText(numRows + " Rows");
        }
    }
    
    private class ClearActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		clearForm();
    		removeHighlightOfAllRows();
    		deActivateSaveButton();
    		deActivateDeleteButton();
    		deActivateClearButton();
        }
    }
    
    private class TextTypedActionListener implements KeyListener
    {	
    	public void keyTyped(KeyEvent e) 
        {   		
        }
    	public void keyPressed(KeyEvent e)
    	{
    	}
    	public void keyReleased(KeyEvent e)
    	{
    		buttonsActivateWhenKeysTyped();
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
    				currentLidFromRowsInsideLoop = ((FcstPtServiceJTableRowData) lhvmRowDataList.get(i)).getLid();
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
        		deActivateSaveButton();
        		deActivateDeleteButton();
        		deActivateClearButton();
    			return;
    		}
    		selectedFcstPtServiceJTableRowsDataList = searchList;
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
}
