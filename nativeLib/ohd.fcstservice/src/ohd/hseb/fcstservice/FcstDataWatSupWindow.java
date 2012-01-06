package ohd.hseb.fcstservice;   

import java.util.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.*;
import ohd.hseb.util.gui.DateTextField;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.jtable.*;

public class FcstDataWatSupWindow implements Subject
{
	private DataServicesGlobals dataServicesGlobals = null;
	private java.util.List lhvmRowDataList = null;
	private LhvmDataManager lhvmDataManager = null;
	private LhvmLogger lhvmLogger = null;
	private JTableManager jtm = null;
	private JFrame lhvm = null;
	
	private FcstPtWatSupJTableRowData selectedFcstPtWatsupJTableRowData = null;
	private ArrayList selectedFcstPtWatsupJTableRowsDataList = null;

	private JTextField locationTextField = null;
	private JComboBox normalTextField = null;	
	private JComboBox watsupMethodTextField = null;
	private JComboBox watsupCoordAgencyTextField = null;
	private JComboBox periodReqTextField = null;
	private JComboBox watsupCritTextField = null;
	private JComboBox respAgencyTextField = null;
	private JTextField customerDescTextField = null;
	
	private DateTextField implDateTextField = null;	
	private DateTextField webDateTextField = null;	
	
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
	
	private java.util.List lhvmRowDataList1 = null;
	private java.util.List lhvmRowDataList2 = null;
	private java.util.List lhvmRowDataList3 = null;
	private java.util.List lhvmRowDataList4 = null;
	private java.util.List lhvmRowDataList5 = null;
	private java.util.List lhvmRowDataList6 = null;
	
	private String str1[] = null;
	private String str2[] = null;
	private String str3[] = null;
	private String str4[] = null;
	private String str5[] = null;
	private String str6[] = null;
	
	private ArrayList observers = null;
	
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
	    if(LhvmDataManager.displayedMessageWatsupWindow != null)
	    {
	    	tempStr = new String(LhvmDataManager.displayedMessageWatsupWindow);
	    }
	    LhvmDataManager.displayedMessageWatsupWindow = new StringBuffer();
	    if(tempStr != null)
	    {
	    	LhvmDataManager.displayedMessageWatsupWindow.append(tempStr);
	    }
	    LhvmDataManager.displayedMessageWatsupWindow.append("\n--------------\n");
	    LhvmDataManager.displayedMessageWatsupWindow.append(LhvmDataManager.databaseMessage);
	    //consoleTextArea.insert(new String(LhvmDataManager.displayedMessageWatsupWindow), 0);
	    consoleTextArea.append(new String(LhvmDataManager.displayedMessageWatsupWindow));
	}
	
	private void appendToConsoleMessage(String lid)
	{
		String valueToBeSet = new String(consoleTextArea.getText() + "\n" + "Successfully deleted row with lid: " + lid + " ");
		//consoleTextArea.insert("", 0);
		//consoleTextArea.insert(valueToBeSet, 0);
		consoleTextArea.append(valueToBeSet);
	}
	
	private void appendToConsoleMessageInsert(String lid)
	{
		String valueToBeSet = new String(consoleTextArea.getText() + "\n" + "Successfully inserted row with lid: " + lid.toUpperCase() + " ");
		//consoleTextArea.insert("", 0);
		//consoleTextArea.insert(valueToBeSet, 0);
		consoleTextArea.append(valueToBeSet);
	}
	
	private void appendToConsoleMessageReplace(String lid)
	{
		String valueToBeSet = new String(consoleTextArea.getText() + "\n" + "Successfully replaced row with lid: " + lid.toUpperCase() + " ");
		consoleTextArea.append(valueToBeSet);
	}
	
	public FcstDataWatSupWindow(String jdbcUrl)
	{		 
		observers = new ArrayList();
		dataServicesGlobals = DataServicesGlobals.getSingleInstanceOfDataServicesGlobals();
		this.observers = DataServicesGlobals.dataWatSupObservers;
		
		lhvm = new JFrame("Water Supply");
	    lhvm.setSize(1000, 500);
	    lhvm.setLocation(20,50);
	    
	    //Fix the dimensions of this window i.e. dis-allow window resizing
	    Dimension d = new Dimension(1060, 880);
	    new WindowResizingManager(lhvm, d, d);
	    
	    lhvm.setResizable(false);
	    
	    GridBagLayout gbl = new GridBagLayout();
	    lhvm.setLayout(gbl);
	    GridBagConstraints c = new GridBagConstraints();
	    c.fill = GridBagConstraints.HORIZONTAL;
	
	    JLabel consoleLabel = new JLabel("-----Database Messages-----");
	    consoleLabel.setForeground(Color.red);
	    consoleTextArea = new JTextArea(4,50);
	
	    //Font font = new Font("Serif", Font.BOLD, 20);
	    //consoleTextArea.setFont(font);
	    consoleTextArea.setForeground(Color.green);
	    consoleTextArea.setBackground(Color.black);
	    consoleTextArea.setCaretColor(Color.red);
	    consoleTextArea.setBorder(BorderFactory.createEtchedBorder() );
	    consoleTextArea.setLineWrap(true);

	    
	    String[] columnsToBeDisplayed = {"Location ID", "Wat Sup Method", "Wat Sup Coord Agency", "Normal Upd Freq", "Period Req", "Wat Sup Criteria", "Wat Sup Resp Agency", "Customer Desc", "Implementation Date", "Web Date"};
	    lhvmLogger = new LhvmLogger();
	    lhvmDataManager = LhvmDataManager.getSingleInstanceOfDataManager(jdbcUrl, lhvmLogger,"-");
	    java.util.List lhvmColumnDescriptorList = new ArrayList();
	    lhvmRowDataList = lhvmDataManager.readDataFromPtWatsup();
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Location ID", true, 120, "center", "location id"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Wat Sup Method", true, 120, "center", "computational method used to produce the water supply forecast"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Wat Sup Coord Agency", true, 180, "center", "coordinating agency for the water supply forecast"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Normal Upd Freq", true, 120, "center", "normal operations period of issuance"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Period Req", true, 120, "center", "period required to be forecast"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Wat Sup Criteria", true, 120, "center", "water supply criteria"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Customer Desc", true, 120, "center", "description of customers interested in the water supply forecast"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Implementation Date", true, 180, "center", "date service was first produced"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Web Date", true, 120, "center", "date graphical products first available on web"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Wat Sup Resp Agency", true, 180, "center", "agency responsible for the water supply forecast"));
	    jtm = new ComplexJTableManager(lhvmColumnDescriptorList, lhvmRowDataList);
	    jtm.setDisplayableColumns(columnsToBeDisplayed, false, true);
	    jtm.setPreferredSizeForJScrollPane(new Dimension(1010, 500));
	    jtm.addTableListener(new DetermTableViewListSelectListener());
	    tableScrollPane = jtm.getJScrollPane();
	    //jtm.setTableToolTipText("str");
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
	    editPanel2.setLayout(new GridLayout(2,4));
		//editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Fields with red text are non-editable"));
		editPanel2.setBackground(new Color(0,150,255));
		
	    JPanel editPanel3 = new JPanel();
	    editPanel3.setLayout(new GridLayout(2,4));
		//editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Fields with red text are non-editable"));
		editPanel3.setBackground(new Color(0,150,255));
		
		c.gridy++;
		
		JLabel locationIdLabel = new JLabel("Location ID");
		JLabel watsupMethodLabel = new JLabel("Wat Sup Method");
		JLabel watsupCoordAgencyLabel = new JLabel("Wat Sup Coord Agency");
		JLabel normalLabel = new JLabel("Normal Upd Freq");
		JLabel periodReqLabel = new JLabel("Period Req");
		JLabel watsupCritLabel = new JLabel("Wat Sup Criteria");
		JLabel customerDescLabel = new JLabel("Customer Desc");
		JLabel implDateLabel = new JLabel("Implementation Date");
		JLabel webDateLabel = new JLabel("Web Date");
		JLabel respAgencyLabel = new JLabel("Wat Sup Resp Agency");
		
		locationTextField = new JTextField();
		locationTextField.setBackground(new Color(255,175,75));
		locationTextField.setForeground(new Color(50,150,50));
		locationTextField.setFont(fnt);
		locationTextField.setCaretColor(Color.red);
		locationTextField.setSize(10,10);
		TextTypedActionListener textTypedActionListener = new TextTypedActionListener();
		locationTextField.addKeyListener(textTypedActionListener);
		implDateTextField = new DateTextField(0, lhvm, "Date Selector", 10, dataServicesGlobals.getMissingRepresentation());
		implDateTextField.setBackground(Color.yellow);
		implDateTextField.setForeground(new Color(50,150,50));
		implDateTextField.setFont(fnt);
		implDateTextField.setCaretColor(Color.red);
		implDateTextField.setSize(10,10);
		webDateTextField = new DateTextField(0, lhvm, "Date Selector", 10, dataServicesGlobals.getMissingRepresentation());
		webDateTextField.setBackground(Color.yellow);
		webDateTextField.setForeground(new Color(50,150,50));
		webDateTextField.setFont(fnt);
		webDateTextField.setCaretColor(Color.red);
		webDateTextField.setSize(10,10);		
		customerDescTextField = new JTextField();
		customerDescTextField.setBackground(Color.yellow);
		customerDescTextField.setForeground(new Color(50,150,50));
		customerDescTextField.setFont(fnt);
		customerDescTextField.setCaretColor(Color.red);
		customerDescTextField.setSize(10,10);
		customerDescTextField.addKeyListener(textTypedActionListener);
		
		lhvmRowDataList1 = lhvmDataManager.readDataFromRequiredPeriod();
		lhvmRowDataList2 = lhvmDataManager.readDataFromWatsupCriterion();
		lhvmRowDataList3 = lhvmDataManager.readDataFromWatsupCoordAgency();
		lhvmRowDataList4 = lhvmDataManager.readDataFromFrequencyUpdate();
		lhvmRowDataList5 = lhvmDataManager.readDataFromWatsupMethod();
		lhvmRowDataList6 = lhvmDataManager.readDataFromRespAgency();
		
		str1 = new String[lhvmRowDataList1.size()];
		str2 = new String[lhvmRowDataList2.size()];
		str3 = new String[lhvmRowDataList3.size()];
		str4 = new String[lhvmRowDataList4.size()];
		str5 = new String[lhvmRowDataList5.size()];
		str6 = new String[lhvmRowDataList6.size()];
		
		for(int i=0;i<lhvmRowDataList1.size();i++)
		{
			str1[i] = ((RequiredPeriodJTableRowData) lhvmRowDataList1.get(i)).getRequiredPeriod();
		}
		for(int i=0;i<lhvmRowDataList2.size();i++)
		{
			str2[i] = ((WatSupCriterionJTableRowData) lhvmRowDataList2.get(i)).getWatsupCriterion();
		}
		for(int i=0;i<lhvmRowDataList3.size();i++)
		{
			str3[i] = ((WatSupCoordAgencyJTableRowData) lhvmRowDataList3.get(i)).getWatsupCoordAgency();
		}
		for(int i=0;i<lhvmRowDataList4.size();i++)
		{
			str4[i] = ((FrequencyUpdateJTableRowData) lhvmRowDataList4.get(i)).getFrequencyUpdate();
		}
		for(int i=0;i<lhvmRowDataList5.size();i++)
		{
			str5[i] = ((WatSupMethodJTableRowData) lhvmRowDataList5.get(i)).getWatsupMethod();
		}
		for(int i=0;i<lhvmRowDataList6.size();i++)
		{
			str6[i] = ((RespAgencyJTableRowData) lhvmRowDataList6.get(i)).getRespAgency();
		}
		
		periodReqTextField = new JComboBox(str1);
		periodReqTextField.setBackground(new Color(255,175,75));
		periodReqTextField.setForeground(new Color(50,150,50));
		watsupCritTextField = new JComboBox(str2);
		watsupCritTextField.setBackground(new Color(255,175,75));
		watsupCritTextField.setForeground(new Color(50,150,50));
		watsupCoordAgencyTextField = new JComboBox(str3);
		watsupCoordAgencyTextField.setBackground(new Color(255,175,75));
		watsupCoordAgencyTextField.setForeground(new Color(50,150,50));
		normalTextField = new JComboBox(str4);
		normalTextField.setBackground(new Color(255,175,75));
		normalTextField.setForeground(new Color(50,150,50));
		watsupMethodTextField = new JComboBox(str5);
		watsupMethodTextField.setBackground(new Color(255,175,75));
		watsupMethodTextField.setForeground(new Color(50,150,50));
		respAgencyTextField = new JComboBox(str6);
		respAgencyTextField.setBackground(Color.yellow);
		respAgencyTextField.setForeground(new Color(50,150,50));
		
		editPanel.add(locationIdLabel);
		editPanel.add(watsupMethodLabel);
		editPanel.add(watsupCoordAgencyLabel);
		
		editPanel2.add(normalLabel);
		editPanel2.add(periodReqLabel);
		editPanel2.add(watsupCritLabel);
		
		editPanel3.add(respAgencyLabel);
		editPanel3.add(customerDescLabel);
		editPanel3.add(implDateLabel);
		editPanel3.add(webDateLabel);
		
		editPanel.add(locationTextField);
		editPanel.add(watsupMethodTextField);
		editPanel.add(watsupCoordAgencyTextField);
		
		editPanel2.add(normalTextField);		
		editPanel2.add(periodReqTextField);
		editPanel2.add(watsupCritTextField);
		
		editPanel3.add(respAgencyTextField);
		editPanel3.add(customerDescTextField);
		editPanel3.add(implDateTextField);
		editPanel3.add(webDateTextField);
		
		lhvm.getContentPane().add(editPanel,c);
		c.gridy++;
		c.gridy++;
		lhvm.getContentPane().add(editPanel2,c);
		c.gridy++;
		c.gridy++;
		lhvm.getContentPane().add(editPanel3,c);
	    
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
	    lhvm.getContentPane ().add(emptyLabel4, c);
	    c.gridx++;
	    lhvm.getContentPane ().add(emptyLabel3, c);
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
		if(selectedFcstPtWatsupJTableRowData != null)
		{
			locationTextField.setText(selectedFcstPtWatsupJTableRowData.getDataValue("Location ID"));
			implDateTextField.setText(selectedFcstPtWatsupJTableRowData.getDataValue("Implementation Date"));
			webDateTextField.setText(selectedFcstPtWatsupJTableRowData.getDataValue("Web Date"));
			customerDescTextField.setText(selectedFcstPtWatsupJTableRowData.getDataValue("Customer Desc"));
			
			periodReqTextField.setSelectedItem(selectedFcstPtWatsupJTableRowData.getDataValue("Period Req"));
			watsupCritTextField.setSelectedItem(selectedFcstPtWatsupJTableRowData.getDataValue("Wat Sup Criteria"));
			watsupCoordAgencyTextField.setSelectedItem(selectedFcstPtWatsupJTableRowData.getDataValue("Wat Sup Coord Agency"));
			normalTextField.setSelectedItem(selectedFcstPtWatsupJTableRowData.getDataValue("Normal Upd Freq"));
			watsupMethodTextField.setSelectedItem(selectedFcstPtWatsupJTableRowData.getDataValue("Wat Sup Method"));
			respAgencyTextField.setSelectedItem(selectedFcstPtWatsupJTableRowData.getDataValue("Wat Sup Resp Agency"));
			
			selectedFcstPtWatsupJTableRowData = null;
			
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
		implDateTextField.setText("yyyy-mm-dd");
		webDateTextField.setText("yyyy-mm-dd");
		customerDescTextField.setText("");
	} 
	
	public void buttonsActivateWhenKeysTyped()
    {
    	if(customerDescTextField.getText().length() > 0)
    	{
    		activateClearButton();
    	}
    	else
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
		if(selectedFcstPtWatsupJTableRowsDataList != null && selectedFcstPtWatsupJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
		else if(selectedFcstPtWatsupJTableRowsDataList != null && selectedFcstPtWatsupJTableRowsDataList.size() == 1)
		{
			selectedFcstPtWatsupJTableRowData = (FcstPtWatSupJTableRowData) selectedFcstPtWatsupJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
			activateSaveButton();
			activateClearButton();
		}
		else if(selectedFcstPtWatsupJTableRowsDataList != null && selectedFcstPtWatsupJTableRowsDataList.size() > 1)
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
		if(selectedFcstPtWatsupJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
		else if(selectedFcstPtWatsupJTableRowsDataList.size() == 1)
		{
			selectedFcstPtWatsupJTableRowData = (FcstPtWatSupJTableRowData) selectedFcstPtWatsupJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
			activateSaveButton();
			activateClearButton();
		}
		else if(selectedFcstPtWatsupJTableRowsDataList.size() > 1)
		{
			activateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
	}
    
	private void getSelectedRows()
	{
		selectedFcstPtWatsupJTableRowsDataList = (ArrayList) jtm.getSelectedRowsData();
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
        	int numRowsTemp = -1;
        	String currentLidFromRowsInsideLoop = null;
        	String currentwatsupMethodFromRowsInsideLoop = null;
        	String currentwatsupCoordAgencyFromRowsInsideLoop = null;
        	String currentNormalFromRowsInsideLoop = null;
        	String currentPeriodReqFromRowsInsideLoop = null;
        	String currentWatsupCritFromRowsInsideLoop = null;
        	int i, null_or_not = 0;
        	rowIndex[0] = -1;
        	FcstPtWatSupRecord record = new FcstPtWatSupRecord();
			record.setLid(locationTextField.getText().toUpperCase());
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
			if(customerDescTextField.getText().length() > 0 && !customerDescTextField.getText().equals(lhvmDataManager.getMissingRepresentation()))
			{
				record.setCustomer_desc(customerDescTextField.getText());
			}
			else
			{
				null_or_not = 1;
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
			record.setFrequpd_normal(str4[normalTextField.getSelectedIndex()]);
			record.setWatsup_method(str5[watsupMethodTextField.getSelectedIndex()]);
			record.setWatsup_coord_agency(str3[watsupCoordAgencyTextField.getSelectedIndex()]);
			record.setWatsup_crit(str2[watsupCritTextField.getSelectedIndex()]);
			record.setPeriod_req(str1[periodReqTextField.getSelectedIndex()]);
			record.setWatsup_resp_agency(str6[respAgencyTextField.getSelectedIndex()]);
//			lhvmLogger.log(" Delete record"+ record.toString());
			LhvmDataManager.databaseMessage = null;
			
			lhvmRowDataList = lhvmDataManager.readDataFromPtWatsup();
			numRows = lhvmRowDataList.size();
			
			int success = lhvmDataManager.insertOrUpdateWatsupTable(record);
			
			//lhvmRowDataList = lhvmDataManager.readDataFromPtWatsup();
			numRowsTemp = lhvmRowDataList.size();
			
			if(success != -1)
			{
				getSelectedRows();
				if(selectedFcstPtWatsupJTableRowsDataList.size() == 1 && ((numRows+1) == numRowsTemp))
				{
					record = new FcstPtWatSupRecord();
	    			String lid = ((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(0)).getLid();
	    			record.setLid(lid);
	    			record.setWatsup_method(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(0)).getWatsupMethod());
	    			record.setWatsup_coord_agency(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(0)).getWatsupCoordAgency());
	    			record.setFrequpd_normal(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(0)).getNormal());
	    			record.setPeriod_req(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(0)).getPeriodReq());
	    			record.setWatsup_crit(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(0)).getWatsupCrit());
	    			lhvmDataManager.deleteFromWatsupTable(record);
	    			
	    			appendToConsoleMessageReplace(locationTextField.getText());
				}
				else if(selectedFcstPtWatsupJTableRowsDataList.size() == 1 && numRows == numRowsTemp)
				{
					appendToConsoleMessageReplace(locationTextField.getText());
				}
				else if(selectedFcstPtWatsupJTableRowsDataList.size() == 0 && ((numRows+1) == numRowsTemp))
				{
					appendToConsoleMessageInsert(locationTextField.getText());
				}
			}
			else
			{
				JOptionPane.showMessageDialog(null, "Oops: Problem inserting, please check console");
				setDatabaseMessage();
			}
			lhvmRowDataList = lhvmDataManager.readDataFromPtWatsup();
			jtm.setChangedAllRowDataList(lhvmRowDataList);
			numRows = lhvmRowDataList.size();
			numRowsLabel.setText(numRows + " Rows");
			jtm.refreshDisplay();
			
			for(i=0;i<lhvmRowDataList.size();i++)
    		{
				FcstPtWatSupJTableRowData rowData = (FcstPtWatSupJTableRowData) lhvmRowDataList.get(i);
				currentLidFromRowsInsideLoop = rowData.getLid();
				currentwatsupMethodFromRowsInsideLoop = rowData.getWatsupMethod();
				currentwatsupCoordAgencyFromRowsInsideLoop = rowData.getWatsupCoordAgency();
				currentNormalFromRowsInsideLoop = rowData.getNormal();
				currentPeriodReqFromRowsInsideLoop = rowData.getPeriodReq();
				currentWatsupCritFromRowsInsideLoop = rowData.getWatsupCrit();
    			if(locationTextField.getText().equalsIgnoreCase(currentLidFromRowsInsideLoop) &&
    			str5[watsupMethodTextField.getSelectedIndex()].equalsIgnoreCase(currentwatsupMethodFromRowsInsideLoop) &&
    			str3[watsupCoordAgencyTextField.getSelectedIndex()].equalsIgnoreCase(currentwatsupCoordAgencyFromRowsInsideLoop) &&
    			str4[normalTextField.getSelectedIndex()].equalsIgnoreCase(currentNormalFromRowsInsideLoop) &&
    			str1[periodReqTextField.getSelectedIndex()].equalsIgnoreCase(currentPeriodReqFromRowsInsideLoop) &&
    			str2[watsupCritTextField.getSelectedIndex()].equalsIgnoreCase(currentWatsupCritFromRowsInsideLoop))
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
	
	private class RefreshActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		lhvmRowDataList = lhvmDataManager.readDataFromPtWatsup();
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
    
    private class DeleteActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		int i, size;
    		origNumRows = lhvmRowDataList.size();
    		size = selectedFcstPtWatsupJTableRowsDataList.size();
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
	    			FcstPtWatSupRecord record = new FcstPtWatSupRecord();
	    			String lid = ((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(i)).getLid();
	    			record.setLid(lid);
	    			record.setWatsup_method(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(i)).getWatsupMethod());
	    			record.setWatsup_coord_agency(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(i)).getWatsupCoordAgency());
	    			record.setFrequpd_normal(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(i)).getNormal());
	    			record.setPeriod_req(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(i)).getPeriodReq());
	    			record.setWatsup_crit(((FcstPtWatSupJTableRowData)selectedFcstPtWatsupJTableRowsDataList.get(i)).getWatsupCrit());
	    			//lhvmLogger.log(" Delete record"+ record.toString());
	    			LhvmDataManager.databaseMessage = null;
	    			int success = lhvmDataManager.deleteFromWatsupTable(record);
	    			if(success != -1)
	    			{
	    				appendToConsoleMessage(lid);
	    			}
	    			else
	    			{
	    				JOptionPane.showMessageDialog(null, "Oops: Some or all of the rows could not be deleted, please check console");
	    				setDatabaseMessage();
	    			}
	    		}
    		}
    		
    		lhvmRowDataList = lhvmDataManager.readDataFromPtWatsup();
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
    
    private class DetermTableViewListSelectListener implements ListSelectionListener
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
    				currentLidFromRowsInsideLoop = ((FcstPtWatSupJTableRowData) lhvmRowDataList.get(i)).getLid();
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
    		selectedFcstPtWatsupJTableRowsDataList = searchList;
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
