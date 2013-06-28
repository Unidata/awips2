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

public class FcstDataEnsembleWindow implements Subject
{
	private DataServicesGlobals dataServicesGlobals = null;
	private java.util.List lhvmRowDataList = null;
	private LhvmDataManager lhvmDataManager = null;
	private LhvmLogger lhvmLogger = null;
	private JTableManager jtm = null;
	private JFrame lhvm = null;
	
	private FcstPtEnsembleJTableRowData selectedFcstPtEnsembleJTableRowData = null;
	private ArrayList selectedFcstPtEnsembleJTableRowsDataList = null;
	
	private JTextField locationTextField = null;
	private JComboBox snowMethodTextField = null;
	private JComboBox postProcessingTextField = null;
	private JComboBox hydrolTextField = null;
	private JComboBox hydraulTextField = null;	
	private JComboBox flowTypeTextField = null;	
	private JComboBox floodTextField = null;
	private JComboBox droughtTextField = null;	
	private JComboBox horizonTextField = null;	
	private JComboBox fcstTypeTextField = null;
	private JComboBox normalTextField = null;
	private JTextField varUsageTextField = null;
	private JTextField numMonClimTextField = null;
	private JTextField numDayHydTextField = null;
	private JTextField upstreamSegTextField = null;
	private JComboBox reservoirModelTextField = null;
	private JTextField consumptiveUseTextField = null;
	private JTextField channelLossTextField = null;
	private JTextField numElevZonesTextField = null;
	
	private DateTextField implDateTextField = null;	
	private DateTextField webDateTextField = null;
	private DateTextField externalDateTextField = null;
	
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
	private java.util.List lhvmRowDataList3 = null;
	private java.util.List lhvmRowDataList4 = null;
	private java.util.List lhvmRowDataList5 = null;
	private java.util.List lhvmRowDataList6 = null;
	private java.util.List lhvmRowDataList7 = null;
	private java.util.List lhvmRowDataList9 = null;
	private java.util.List lhvmRowDataList10 = null;
	
	private String str1[] = null;
	private String str2[] = null;
	private String str3[] = null;
	private String str4[] = null;
	private String str5[] = null;
	private String str6[] = null;
	private String str7[] = null;
	private String str9[] = null;
	private String str10[] = null;
	
	private JButton filterOptionsButton;
	
	private void setDatabaseMessage()
	{
	    String tempStr = null;
	    if(LhvmDataManager.displayedMessageEnsembleWindow != null)
	    {
	    	tempStr = new String(LhvmDataManager.displayedMessageEnsembleWindow);
	    }
	    LhvmDataManager.displayedMessageEnsembleWindow = new StringBuffer();
	    if(tempStr != null)
	    {
	    	LhvmDataManager.displayedMessageEnsembleWindow.append(tempStr);
	    }
	    LhvmDataManager.displayedMessageEnsembleWindow.append("\n--------------\n");
	    LhvmDataManager.displayedMessageEnsembleWindow.append(LhvmDataManager.databaseMessage);
	    //consoleTextArea.insert(new String(LhvmDataManager.displayedMessageEnsembleWindow), 0);
	    consoleTextArea.append(new String(LhvmDataManager.displayedMessageEnsembleWindow));
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
	
	public FcstDataEnsembleWindow(String jdbcUrl)
	{		 
		dataServicesGlobals = DataServicesGlobals.getSingleInstanceOfDataServicesGlobals();
		this.observers = DataServicesGlobals.dataEnsembleObservers;
		
		lhvm = new JFrame("Ensemble Forecast");
	    lhvm.setSize(1000, 500);
	    lhvm.setLocation(20,50);
	    
	    //Fix the dimensions of this window i.e. dis-allow window resizing
	    Dimension d = new Dimension(1210, 934);
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

	    String[] columnsToBeDisplayed = {"Location ID", "Snow Method", "Hydrologic Method", "Reservoir Model", "Upstream Segment", "Routing Method", "Flow Type", "Fcst Type", "Normal Upd Freq", "Flood Upd Freq", "Drought Upd Freq", "Forecast Period", "Num Mon Clim Fcst", "Num Day Hydromet Fcst", "Num Elev Zones", "Consumptive Use", "Channel Loss", "Post Processor", "Implementation Date", "External Date", "Web Date", "VAR Usage"};

	    lhvmLogger = new LhvmLogger();
	    lhvmDataManager = LhvmDataManager.getSingleInstanceOfDataManager(jdbcUrl, lhvmLogger,"-");
	    java.util.List lhvmColumnDescriptorList = new ArrayList();
	    lhvmRowDataList = lhvmDataManager.readDataFromPtEnsemble();
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Location ID", true, 90, "center", "location id"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Upstream Segment", true, 120, "center", "location id of upstream point may be forecast or data point if point is a headwater point, then this value is set to the value of the lid column"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Hydrologic Method", true, 120, "center", "hydrologic computation method for the segment"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Routing Method", true, 120, "center", "routing computation method for the segment"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Snow Method", true, 90, "center", "snow computation method for the segment"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Reservoir Model", true, 120, "center", "reservoir model for the segment"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Consumptive Use", true, 150, "center", "indicates consumptive use model is used for the segment (Y or N)"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Channel Loss", true, 90, "center", "indicates channel loss model is used for the segment (Y or N)"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Num Elev Zones", true, 120, "center", "number of elevation zones in mountainous area may be 1,2 or 3 else should be set to 1"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Flow Type", true, 120, "center", "flow type"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Fcst Type", true, 120, "center", "forecast time horizon type"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Implementation Date", true, 150, "center", "date service was first produced"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Normal Upd Freq", true, 150, "center", "normal operations period of issuance"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Flood Upd Freq", true, 120, "center", "flood operations period of issuance"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Drought Upd Freq", true, 120, "center", "drought operations period of issuance"));	
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("VAR Usage", true, 120, "center", ""));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Forecast Period", true, 120, "center", ""));	    
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Num Mon Clim Fcst", true, 150, "center", "number of months climotological forecasts are used in streamflow forecast 0  N/A"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Num Day Hydromet Fcst", true, 180, "center", "number of days hydrometeorological forecasts are used in streamflow forecast 0  N/A"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Post Processor", true, 120, "center", "post processing type used to create the forecast"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Web Date", true, 90, "center", "date graphical products first available on web"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("External Date", true, 90, "center", "date product first available the external customers"));
	    jtm = new ComplexJTableManager(lhvmColumnDescriptorList, lhvmRowDataList);
	    jtm.setDisplayableColumns(columnsToBeDisplayed, false, true);
	    jtm.setPreferredSizeForJScrollPane(new Dimension(1172, 500));
	    jtm.addTableListener(new EnsembleTableViewListSelectListener());
	    tableScrollPane = jtm.getJScrollPane();
	    //tableScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
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
	    
	    filterOptionsButton = new JButton("Show reservoirs");
	    filterOptionsButton.addActionListener(new ReservoirsActionListener());
	    c.gridx++;
	    lhvm.getContentPane ().add(filterOptionsButton, c);
	    
	    c.gridy++;  
	    lhvm.getContentPane().add(emptyLabel9, c);
	    c.gridwidth = 0;
	    c.gridx = 0;
	    c.gridy++;
	    lhvm.getContentPane().add(tableScrollPanePanel,c);
	 
	    //int[] selectedRowIndices = jtm.getSelectedRowIndices();
	    
		
	    JPanel editPanel = new JPanel();
	    editPanel.setLayout(new GridLayout(2,8));
		editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor"));
		editPanel.setBackground(new Color(0,150,255));
		
	    JPanel editPanel2 = new JPanel();
	    editPanel2.setLayout(new GridLayout(2,8));
		//editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Fields with red text are non-editable"));
		editPanel2.setBackground(new Color(0,150,255));
		
	    JPanel editPanel3 = new JPanel();
	    editPanel3.setLayout(new GridLayout(2,8));
		//editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Fields with red text are non-editable"));
		editPanel3.setBackground(new Color(0,150,255));
		
	    JPanel editPanel4 = new JPanel();
	    editPanel4.setLayout(new GridLayout(2,8));
		//editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Fields with red text are non-editable"));
		editPanel4.setBackground(new Color(0,150,255));
		
		c.gridy++;
		
		JLabel locationIdLabel = new JLabel("Location ID");
		JLabel hydrolLabel = new JLabel("Hydrologic Method");
		JLabel flowTypeLabel = new JLabel("Flow Type");
		JLabel floodLabel = new JLabel("Flood Upd Freq");
		JLabel droughtLabel = new JLabel("Drought Upd Freq");
		JLabel implDateLabel = new JLabel("Implementation Date");
		JLabel horizonLabel = new JLabel("Forecast Period");
		JLabel fcstTypeLabel = new JLabel("Fcst Type");
		JLabel normalLabel = new JLabel("Normal Upd Freq");
		JLabel numMonClimLabel = new JLabel("Num Mon Clim Fcst");
		JLabel numDayHydLabel = new JLabel("Num Day Hydromet Fcst");
		JLabel hydraulLabel = new JLabel("Routing Method");
		JLabel webDateLabel = new JLabel("Web Date");
		JLabel externalDateLabel = new JLabel("External Date");
		JLabel snowMethodLabel = new JLabel("Snow Method");
		JLabel postProcessingLabel = new JLabel("Post Processor");
		JLabel upstreamLabel = new JLabel("Upstream Segment");
		JLabel reservoirLabel = new JLabel("Reservoir Model");
		JLabel consumptiveUseLabel = new JLabel("Consumptive Use");
		JLabel channelLossLabel = new JLabel("Channel Loss");
		JLabel numElevZonesLabel = new JLabel("Num Elev Zones");
		JLabel varUsageLabel = new JLabel("VAR Usage");
		
		locationTextField = new JTextField();
		locationTextField.setBackground(new Color(255,175,75));
		locationTextField.setForeground(new Color(50,150,50));
		locationTextField.setFont(fnt);
		locationTextField.setCaretColor(Color.red);
		locationTextField.setSize(10,10);
		TextTypedActionListener textTypedActionListener = new TextTypedActionListener();
		TextFieldClickListener textFieldClickListener = new TextFieldClickListener();
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
		externalDateTextField = new DateTextField(0, lhvm, "Date Selector", 10, dataServicesGlobals.getMissingRepresentation());
		externalDateTextField.setBackground(Color.yellow);
		externalDateTextField.setForeground(new Color(50,150,50));
		externalDateTextField.setFont(fnt);
		externalDateTextField.setCaretColor(Color.red);
		externalDateTextField.setSize(10,10);	
		numMonClimTextField = new JTextField();
		numMonClimTextField.setBackground(Color.yellow);
		numMonClimTextField.setForeground(new Color(50,150,50));
		numMonClimTextField.setFont(fnt);
		numMonClimTextField.setCaretColor(Color.red);
		numMonClimTextField.setSize(10,10);
		numMonClimTextField.addKeyListener(textTypedActionListener);		
		numDayHydTextField = new JTextField();
		numDayHydTextField.setBackground(Color.yellow);
		numDayHydTextField.setForeground(new Color(50,150,50));
		numDayHydTextField.setFont(fnt);
		numDayHydTextField.setCaretColor(Color.red);
		numDayHydTextField.setSize(10,10);
		numDayHydTextField.addKeyListener(textTypedActionListener);
		consumptiveUseTextField = new JTextField("N");
		consumptiveUseTextField.setBackground(Color.yellow);
		consumptiveUseTextField.setForeground(new Color(50,150,50));
		consumptiveUseTextField.setFont(fnt);
		consumptiveUseTextField.setCaretColor(Color.red);
		consumptiveUseTextField.setSize(10,10);
		consumptiveUseTextField.setEditable(false);
		consumptiveUseTextField.addMouseListener(textFieldClickListener);
		channelLossTextField = new JTextField("N");
		channelLossTextField.setBackground(Color.yellow);
		channelLossTextField.setForeground(new Color(50,150,50));
		channelLossTextField.setFont(fnt);
		channelLossTextField.setCaretColor(Color.red);
		channelLossTextField.setSize(10,10);
		channelLossTextField.setEditable(false);
		channelLossTextField.addMouseListener(textFieldClickListener);
		numElevZonesTextField = new JTextField("1");
		numElevZonesTextField.setBackground(Color.yellow);
		numElevZonesTextField.setForeground(new Color(50,150,50));
		numElevZonesTextField.setFont(fnt);
		numElevZonesTextField.setCaretColor(Color.red);
		numElevZonesTextField.setSize(10,10);
		numElevZonesTextField.addKeyListener(textTypedActionListener);
		upstreamSegTextField = new JTextField("XXXXX");
		upstreamSegTextField.setBackground(new Color(255,175,75));
		upstreamSegTextField.setForeground(new Color(50,150,50));
		upstreamSegTextField.setFont(fnt);
		upstreamSegTextField.setCaretColor(Color.red);
		upstreamSegTextField.setSize(10,10);
		upstreamSegTextField.addKeyListener(textTypedActionListener);
		varUsageTextField = new JTextField("N");
		varUsageTextField.setBackground(Color.yellow);
		varUsageTextField.setForeground(new Color(50,150,50));
		varUsageTextField.setFont(fnt);
		varUsageTextField.setCaretColor(Color.red);
		varUsageTextField.setSize(10,10);
		varUsageTextField.setEditable(false);
		varUsageTextField.addMouseListener(textFieldClickListener);
		
		lhvmRowDataList1 = lhvmDataManager.readDataFromFcstType();
		lhvmRowDataList2 = lhvmDataManager.readDataFromFlowType();
		lhvmRowDataList3 = lhvmDataManager.readDataFromSnowMethod();
		lhvmRowDataList4 = lhvmDataManager.readDataFromHydrologicMethod();
		lhvmRowDataList5 = lhvmDataManager.readDataFromRoutingMethod();
		lhvmRowDataList6 = lhvmDataManager.readDataFromFrequencyUpdate();
		lhvmRowDataList7 = lhvmDataManager.readDataFromPostProcessing();
		lhvmRowDataList9 = lhvmDataManager.readDataFromReservoirModel();
		lhvmRowDataList10 = lhvmDataManager.readDataFromHorizon();
		
		str1 = new String[lhvmRowDataList1.size()];
		str2 = new String[lhvmRowDataList2.size()];
		str3 = new String[lhvmRowDataList3.size()];
		str4 = new String[lhvmRowDataList4.size()];
		str5 = new String[lhvmRowDataList5.size()];
		str6 = new String[lhvmRowDataList6.size()];
		str7 = new String[lhvmRowDataList7.size()];
		str9 = new String[lhvmRowDataList9.size()];
		str10 = new String[lhvmRowDataList10.size()];
		
		for(int i=0;i<lhvmRowDataList1.size();i++)
		{
			str1[i] = ((FcstTypeJTableRowData) lhvmRowDataList1.get(i)).getFcstType();
		}
		for(int i=0;i<lhvmRowDataList2.size();i++)
		{
			str2[i] = ((FlowTypeJTableRowData) lhvmRowDataList2.get(i)).getFlowType();
		}
		for(int i=0;i<lhvmRowDataList3.size();i++)
		{
			str3[i] = ((SnowMethodJTableRowData) lhvmRowDataList3.get(i)).getSnowMethod();
		}
		for(int i=0;i<lhvmRowDataList4.size();i++)
		{
			str4[i] = ((HydrologicMethodJTableRowData) lhvmRowDataList4.get(i)).getHydrologicMethod();
		}
		for(int i=0;i<lhvmRowDataList5.size();i++)
		{
			str5[i] = ((RoutingMethodJTableRowData) lhvmRowDataList5.get(i)).getRoutingMethod();
		}
		for(int i=0;i<lhvmRowDataList6.size();i++)
		{
			str6[i] = ((FrequencyUpdateJTableRowData) lhvmRowDataList6.get(i)).getFrequencyUpdate();
		}
		for(int i=0;i<lhvmRowDataList7.size();i++)
		{
			str7[i] = ((PostProcessingJTableRowData) lhvmRowDataList7.get(i)).getPostProcessing();
		}
		for(int i=0;i<lhvmRowDataList9.size();i++)
		{
			str9[i] = ((ReservoirModelJTableRowData) lhvmRowDataList9.get(i)).getReservoirModel();
		}
		for(int i=0;i<lhvmRowDataList10.size();i++)
		{
			str10[i] = ((FcstHorizonJTableRowData) lhvmRowDataList10.get(i)).getFcstHorizon();
		}
		
		fcstTypeTextField = new JComboBox(str1);
		fcstTypeTextField.setBackground(new Color(255,175,75));
		fcstTypeTextField.setForeground(new Color(50,150,50));
		flowTypeTextField = new JComboBox(str2);
		flowTypeTextField.setBackground(new Color(255,175,75));
		flowTypeTextField.setForeground(new Color(50,150,50));
		snowMethodTextField = new JComboBox(str3);
		snowMethodTextField.setBackground(new Color(255,175,75));
		snowMethodTextField.setForeground(new Color(50,150,50));
		hydrolTextField = new JComboBox(str4);
		hydrolTextField.setBackground(new Color(255,175,75));
		hydrolTextField.setForeground(new Color(50,150,50));
		hydraulTextField = new JComboBox(str5);
		hydraulTextField.setBackground(new Color(255,175,75));
		hydraulTextField.setForeground(new Color(50,150,50));
		normalTextField = new JComboBox(str6);
		normalTextField.setBackground(Color.yellow);
		normalTextField.setForeground(new Color(50,150,50));
		floodTextField = new JComboBox(str6);
		floodTextField.setBackground(Color.yellow);
		floodTextField.setForeground(new Color(50,150,50));
		droughtTextField = new JComboBox(str6);
		droughtTextField.setBackground(Color.yellow);
		droughtTextField.setForeground(new Color(50,150,50));
		postProcessingTextField = new JComboBox(str7);
		postProcessingTextField.setBackground(Color.yellow);
		postProcessingTextField.setForeground(new Color(50,150,50));
		reservoirModelTextField = new JComboBox(str9);
		reservoirModelTextField.setBackground(new Color(255,175,75));
		reservoirModelTextField.setForeground(new Color(50,150,50));		
		horizonTextField = new JComboBox(str10);
		horizonTextField.setBackground(Color.yellow);
		horizonTextField.setForeground(new Color(50,150,50));
		
		editPanel.add(locationIdLabel);
		editPanel.add(snowMethodLabel);
		editPanel.add(hydrolLabel);
		editPanel.add(reservoirLabel);
		editPanel.add(upstreamLabel);
		
		editPanel2.add(hydraulLabel);		
		editPanel2.add(flowTypeLabel);	
		editPanel2.add(fcstTypeLabel);
		editPanel2.add(normalLabel);
		editPanel2.add(floodLabel);
		
		editPanel3.add(droughtLabel);
		editPanel3.add(horizonLabel);
		editPanel3.add(numMonClimLabel);
		editPanel3.add(numDayHydLabel);
		editPanel3.add(numElevZonesLabel);
		editPanel3.add(consumptiveUseLabel);
		
		editPanel4.add(channelLossLabel);
		editPanel4.add(postProcessingLabel);				
		editPanel4.add(implDateLabel);
		editPanel4.add(externalDateLabel);
		editPanel4.add(webDateLabel);
		editPanel4.add(varUsageLabel);
		
		editPanel.add(locationTextField);
		editPanel.add(snowMethodTextField);
		editPanel.add(hydrolTextField);
		editPanel.add(reservoirModelTextField);
		editPanel.add(upstreamSegTextField);
				
		editPanel2.add(hydraulTextField);
		editPanel2.add(flowTypeTextField);		
		editPanel2.add(fcstTypeTextField);
		editPanel2.add(normalTextField);
		editPanel2.add(floodTextField);		
		
		editPanel3.add(droughtTextField);
		editPanel3.add(horizonTextField);
		editPanel3.add(numMonClimTextField);
		editPanel3.add(numDayHydTextField);
		editPanel3.add(numElevZonesTextField);
		editPanel3.add(consumptiveUseTextField);
		
		editPanel4.add(channelLossTextField);
		editPanel4.add(postProcessingTextField);		
		editPanel4.add(implDateTextField);
		editPanel4.add(externalDateTextField);
		editPanel4.add(webDateTextField);
		editPanel4.add(varUsageTextField);
		
		lhvm.getContentPane().add(editPanel,c);
		c.gridy++;
		c.gridy++;
		lhvm.getContentPane().add(editPanel2,c);
		c.gridy++;
		c.gridy++;
		lhvm.getContentPane().add(editPanel3,c);
		c.gridy++;
		c.gridy++;
		lhvm.getContentPane().add(editPanel4,c);
	    
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
		if(selectedFcstPtEnsembleJTableRowData != null)
		{
			locationTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("Location ID"));
			implDateTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("Implementation Date"));
			webDateTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("Web Date"));
			externalDateTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("External Date"));	
			numMonClimTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("Num Mon Clim Fcst"));
			numDayHydTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("Num Day Hydromet Fcst"));
			consumptiveUseTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("Consumptive Use"));
			channelLossTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("Channel Loss"));
			numElevZonesTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("Num Elev Zones"));
			upstreamSegTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("Upstream Segment"));
			varUsageTextField.setText(selectedFcstPtEnsembleJTableRowData.getDataValue("VAR Usage"));
			
			hydrolTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Hydrologic Method"));
			hydraulTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Routing Method"));
			flowTypeTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Flow Type"));
			fcstTypeTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Fcst Type"));
			snowMethodTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Snow Method"));
			normalTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Normal Upd Freq"));
			floodTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Flood Upd Freq"));
			horizonTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Forecast Period"));
			droughtTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Drought Upd Freq"));
			postProcessingTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Post Processor"));
			reservoirModelTextField.setSelectedItem(selectedFcstPtEnsembleJTableRowData.getDataValue("Reservoir Model"));
			
			selectedFcstPtEnsembleJTableRowData = null;
			
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
		externalDateTextField.setText("yyyy-mm-dd");
		numMonClimTextField.setText("");
		numDayHydTextField.setText("");
		consumptiveUseTextField.setText("N");
		channelLossTextField.setText("N");
		varUsageTextField.setText("N");
		numElevZonesTextField.setText("1");
		upstreamSegTextField.setText("XXXXX");
	} 
	
	public void buttonsActivateWhenKeysTyped()
    {
    	int either = 0;
		
		if((upstreamSegTextField.getText().length() >= 0 && !upstreamSegTextField.getText().equals("XXXXX")) ||
    	(numElevZonesTextField.getText().length() >= 0 && !numElevZonesTextField.getText().equals("1")))
    	{
    		activateClearButton();
    		either++;
    	}
		if(numMonClimTextField.getText().length() > 0 || numDayHydTextField.getText().length() > 0)
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
		if(selectedFcstPtEnsembleJTableRowsDataList != null && selectedFcstPtEnsembleJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
		else if(selectedFcstPtEnsembleJTableRowsDataList != null && selectedFcstPtEnsembleJTableRowsDataList.size() == 1)
		{
			selectedFcstPtEnsembleJTableRowData = (FcstPtEnsembleJTableRowData) selectedFcstPtEnsembleJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
			activateSaveButton();
			activateClearButton();
		}
		else if(selectedFcstPtEnsembleJTableRowsDataList != null && selectedFcstPtEnsembleJTableRowsDataList.size() > 1)
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
		if(selectedFcstPtEnsembleJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
		else if(selectedFcstPtEnsembleJTableRowsDataList.size() == 1)
		{
			selectedFcstPtEnsembleJTableRowData = (FcstPtEnsembleJTableRowData) selectedFcstPtEnsembleJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
			activateSaveButton();
			activateClearButton();
		}
		else if(selectedFcstPtEnsembleJTableRowsDataList.size() > 1)
		{
			activateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
	}
    
	private void getSelectedRows()
	{
		selectedFcstPtEnsembleJTableRowsDataList = (ArrayList) jtm.getSelectedRowsData();
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
        	String currentHydrolMethodFromRowsInsideLoop = null;
        	String currentHydraulMethodFromRowsInsideLoop = null;
        	String currentSnowMethodFromRowsInsideLoop = null;
        	String currentFcstTypeFromRowsInsideLoop = null;
        	String currentFlowTypeFromRowsInsideLoop = null;
        	String currentUpstreamSegFromRowsInsideLoop = null;
        	String currentReservoirModelFromRowsInsideLoop = null;
        	int i, null_or_not = 0;
        	rowIndex[0] = -1;
        	FcstPtESPRecord record = new FcstPtESPRecord();
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
			if(!externalDateTextField.getText().equalsIgnoreCase("yyyy-mm-dd") && !externalDateTextField.getText().equalsIgnoreCase(dataServicesGlobals.getMissingRepresentation()))
			{
				record.setExternal_date(DbTimeHelper.getLongTimeFromDateString(externalDateTextField.getText()));
			}
			else
			{
				null_or_not = 1;
				record.setExternal_date(Long.MIN_VALUE);
			}
			record.setFrequpd_normal(str6[normalTextField.getSelectedIndex()]);
			record.setFrequpd_flood(str6[floodTextField.getSelectedIndex()]);
			record.setFrequpd_drought(str6[droughtTextField.getSelectedIndex()]);
			record.setPost_processor(str7[postProcessingTextField.getSelectedIndex()]);
			record.setUpstream_seg(upstreamSegTextField.getText().toUpperCase());
			record.setReservoir_model(str9[reservoirModelTextField.getSelectedIndex()]);
			record.setFcst_horizon(str10[horizonTextField.getSelectedIndex()]);
			if(consumptiveUseTextField.getText().length() > 0 && !consumptiveUseTextField.getText().equals(lhvmDataManager.getMissingRepresentation()))
			{
				record.setConsumptive_use(consumptiveUseTextField.getText());
			}
			else
			{
				record.setConsumptive_use("N");
			}
			if(varUsageTextField.getText().length() > 0 && !varUsageTextField.getText().equals(lhvmDataManager.getMissingRepresentation()))
			{
				record.setVar_usage(varUsageTextField.getText());
			}
			else
			{
				record.setVar_usage("N");
			}
			if(channelLossTextField.getText().length() > 0 && !channelLossTextField.getText().equals(lhvmDataManager.getMissingRepresentation()))
			{
				record.setChannel_loss(channelLossTextField.getText());
			}
			else
			{
				record.setChannel_loss("N");
			}	
			try
			{
				if(numElevZonesTextField.getText().length() > 0)
				{
					if(Short.valueOf(numElevZonesTextField.getText().trim()) > 0)
					{
						record.setNum_elev_zones(Short.valueOf(numElevZonesTextField.getText().trim()));
					}
					else
					{
						JOptionPane.showMessageDialog(null, "'Num Elev Zones' should be '1' or greater. Aborting insert.");
						numElevZonesTextField.setText("1");
						return;
					}
				}
				else
				{
					record.setNum_elev_zones(Short.MIN_VALUE);
					null_or_not = 1;
				}
			}
			catch(NumberFormatException ne)
			{
				JOptionPane.showMessageDialog(null, "Not a valid value in the Num Elev Zones text box; It must be a number. Aborting insert.");
				numElevZonesTextField.setText("1");
				return;
			}
			try
			{
				if(numMonClimTextField.getText().length() > 0 && !numMonClimTextField.getText().equals(lhvmDataManager.getMissingRepresentation()))
				{
					record.setNummonclim(Short.valueOf(numMonClimTextField.getText().trim()));
				}
				else
				{
					null_or_not = 1;
				}
			}
			catch(NumberFormatException ne)
			{
				JOptionPane.showMessageDialog(null, "Not a valid value in the Num Mon Clim text box; It must be a number. Aborting insert.");
				return;
			}
			try
			{
				if(numDayHydTextField.getText().length() > 0 && !numDayHydTextField.getText().equals(lhvmDataManager.getMissingRepresentation()))
				{
					record.setNumdayhyd(Short.valueOf(numDayHydTextField.getText().trim()));
				}
				else
				{
					null_or_not = 1;
				}
			}
			catch(NumberFormatException ne)
			{
				JOptionPane.showMessageDialog(null, "Not a valid value in the Num Day Hyd text box; It must be a number. Aborting insert.");
				return;
			}
			record.setHydrol_method(str4[hydrolTextField.getSelectedIndex()]);
			record.setHydraul_method(str5[hydraulTextField.getSelectedIndex()]);
			if(record.getHydraul_method().equalsIgnoreCase("None"))
			{
				if(!record.getLid().equalsIgnoreCase(record.getUpstream_seg()) && !record.getUpstream_seg().equalsIgnoreCase("xxxxx"))
				{
					JOptionPane.showMessageDialog(null, "Routing Method can be 'None' only if LID and Upstream Segment are the same or Upstream Segment is 'XXXXX'. Aborting insert.");
					return;
				}
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
			record.setSnow_method(str3[snowMethodTextField.getSelectedIndex()]);
			record.setFlowtype(str2[flowTypeTextField.getSelectedIndex()]);
			record.setFcsttype(str1[fcstTypeTextField.getSelectedIndex()]);
//			lhvmLogger.log(" Delete record"+ record.toString());
			LhvmDataManager.databaseMessage = null;
			
			lhvmRowDataList = lhvmDataManager.readDataFromPtEnsemble();
			numRows = lhvmRowDataList.size();
			
			int success = lhvmDataManager.insertOrUpdateEnsembleTable(record);
			
			//lhvmRowDataList = lhvmDataManager.readDataFromPtEnsemble();
			numRowsTemp = lhvmRowDataList.size();
			
			if(success != -1)
			{
				getSelectedRows();
				if(selectedFcstPtEnsembleJTableRowsDataList.size() == 1 && ((numRows+1) == numRowsTemp))
				{
	    			record = new FcstPtESPRecord();
	    			String lid = ((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(0)).getLid();
	    			record.setLid(lid);
	    			record.setHydrol_method(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(0)).getHydrolMethod());
	    			record.setHydraul_method(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(0)).getHydraulMethod());
	    			record.setSnow_method(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(0)).getSnowMethod());
	    			record.setFcsttype(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(0)).getFcstType());
	    			record.setFlowtype(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(0)).getFlowType());
	    			record.setUpstream_seg(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(0)).getUpstreamSeg());
	    			record.setReservoir_model(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(0)).getReservoirModel());
	    			lhvmDataManager.deleteFromEnsembleTable(record);
	    			
	    			appendToConsoleMessageReplace(locationTextField.getText());
				}
				else if(selectedFcstPtEnsembleJTableRowsDataList.size() == 1 && numRows == numRowsTemp)
				{
					appendToConsoleMessageReplace(locationTextField.getText());
				}
				else if(selectedFcstPtEnsembleJTableRowsDataList.size() == 0 && ((numRows+1) == numRowsTemp))
				{
					appendToConsoleMessageInsert(locationTextField.getText());
				}
			}
			else
			{
				JOptionPane.showMessageDialog(null, "Oops: Problem inserting, please check console");
				setDatabaseMessage();
			}
			lhvmRowDataList = lhvmDataManager.readDataFromPtEnsemble();
			jtm.setChangedAllRowDataList(lhvmRowDataList);
			numRows = lhvmRowDataList.size();
			numRowsLabel.setText(numRows + " Rows");
			jtm.refreshDisplay();
			
			for(i=0;i<lhvmRowDataList.size();i++)
    		{
				FcstPtEnsembleJTableRowData rowData = (FcstPtEnsembleJTableRowData) lhvmRowDataList.get(i);
				currentLidFromRowsInsideLoop = rowData.getLid();
				currentHydrolMethodFromRowsInsideLoop = rowData.getHydrolMethod();
				currentHydraulMethodFromRowsInsideLoop = rowData.getHydraulMethod();
				currentSnowMethodFromRowsInsideLoop = rowData.getSnowMethod();
				currentFcstTypeFromRowsInsideLoop = rowData.getFcstType();
				currentFlowTypeFromRowsInsideLoop = rowData.getFlowType();
				currentUpstreamSegFromRowsInsideLoop = rowData.getUpstreamSeg();
				currentReservoirModelFromRowsInsideLoop = rowData.getReservoirModel();
    			if(locationTextField.getText().equalsIgnoreCase(currentLidFromRowsInsideLoop) &&
    			str4[hydrolTextField.getSelectedIndex()].equalsIgnoreCase(currentHydrolMethodFromRowsInsideLoop) &&
    			str5[hydraulTextField.getSelectedIndex()].equalsIgnoreCase(currentHydraulMethodFromRowsInsideLoop) &&
    			str3[snowMethodTextField.getSelectedIndex()].equalsIgnoreCase(currentSnowMethodFromRowsInsideLoop) &&
    			str1[fcstTypeTextField.getSelectedIndex()].equalsIgnoreCase(currentFcstTypeFromRowsInsideLoop) &&
    			upstreamSegTextField.getText().equalsIgnoreCase(currentUpstreamSegFromRowsInsideLoop) &&
    			str9[reservoirModelTextField.getSelectedIndex()].equalsIgnoreCase(currentReservoirModelFromRowsInsideLoop) &&
    			str2[flowTypeTextField.getSelectedIndex()].equalsIgnoreCase(currentFlowTypeFromRowsInsideLoop))
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
    		size = selectedFcstPtEnsembleJTableRowsDataList.size();
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
	    			FcstPtESPRecord record = new FcstPtESPRecord();
	    			String lid = ((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(i)).getLid();
	    			record.setLid(lid);
	    			record.setHydrol_method(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(i)).getHydrolMethod());
	    			record.setHydraul_method(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(i)).getHydraulMethod());
	    			record.setSnow_method(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(i)).getSnowMethod());
	    			record.setFcsttype(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(i)).getFcstType());
	    			record.setFlowtype(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(i)).getFlowType());
	    			record.setUpstream_seg(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(i)).getUpstreamSeg());
	    			record.setReservoir_model(((FcstPtEnsembleJTableRowData)selectedFcstPtEnsembleJTableRowsDataList.get(i)).getReservoirModel());
	    			//lhvmLogger.log(" Delete record"+ record.toString());
	    			LhvmDataManager.databaseMessage = null;
	    			int success = lhvmDataManager.deleteFromEnsembleTable(record);
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
    		
    		lhvmRowDataList = lhvmDataManager.readDataFromPtEnsemble();
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
    
    private class ReservoirsActionListener implements ActionListener
    {	
		public void actionPerformed(ActionEvent e) 
		{			
			if(filterOptionsButton.getText().equalsIgnoreCase("Show reservoirs"))
			{
	    		lhvmRowDataList = lhvmDataManager.readReservoirsFromPtEnsemble();
	    		jtm.setChangedAllRowDataList(lhvmRowDataList);
	    		jtm.refreshDisplay();
	    		deActivateDeleteButton();
	    		deActivateClearButton();
	    		deActivateSaveButton();
	    		clearForm();
				numRows = lhvmRowDataList.size();
				numRowsLabel.setText(numRows + " reservoir(s)");
				
				filterOptionsButton.setText("Show all rows");
			}
			else if(filterOptionsButton.getText().equalsIgnoreCase("Show all rows"))
			{
	    		lhvmRowDataList = lhvmDataManager.readDataFromPtEnsemble();
	    		jtm.setChangedAllRowDataList(lhvmRowDataList);
	    		jtm.refreshDisplay();
	    		deActivateDeleteButton();
	    		deActivateClearButton();
	    		deActivateSaveButton();
	    		clearForm();
				numRows = lhvmRowDataList.size();
				numRowsLabel.setText(numRows + " Rows");
				
				filterOptionsButton.setText("Show reservoirs");
			}
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
    
	private class RefreshActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		lhvmRowDataList = lhvmDataManager.readDataFromPtEnsemble();
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
    
    private class EnsembleTableViewListSelectListener implements ListSelectionListener
	{
    	public void valueChanged(ListSelectionEvent e)
    	{
    		buttonsActivateWhenSelected();
    	}
	}
    
    private class TextFieldClickListener implements MouseListener
    {	
		public void mouseClicked(MouseEvent e) 
		{
		}

		public void mouseEntered(MouseEvent e)
		{
		}

		public void mouseExited(MouseEvent e) 
		{
		}

		public void mousePressed(MouseEvent e) 
		{
		}

		public void mouseReleased(MouseEvent e) 
		{
    		JTextField jt = (JTextField) e.getSource();
    		if("Y".equalsIgnoreCase(jt.getText()))
    		{
    			jt.setText("N");
    		}
    		else
    		{
    			jt.setText("Y");
    		}	
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
    				currentLidFromRowsInsideLoop = ((FcstPtEnsembleJTableRowData) lhvmRowDataList.get(i)).getLid();
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
    		selectedFcstPtEnsembleJTableRowsDataList = searchList;
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
