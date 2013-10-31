package ohd.hseb.fcstservice;   

import java.util.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.*;
import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.util.gui.*;

public class FcstDataDetermWindow implements Subject
{
	private DataServicesGlobals dataServicesGlobals = null;
	private java.util.List lhvmRowDataList = null;
	private LhvmDataManager lhvmDataManager = null;
	private LhvmLogger lhvmLogger = null;
	private JTableManager jtm = null;
	private JFrame lhvm = null;
	
	private FcstPtDetermJTableRowData selectedFcstPtDetermJTableRowData = null;
	private ArrayList selectedFcstPtDetermJTableRowsDataList = null;

	private JTextField locationTextField = null;
	private JComboBox snowMethodTextField = null;	
	private JComboBox hydrolTextField = null;
	private JComboBox hydraulTextField = null;	
	private JComboBox issueCriteriaTextField = null;
	private JComboBox floodTextField = null;
	private JComboBox droughtTextField = null;	
	private JComboBox horizonTextField = null;	
	private JTextField qpfTextField = null;
	private JTextField qtfTextField = null;	
	private JTextField qzfTextField = null;
	private JComboBox normalTextField = null;
	private JTextField upstreamSegTextField = null;
	private JComboBox reservoirModelTextField = null;
	private JComboBox genMethodTextField = null;
	private JTextField consumptiveUseTextField = null;
	private JTextField channelLossTextField = null;
	private JTextField varUsageTextField = null;
	private JTextField numElevZonesTextField = null;
	
	private JButton insertButton = null;
	private JButton deleteButton = null;
	private JButton clearButton = null;
	private JButton closeButton = null;
	private JButton refreshButton = null;
	
	private JTextField stationSearchTextField = null;
	
	private JScrollPane tableScrollPane = null;
	private JPanel tableScrollPanePanel = null;
	
	private DateTextField implDateTextField = null;	
	private DateTextField webDateTextField = null;
	
	private int numRows;
	private JLabel numRowsLabel = null;
	private JTextArea consoleTextArea = null;
	private int origNumRows;
	
	private java.util.List lhvmRowDataList1 = null;
	private java.util.List lhvmRowDataList2 = null;
	private java.util.List lhvmRowDataList3 = null;
	private java.util.List lhvmRowDataList4 = null;
	private java.util.List lhvmRowDataList5 = null;
	private java.util.List lhvmRowDataList7 = null;
	private java.util.List lhvmRowDataList8 = null;
	private java.util.List lhvmRowDataList9 = null;
	
	private String str1[] = null;
	private String str2[] = null;
	private String str3[] = null;
	private String str4[] = null;
	private String str5[] = null;
	private String str7[] = null;
	private String str8[] = null;
	private String str9[] = null;
		
	private ArrayList observers = null;
	
	private JButton filterOptionsButton;
	
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
	    if(LhvmDataManager.displayedMessageDetermWindow != null)
	    {
	    	tempStr = new String(LhvmDataManager.displayedMessageDetermWindow);
	    }
	    LhvmDataManager.displayedMessageDetermWindow = new StringBuffer();
	    if(tempStr != null)
	    {
	    	LhvmDataManager.displayedMessageDetermWindow.append(tempStr);
	    }
	    LhvmDataManager.displayedMessageDetermWindow.append("\n--------------\n");
	    LhvmDataManager.displayedMessageDetermWindow.append(LhvmDataManager.databaseMessage);
	    //consoleTextArea.insert(new String(LhvmDataManager.displayedMessageDetermWindow), 0);
	    consoleTextArea.append(new String(LhvmDataManager.displayedMessageDetermWindow));
	}
	
	private void appendToConsoleMessageDelete(String lid)
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
	
	public FcstDataDetermWindow(String jdbcUrl)
	{		 
		dataServicesGlobals = DataServicesGlobals.getSingleInstanceOfDataServicesGlobals();
		this.observers = DataServicesGlobals.dataDetermObservers;
		
		lhvm = new JFrame("Deterministic Forecast");
	    lhvm.setSize(1000, 500);
	    lhvm.setLocation(20,50);
	    
	    //Fix the dimensions of this window i.e. dis-allow window resizing
	    Dimension d = new Dimension(1275, 935);
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

	    String[] columnsToBeDisplayed = {"Location ID", "Snow Method", "Hydrologic Method", "Reservoir Model", "Upstream Segment", "Routing Method", "Def Issue Criteria", "Qpf(hours)", "Normal Upd Freq", "Flood Upd Freq", "Drought Upd Freq", "Forecast Period", "Qtf(hours)", "Qzf(hours)", "Num Elev Zones", "Consumptive Use", "Channel Loss", "Typical Fcst Gen Method", "Implementation Date", "Web Date", "VAR Usage"};

	    lhvmLogger = new LhvmLogger();
	    lhvmDataManager = LhvmDataManager.getSingleInstanceOfDataManager(jdbcUrl, lhvmLogger,"-");
	    java.util.List lhvmColumnDescriptorList = new ArrayList();
	    lhvmRowDataList = lhvmDataManager.readDataFromPtDeterm();
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Location ID", true, 90, "center", "location id"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Hydrologic Method", true, 150, "center", "hydrologic computation method for the segment"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Snow Method", true, 90, "center", "snow computational method for the segment"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Routing Method", true, 120, "center", "routing computational method for the segment"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Def Issue Criteria", true, 150, "center", "minimum issuance criteria for the forecast service"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Qpf(hours)", true, 90, "center", "hours of QPF used in streamflow forecast"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Upstream Segment", true, 120, "center", "location id of upstream point may be forecast or data pointif point is a headwater point, then this value is set to the value of the lid column"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Reservoir Model", true, 120, "center", "reservoir model for the segment"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Normal Upd Freq", true, 120, "center", "normal operations update frequency"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Flood Upd Freq", true, 120, "center", "flood operations update frequency"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Drought Upd Freq", true, 120, "center", "drought operations update frequency"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Implementation Date", true, 150, "center", "date service was first produce"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Forecast Period", true, 150, "center", ""));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Typical Fcst Gen Method", true, 180, "center", "forecast generation method"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("VAR Usage", true, 180, "center", ""));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Consumptive Use", true, 120, "center", "indicates consumptive use model is used for the segment (Y or N)"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Qtf(hours)", true, 90, "center", "hours of QTF used in streamflow forecast"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Qzf(hours)", true, 90, "center", "hours of freezing level forecast used in streamflow forecast"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Web Date", true, 90, "center", "date official forecast hydrograph first available on AHPS website"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Channel Loss", true, 120, "center", "indicates channel loss model is used for the segment (Y or N)"));
	    lhvmColumnDescriptorList.add(new JTableColumnDescriptor("Num Elev Zones", true, 120, "center", "number of elevation zones in mountainous area may be 1,2 or 3 else should be set to 1"));
  
	    jtm = new ComplexJTableManager(lhvmColumnDescriptorList, lhvmRowDataList);
	    jtm.setDisplayableColumns(columnsToBeDisplayed, false, true);
	    jtm.setPreferredSizeForJScrollPane(new Dimension(1200, 500));    
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
	    editPanel.setLayout(new GridLayout(2,5));
		editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor"));
		editPanel.setBackground(new Color(0,150,255));
		
	    JPanel editPanel2 = new JPanel();
	    editPanel2.setLayout(new GridLayout(2,5));
		//editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Fields with red text are non-editable"));
		editPanel2.setBackground(new Color(0,150,255));
		
	    JPanel editPanel3 = new JPanel();
	    editPanel3.setLayout(new GridLayout(2,5));
		//editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Fields with red text are non-editable"));
		editPanel3.setBackground(new Color(0,150,255));
		
	    JPanel editPanel4 = new JPanel();
	    editPanel4.setLayout(new GridLayout(2,5));
		//editPanel.setBorder(BorderFactory.createTitledBorder("Column Editor -- Fields with red text are non-editable"));
		editPanel4.setBackground(new Color(0,150,255));
		
		c.gridy++;
		
		JLabel locationIdLabel = new JLabel("Location ID");
		JLabel hydrolLabel = new JLabel("Hydrologic Method");
		JLabel snowLabel = new JLabel("Snow Method");
		JLabel issueCriteriaLabel = new JLabel("Def Issue Criteria");
		JLabel normalLabel = new JLabel("Normal Upd Freq");
		JLabel floodLabel = new JLabel("Flood Upd Freq");
		JLabel droughtLabel = new JLabel("Drought Upd Freq");
		JLabel implDateLabel = new JLabel("Implementation Date");
		JLabel horizonLabel = new JLabel("Forecast Period");
		JLabel qpfLabel = new JLabel("Qpf(hours)");
		JLabel qtfLabel = new JLabel("Qtf(hours)");
		JLabel qzfLabel = new JLabel("Qzf(hours)");
		JLabel hydraulLabel = new JLabel("Routing Method");
		JLabel webDateLabel = new JLabel("Web Date");		
		JLabel upstreamLabel = new JLabel("Upstream Segment");
		JLabel reservoirLabel = new JLabel("Reservoir Model");
		JLabel genMethodLabel = new JLabel("Typical Fcst Gen Method");
		JLabel varUsageLabel = new JLabel("VAR Usage");
		JLabel consumptiveUseLabel = new JLabel("Consumptive Use");
		JLabel channelLossLabel = new JLabel("Channel Loss");
		JLabel numElevZonesLabel = new JLabel("Num Elev Zones");
		
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
		qpfTextField = new JTextField("-");
		qpfTextField.setBackground(new Color(255,175,75));
		qpfTextField.setForeground(new Color(50,150,50));
		qpfTextField.setFont(fnt);
		qpfTextField.setCaretColor(Color.red);
		qpfTextField.setSize(10,10);
		qpfTextField.addKeyListener(textTypedActionListener);		
		qtfTextField = new JTextField();
		qtfTextField.setBackground(Color.yellow);
		qtfTextField.setForeground(new Color(50,150,50));
		qtfTextField.setFont(fnt);
		qtfTextField.setCaretColor(Color.red);
		qtfTextField.setSize(10,10);
		qtfTextField.addKeyListener(textTypedActionListener);		
		qzfTextField = new JTextField();
		qzfTextField.setBackground(Color.yellow);
		qzfTextField.setForeground(new Color(50,150,50));
		qzfTextField.setFont(fnt);
		qzfTextField.setCaretColor(Color.red);
		qzfTextField.setSize(10,10);
		qzfTextField.addKeyListener(textTypedActionListener);	
		varUsageTextField = new JTextField("N");
		varUsageTextField.setBackground(Color.yellow);
		varUsageTextField.setForeground(new Color(50,150,50));
		varUsageTextField.setFont(fnt);
		varUsageTextField.setCaretColor(Color.red);
		varUsageTextField.setSize(10,10);
		varUsageTextField.setEditable(false);
		varUsageTextField.addMouseListener(textFieldClickListener);
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
		
		lhvmRowDataList1 = lhvmDataManager.readDataFromIssueCriteria();
		lhvmRowDataList2 = lhvmDataManager.readDataFromHydrologicMethod();
		lhvmRowDataList3 = lhvmDataManager.readDataFromRoutingMethod();
		lhvmRowDataList4 = lhvmDataManager.readDataFromSnowMethod();
		lhvmRowDataList5 = lhvmDataManager.readDataFromFrequencyUpdate();		
		lhvmRowDataList7 = lhvmDataManager.readDataFromReservoirModel();
		lhvmRowDataList8 = lhvmDataManager.readDataFromGenMethod();
		lhvmRowDataList9 = lhvmDataManager.readDataFromHorizon();
		
		str1 = new String[lhvmRowDataList1.size()];
		str2 = new String[lhvmRowDataList2.size()];
		str3 = new String[lhvmRowDataList3.size()];
		str4 = new String[lhvmRowDataList4.size()];
		str5 = new String[lhvmRowDataList5.size()];
		str7 = new String[lhvmRowDataList7.size()];
		str8 = new String[lhvmRowDataList8.size()];
		str9 = new String[lhvmRowDataList9.size()];
		
		for(int i=0;i<lhvmRowDataList1.size();i++)
		{
			str1[i] = ((IssueCriteriaJTableRowData) lhvmRowDataList1.get(i)).getIssueCriteria();
		}
		for(int i=0;i<lhvmRowDataList2.size();i++)
		{
			str2[i] = ((HydrologicMethodJTableRowData) lhvmRowDataList2.get(i)).getHydrologicMethod();
		}
		for(int i=0;i<lhvmRowDataList3.size();i++)
		{
			str3[i] = ((RoutingMethodJTableRowData) lhvmRowDataList3.get(i)).getRoutingMethod();
		}
		for(int i=0;i<lhvmRowDataList4.size();i++)
		{
			str4[i] = ((SnowMethodJTableRowData) lhvmRowDataList4.get(i)).getSnowMethod();
		}
		for(int i=0;i<lhvmRowDataList5.size();i++)
		{
			str5[i] = ((FrequencyUpdateJTableRowData) lhvmRowDataList5.get(i)).getFrequencyUpdate();
		}
		for(int i=0;i<lhvmRowDataList7.size();i++)
		{
			str7[i] = ((ReservoirModelJTableRowData) lhvmRowDataList7.get(i)).getReservoirModel();
		}
		for(int i=0;i<lhvmRowDataList8.size();i++)
		{
			str8[i] = ((FcstGenMethodJTableRowData) lhvmRowDataList8.get(i)).getGenMethod();
		}
		for(int i=0;i<lhvmRowDataList9.size();i++)
		{
			str9[i] = ((FcstHorizonJTableRowData) lhvmRowDataList9.get(i)).getFcstHorizon();
		}
		
		issueCriteriaTextField = new JComboBox(str1);
		issueCriteriaTextField.setBackground(new Color(255,175,75));
		issueCriteriaTextField.setForeground(new Color(50,150,50));
		hydrolTextField = new JComboBox(str2);
		hydrolTextField.setBackground(new Color(255,175,75));
		hydrolTextField.setForeground(new Color(50,150,50));
		hydraulTextField = new JComboBox(str3);
		hydraulTextField.setBackground(new Color(255,175,75));
		hydraulTextField.setForeground(new Color(50,150,50));
		snowMethodTextField = new JComboBox(str4);
		snowMethodTextField.setBackground(new Color(255,175,75));
		snowMethodTextField.setForeground(new Color(50,150,50));
		normalTextField = new JComboBox(str5);
		normalTextField.setBackground(Color.yellow);
		normalTextField.setForeground(new Color(50,150,50));
		floodTextField = new JComboBox(str5);
		floodTextField.setBackground(Color.yellow);
		floodTextField.setForeground(new Color(50,150,50));
		droughtTextField = new JComboBox(str5);
		droughtTextField.setBackground(Color.yellow);
		droughtTextField.setForeground(new Color(50,150,50));		
		reservoirModelTextField = new JComboBox(str7);
		reservoirModelTextField.setBackground(new Color(255,175,75));
		reservoirModelTextField.setForeground(new Color(50,150,50));		
		genMethodTextField = new JComboBox(str8);
		genMethodTextField.setBackground(Color.yellow);
		genMethodTextField.setForeground(new Color(50,150,50));
		horizonTextField = new JComboBox(str9);
		horizonTextField.setBackground(Color.yellow);
		horizonTextField.setForeground(new Color(50,150,50));
		
		editPanel.add(locationIdLabel);
		editPanel.add(snowLabel);
		editPanel.add(hydrolLabel);
		editPanel.add(reservoirLabel);
		editPanel.add(upstreamLabel);
		
		editPanel2.add(hydraulLabel);
		editPanel2.add(issueCriteriaLabel);	
		editPanel2.add(qpfLabel);
		editPanel2.add(normalLabel);
		editPanel2.add(floodLabel);		
		
		editPanel3.add(droughtLabel);
		editPanel3.add(horizonLabel);
		editPanel3.add(qtfLabel);
		editPanel3.add(qzfLabel);
		editPanel3.add(numElevZonesLabel);
				
		editPanel4.add(consumptiveUseLabel);
		editPanel4.add(channelLossLabel);
		editPanel4.add(genMethodLabel);
		editPanel4.add(implDateLabel);
		editPanel4.add(webDateLabel);
		editPanel4.add(varUsageLabel);
				
		editPanel.add(locationTextField);
		editPanel.add(snowMethodTextField);
		editPanel.add(hydrolTextField);
		editPanel.add(reservoirModelTextField);
		editPanel.add(upstreamSegTextField);		
		
		editPanel2.add(hydraulTextField);
		editPanel2.add(issueCriteriaTextField);
		editPanel2.add(qpfTextField);
		editPanel2.add(normalTextField);
		editPanel2.add(floodTextField);		
		
		editPanel3.add(droughtTextField);
		editPanel3.add(horizonTextField);
		editPanel3.add(qtfTextField);
		editPanel3.add(qzfTextField);
		editPanel3.add(numElevZonesTextField);
		
		editPanel4.add(consumptiveUseTextField);		
		editPanel4.add(channelLossTextField);
		editPanel4.add(genMethodTextField);
		editPanel4.add(implDateTextField);		
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
		if(selectedFcstPtDetermJTableRowData != null)
		{
			locationTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Location ID"));
			implDateTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Implementation Date"));
			webDateTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Web Date"));
			qpfTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Qpf(hours)"));
			qtfTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Qtf(hours)"));
			qzfTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Qzf(hours)"));
			consumptiveUseTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Consumptive Use"));
			channelLossTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Channel Loss"));
			varUsageTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("VAR Usage"));
			numElevZonesTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Num Elev Zones"));
			upstreamSegTextField.setText(selectedFcstPtDetermJTableRowData.getDataValue("Upstream Segment"));
			
			hydrolTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Hydrologic Method"));
			hydraulTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Routing Method"));
			issueCriteriaTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Def Issue Criteria"));
			snowMethodTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Snow Method"));
			normalTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Normal Upd Freq"));
			floodTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Flood Upd Freq"));
			droughtTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Drought Upd Freq"));
			reservoirModelTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Reservoir Model"));
			genMethodTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Typical Fcst Gen Method"));
			horizonTextField.setSelectedItem(selectedFcstPtDetermJTableRowData.getDataValue("Forecast Period"));
			
			selectedFcstPtDetermJTableRowData = null;
			
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
		qpfTextField.setText("-");
		qtfTextField.setText("");
		qzfTextField.setText("");
		consumptiveUseTextField.setText("N");
		varUsageTextField.setText("N");
		channelLossTextField.setText("N");
		numElevZonesTextField.setText("1");
		upstreamSegTextField.setText("XXXXX");
	} 
	
	public void buttonsActivateWhenKeysTyped()
    {
    	int either = 0;
		
		if((upstreamSegTextField.getText().length() >= 0 && !upstreamSegTextField.getText().equals("XXXXX")) ||
		(qpfTextField.getText().length() >= 0 && !qpfTextField.getText().equals("-")) ||
    	(numElevZonesTextField.getText().length() >= 0 && !numElevZonesTextField.getText().equals("1")))
    	{
    		activateClearButton();
    		either++;
    	}
		if(qtfTextField.getText().length() > 0 || qzfTextField.getText().length() > 0)
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
		if(selectedFcstPtDetermJTableRowsDataList != null && selectedFcstPtDetermJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
		else if(selectedFcstPtDetermJTableRowsDataList != null && selectedFcstPtDetermJTableRowsDataList.size() == 1)
		{
			selectedFcstPtDetermJTableRowData = (FcstPtDetermJTableRowData) selectedFcstPtDetermJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
			activateSaveButton();
			activateClearButton();
		}
		else if(selectedFcstPtDetermJTableRowsDataList != null && selectedFcstPtDetermJTableRowsDataList.size() > 1)
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
		if(selectedFcstPtDetermJTableRowsDataList.size() < 1)
		{
			deActivateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
		else if(selectedFcstPtDetermJTableRowsDataList.size() == 1)
		{
			selectedFcstPtDetermJTableRowData = (FcstPtDetermJTableRowData) selectedFcstPtDetermJTableRowsDataList.get(0);
			showCurrentRowData();
			activateDeleteButton();
			activateSaveButton();
			activateClearButton();
		}
		else if(selectedFcstPtDetermJTableRowsDataList.size() > 1)
		{
			activateDeleteButton();
			deActivateSaveButton();
			deActivateClearButton();
			clearForm();
		}
	}
    
	private void getSelectedRows()
	{
		selectedFcstPtDetermJTableRowsDataList = (ArrayList) jtm.getSelectedRowsData();
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
        	String currentIssueCritFromRowsInsideLoop = null;
        	String currentUpstreamSegFromRowsInsideLoop = null;
        	String currentReservoirModelFromRowsInsideLoop = null;
        	int currentHoursQpfFromRowsInsideLoop;
        	int i, null_or_not = 0;
        	rowIndex[0] = -1;
        	FcstPtDetermRecord record = new FcstPtDetermRecord();
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
			record.setSnow_method(str4[snowMethodTextField.getSelectedIndex()]);
			record.setFrequpd_normal(str5[normalTextField.getSelectedIndex()]);
			record.setFrequpd_flood(str5[floodTextField.getSelectedIndex()]);
			record.setFrequpd_drought(str5[droughtTextField.getSelectedIndex()]);
			record.setUpstream_seg(upstreamSegTextField.getText().toUpperCase());
			record.setReservoir_model(str7[reservoirModelTextField.getSelectedIndex()]);
			record.setFcst_gen_method(str8[genMethodTextField.getSelectedIndex()]);
			record.setFcst_horizon(str9[horizonTextField.getSelectedIndex()]);
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
				if(qpfTextField.getText().length() > 0)
				{
					record.setHours_qpf(Short.valueOf(qpfTextField.getText().trim()));
				}
			}
			catch(NumberFormatException ne)
			{
				JOptionPane.showMessageDialog(null, "Not a valid value in the QPF text box; It must be a number. Aborting insert.");
				return;
			}
			try
			{
				if(qtfTextField.getText().length() > 0 && !qtfTextField.getText().equals(lhvmDataManager.getMissingRepresentation()))
				{
					record.setHours_qtf(Short.valueOf(qtfTextField.getText().trim()));
				}
				else
				{
					null_or_not = 1;
				}
			}
			catch(NumberFormatException ne)
			{
				JOptionPane.showMessageDialog(null, "Not a valid value in the QTF text box; It must be a number. Aborting insert.");
				return;
			}
			try
			{
				if(qzfTextField.getText().length() > 0 && !qzfTextField.getText().equals(lhvmDataManager.getMissingRepresentation()))
				{
					record.setHours_qzf(Short.valueOf(qzfTextField.getText().trim()));
				}
				else
				{
					null_or_not = 1;
				}
			}
			catch(NumberFormatException ne)
			{
				JOptionPane.showMessageDialog(null, "Not a valid value in the QZF text box; It must be a number. Aborting insert.");
				return;
			}
			record.setHydrol_method(str2[hydrolTextField.getSelectedIndex()]);
			record.setHydraul_method(str3[hydraulTextField.getSelectedIndex()]);
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
			record.setDef_issue_crit(str1[issueCriteriaTextField.getSelectedIndex()]);
//			lhvmLogger.log(" Delete record"+ record.toString());
			LhvmDataManager.databaseMessage = null;
			
			lhvmRowDataList = lhvmDataManager.readDataFromPtDeterm();
			numRows = lhvmRowDataList.size();
			
			int success = lhvmDataManager.insertOrUpdateDetermTable(record);
			
			//lhvmRowDataList = lhvmDataManager.readDataFromPtDeterm();
			numRowsTemp = lhvmRowDataList.size();
			
			if(success != -1)
			{	
				getSelectedRows();
				if(selectedFcstPtDetermJTableRowsDataList.size() == 1 && ((numRows+1) == numRowsTemp))
				{
	    			record = new FcstPtDetermRecord();
	    			String lid = ((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(0)).getLid();
	    			record.setLid(lid);
	    			record.setHydrol_method(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(0)).getHydrolMethod());
	    			record.setHydraul_method(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(0)).getHydraulMethod());
	    			record.setDef_issue_crit(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(0)).getIssueCriteria());
	    			record.setSnow_method(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(0)).getSnowMethod());
	    			record.setUpstream_seg(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(0)).getUpstreamSeg());
	    			record.setReservoir_model(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(0)).getReservoirModel());
	    			record.setHours_qpf((short) (((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(0)).getQpf()));
	    			lhvmDataManager.deleteFromDetermTable(record);
	    			
	    			appendToConsoleMessageReplace(locationTextField.getText());
				}
				else if(selectedFcstPtDetermJTableRowsDataList.size() == 1 && numRows == numRowsTemp)
				{
					appendToConsoleMessageReplace(locationTextField.getText());
				}
				else if(selectedFcstPtDetermJTableRowsDataList.size() == 0 && ((numRows+1) == numRowsTemp))
				{
					appendToConsoleMessageInsert(locationTextField.getText());
				}
			}
			else
			{
				JOptionPane.showMessageDialog(null, "Oops: Problem inserting, please check console");
				setDatabaseMessage();
			}
			lhvmRowDataList = lhvmDataManager.readDataFromPtDeterm();
			jtm.setChangedAllRowDataList(lhvmRowDataList);
			numRows = lhvmRowDataList.size();
			numRowsLabel.setText(numRows + " Rows");
			jtm.refreshDisplay();
			
			for(i=0;i<lhvmRowDataList.size();i++)
    		{
				FcstPtDetermJTableRowData rowData = (FcstPtDetermJTableRowData) lhvmRowDataList.get(i);
				currentLidFromRowsInsideLoop = rowData.getLid();
				currentHydrolMethodFromRowsInsideLoop = rowData.getHydrolMethod();
				currentHydraulMethodFromRowsInsideLoop = rowData.getHydraulMethod();
				currentSnowMethodFromRowsInsideLoop = rowData.getSnowMethod();
				currentIssueCritFromRowsInsideLoop = rowData.getIssueCriteria();
				currentUpstreamSegFromRowsInsideLoop = rowData.getUpstreamSeg();
				currentReservoirModelFromRowsInsideLoop = rowData.getReservoirModel();
				currentHoursQpfFromRowsInsideLoop = rowData.getQpf();
    			if(locationTextField.getText().equalsIgnoreCase(currentLidFromRowsInsideLoop) &&
    			str2[hydrolTextField.getSelectedIndex()].equalsIgnoreCase(currentHydrolMethodFromRowsInsideLoop) &&
    			str3[hydraulTextField.getSelectedIndex()].equalsIgnoreCase(currentHydraulMethodFromRowsInsideLoop) &&
    			str4[snowMethodTextField.getSelectedIndex()].equalsIgnoreCase(currentSnowMethodFromRowsInsideLoop) &&
    			upstreamSegTextField.getText().equalsIgnoreCase(currentUpstreamSegFromRowsInsideLoop) &&
    			str7[reservoirModelTextField.getSelectedIndex()].equalsIgnoreCase(currentReservoirModelFromRowsInsideLoop) &&
    			qpfTextField.getText().trim().equalsIgnoreCase(String.valueOf(currentHoursQpfFromRowsInsideLoop)) &&
    			str1[issueCriteriaTextField.getSelectedIndex()].equalsIgnoreCase(currentIssueCritFromRowsInsideLoop))
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
    		lhvmRowDataList = lhvmDataManager.readDataFromPtDeterm();
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
    		size = selectedFcstPtDetermJTableRowsDataList.size();
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
	    			FcstPtDetermRecord record = new FcstPtDetermRecord();
	    			String lid = ((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(i)).getLid();
	    			record.setLid(lid);
	    			record.setHydrol_method(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(i)).getHydrolMethod());
	    			record.setHydraul_method(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(i)).getHydraulMethod());
	    			record.setDef_issue_crit(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(i)).getIssueCriteria());
	    			record.setSnow_method(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(i)).getSnowMethod());
	    			record.setUpstream_seg(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(i)).getUpstreamSeg());
	    			record.setReservoir_model(((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(i)).getReservoirModel());
	    			record.setHours_qpf((short) (((FcstPtDetermJTableRowData)selectedFcstPtDetermJTableRowsDataList.get(i)).getQpf()));
	    			//lhvmLogger.log(" Delete record"+ record.toString());
	    			LhvmDataManager.databaseMessage = null;
	    			int success = lhvmDataManager.deleteFromDetermTable(record);
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
    		
    		lhvmRowDataList = lhvmDataManager.readDataFromPtDeterm();
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
    
    private class ReservoirsActionListener implements ActionListener
    {	
		public void actionPerformed(ActionEvent e) 
		{			
			if(filterOptionsButton.getText().equalsIgnoreCase("Show reservoirs"))
			{
	    		lhvmRowDataList = lhvmDataManager.readReservoirsFromPtDeterm();
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
	    		lhvmRowDataList = lhvmDataManager.readDataFromPtDeterm();
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
    				currentLidFromRowsInsideLoop = ((FcstPtDetermJTableRowData) lhvmRowDataList.get(i)).getLid();
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
    		selectedFcstPtDetermJTableRowsDataList = searchList;
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
