package ohd.hseb.fcstservice;   

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import javax.swing.*;

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.util.gui.*;

public class ReferenceTablesWindow
{
	private DataServicesGlobals dataServicesGlobals = null;
	private java.util.List lhvmRowDataList1 = null;
	private java.util.List lhvmRowDataList2 = null;
	private java.util.List lhvmRowDataList3 = null;
	private java.util.List lhvmRowDataList4 = null;
	private java.util.List lhvmRowDataList5 = null;	
	private java.util.List lhvmRowDataList6 = null;
	private java.util.List lhvmRowDataList7 = null;
	private java.util.List lhvmRowDataList8 = null;
	private java.util.List lhvmRowDataList9 = null;
	private java.util.List lhvmRowDataList10 = null;
	private java.util.List lhvmRowDataList11 = null;
	private java.util.List lhvmRowDataList12 = null;
	private java.util.List lhvmRowDataList13 = null;
	private java.util.List lhvmRowDataList14 = null;
	private java.util.List lhvmRowDataList15 = null;
	private java.util.List lhvmRowDataList16 = null;
	private java.util.List lhvmRowDataList17 = null;
	private java.util.List lhvmRowDataList18 = null;
	
	private LhvmDataManager lhvmDataManager = null;
	private LhvmLogger lhvmLogger = null;
	private JFrame lhvm = null;
	
	private JTableManager jtm1 = null;
	private JTableManager jtm2 = null;
	private JTableManager jtm3 = null;
	private JTableManager jtm4 = null;
	private JTableManager jtm5 = null;
	private JTableManager jtm6 = null;
	private JTableManager jtm7 = null;
	private JTableManager jtm8 = null;
	private JTableManager jtm9 = null;
	private JTableManager jtm10 = null;
	private JTableManager jtm11 = null;
	private JTableManager jtm12 = null;
	private JTableManager jtm13 = null;
	private JTableManager jtm14 = null;
	private JTableManager jtm15 = null;
	private JTableManager jtm16 = null;
	private JTableManager jtm17 = null;
	private JTableManager jtm18 = null;
	
	private JScrollPane tableScrollPane1 = null;
	private JPanel tableScrollPanePanel1 = null;
	private JScrollPane tableScrollPane2 = null;
	private JPanel tableScrollPanePanel2 = null;
	private JScrollPane tableScrollPane3 = null;
	private JPanel tableScrollPanePanel3 = null;
	private JScrollPane tableScrollPane4 = null;
	private JPanel tableScrollPanePanel4 = null;
	private JScrollPane tableScrollPane5 = null;
	private JPanel tableScrollPanePanel5 = null;
	private JScrollPane tableScrollPane6 = null;
	private JPanel tableScrollPanePanel6 = null;
	private JScrollPane tableScrollPane7 = null;
	private JPanel tableScrollPanePanel7 = null;
	private JScrollPane tableScrollPane8 = null;
	private JPanel tableScrollPanePanel8 = null;
	private JScrollPane tableScrollPane9 = null;
	private JPanel tableScrollPanePanel9 = null;
	private JScrollPane tableScrollPane10 = null;
	private JPanel tableScrollPanePanel10 = null;
	private JScrollPane tableScrollPane11 = null;
	private JPanel tableScrollPanePanel11 = null;
	private JScrollPane tableScrollPane12 = null;
	private JPanel tableScrollPanePanel12 = null;
	private JScrollPane tableScrollPane13 = null;
	private JPanel tableScrollPanePanel13 = null;
	private JScrollPane tableScrollPane14 = null;
	private JPanel tableScrollPanePanel14 = null;
	private JScrollPane tableScrollPane15 = null;
	private JPanel tableScrollPanePanel15 = null;
	private JScrollPane tableScrollPane16 = null;
	private JPanel tableScrollPanePanel16 = null;
	private JScrollPane tableScrollPane17 = null;
	private JPanel tableScrollPanePanel17 = null;
	private JScrollPane tableScrollPane18 = null;
	private JPanel tableScrollPanePanel18 = null;
	
	private JButton closeButton = null;
	
	private void setDatabaseMessage()
	{
	    String tempStr = null;
	    if(LhvmDataManager.displayedMessageReferencesWindow != null)
	    {
	    	tempStr = new String(LhvmDataManager.displayedMessageReferencesWindow);
	    }
	    LhvmDataManager.displayedMessageReferencesWindow = new StringBuffer();
	    LhvmDataManager.displayedMessageReferencesWindow.append(LhvmDataManager.databaseMessage);
	    if(tempStr != null)
	    {
	    	LhvmDataManager.displayedMessageReferencesWindow.append("\n").append(tempStr);
	    }
	}
	
	public ReferenceTablesWindow(String jdbcUrl)
	{		 
		dataServicesGlobals = DataServicesGlobals.getSingleInstanceOfDataServicesGlobals();
		
		lhvm = new JFrame("Forecast Services Reference Tables");
	    lhvm.setSize(1000, 500);
	    lhvm.setLocation(20,50);
	    lhvm.setLayout(new SpringLayout());
	    
	    //Fix the dimensions of this window i.e. dis-allow window resizing
	    Dimension d = new Dimension(980, 850);
	    new WindowResizingManager(lhvm, d, d);
	    
	    lhvm.setResizable(false);
	
	    JLabel consoleLabel = new JLabel("-----Database Messages-----");
	    consoleLabel.setForeground(Color.red);
	    
	    JLabel separatorLabel1 = new JLabel("==================================================================================================");
	    separatorLabel1.setForeground(Color.red);
	    JLabel separatorLabel2 = new JLabel("==================================================================================================");
	    separatorLabel2.setForeground(Color.red);
	    JLabel separatorLabel3 = new JLabel("==================================================================================================");
	    separatorLabel3.setForeground(Color.red);
	    
		closeButton = new JButton("Close Window");
		closeButton.addActionListener(new CloseActionListener());
		closeButton.setEnabled(true);
	    
	    JTextArea consoleTextArea = new JTextArea(1,50);
	    //Font font = new Font("Serif", Font.BOLD, 15);
	    //consoleTextArea.setFont(font);
	    consoleTextArea.setForeground(Color.green);
	    consoleTextArea.setBackground(Color.black);
	    consoleTextArea.setCaretColor(Color.red);
	    consoleTextArea.setBorder(BorderFactory.createEtchedBorder() );
	    consoleTextArea.setLineWrap(true);

	    String[] columnsToBeDisplayed1 = {"Fcst Type"};
	    String[] columnsToBeDisplayed2 = {"Flow Type"};
	    String[] columnsToBeDisplayed3 = {"Hydrologic Method"};
	    String[] columnsToBeDisplayed4 = {"Def Issue Criteria"};
	    String[] columnsToBeDisplayed5 = {"Routing Method"};
	    String[] columnsToBeDisplayed6 = {"Snow Method"};
	    String[] columnsToBeDisplayed7 = {"Wat Sup Coord Agency"};
	    String[] columnsToBeDisplayed8 = {"Wat Sup Criterion"};
	    String[] columnsToBeDisplayed9 = {"Wat Sup Method"};
	    String[] columnsToBeDisplayed10 = {"Frequency Update"};
	    String[] columnsToBeDisplayed11 = {"Required Period"};
	    String[] columnsToBeDisplayed12 = {"Post Processing"};
	    String[] columnsToBeDisplayed13 = {"Horizon"};
	    String[] columnsToBeDisplayed14 = {"Typical Fcst Gen Method"};
	    String[] columnsToBeDisplayed15 = {"Reservoir Model"};
	    String[] columnsToBeDisplayed16 = {"Wat Sup Resp Agency"};
	    String[] columnsToBeDisplayed17 = {"Service Type"};
	    String[] columnsToBeDisplayed18 = {"Verification Response Type"};
	    
	    JLabel fcstTypeLabel = new JLabel("            Fcst Type");
	    JLabel flowTypeLabel = new JLabel("            Flow Type");
	    JLabel hydrologicMethodLabel = new JLabel("            Hydrologic Method");
	    JLabel issueCriteriaLabel = new JLabel("            Def Issue Criteria");	    
	    JLabel routingMethodLabel = new JLabel("            Routing Method");
	    JLabel snowMethodLabel = new JLabel("            Snow Type");
	    JLabel watsupCoordAgencyLabel = new JLabel("              Wat Sup Coord Agency");
	    JLabel watsupCriterionLabel = new JLabel("            Wat Sup Criterion");
	    JLabel watsupMethodLabel = new JLabel("            Wat Sup Method");
	    JLabel frequencyUpdateLabel = new JLabel("            Frequency Update");
	    JLabel requiredPeriodLabel = new JLabel("            Required Period");
	    JLabel postProcessingLabel = new JLabel("            Post Processing");
	    JLabel horizonLabel = new JLabel("            Horizon");
	    JLabel genMethodLabel = new JLabel("            Typical Fcst Gen Method");
	    JLabel reservoirModelLabel = new JLabel("            Reservoir Model");
	    JLabel serviceTypeLabel = new JLabel("                   Service Type");
	    JLabel respAgencyLabel = new JLabel("                       Wat Sup Resp Agency");
	    JLabel verifRespTypeLabel = new JLabel("                         Verification Response Type");
	    
	    java.util.List lhvmColumnDescriptorList1 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList2 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList3 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList4 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList5 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList6 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList7 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList8 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList9 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList10 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList11 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList12 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList13 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList14 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList15 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList16 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList17 = new ArrayList();
	    java.util.List lhvmColumnDescriptorList18 = new ArrayList();
	    
	    lhvmLogger = new LhvmLogger();
	    lhvmDataManager = LhvmDataManager.getSingleInstanceOfDataManager(jdbcUrl, lhvmLogger,"-");
    
	    lhvmRowDataList1 = lhvmDataManager.readDataFromFcstType();
	    lhvmColumnDescriptorList1.add(new JTableColumnDescriptor("Fcst Type", true, 150, "center"));
	    jtm1 = new ComplexJTableManager(lhvmColumnDescriptorList1, lhvmRowDataList1);
	    jtm1.setDisplayableColumns(columnsToBeDisplayed1, false, true);
	    jtm1.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane1 = jtm1.getJScrollPane();
	    //jtm1.setTableToolTipText("str");
	    tableScrollPanePanel1 = new JPanel();
		tableScrollPanePanel1.add(tableScrollPane1);
		setDatabaseMessage();
		
	    lhvmRowDataList2 = lhvmDataManager.readDataFromFlowType();
	    lhvmColumnDescriptorList2.add(new JTableColumnDescriptor("Flow Type", true, 150, "center"));
	    jtm2 = new ComplexJTableManager(lhvmColumnDescriptorList2, lhvmRowDataList2);
	    jtm2.setDisplayableColumns(columnsToBeDisplayed2, false, true);
	    jtm2.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane2 = jtm2.getJScrollPane();
	    //jtm2.setTableToolTipText("str");
	    tableScrollPanePanel2 = new JPanel();
		tableScrollPanePanel2.add(tableScrollPane2);
		setDatabaseMessage();
		
	    lhvmRowDataList3 = lhvmDataManager.readDataFromHydrologicMethod();
	    lhvmColumnDescriptorList3.add(new JTableColumnDescriptor("Hydrologic Method", true, 150, "center"));
	    jtm3 = new ComplexJTableManager(lhvmColumnDescriptorList3, lhvmRowDataList3);
	    jtm3.setDisplayableColumns(columnsToBeDisplayed3, false, true);
	    jtm3.setPreferredSizeForJScrollPane(new Dimension(150,125));
	    tableScrollPane3 = jtm3.getJScrollPane();
	    //jtm3.setTableToolTipText("str");
	    tableScrollPanePanel3 = new JPanel();
		tableScrollPanePanel3.add(tableScrollPane3);
		setDatabaseMessage();
		
	    lhvmRowDataList4 = lhvmDataManager.readDataFromIssueCriteria();
	    lhvmColumnDescriptorList4.add(new JTableColumnDescriptor("Def Issue Criteria", true, 150, "center"));
	    jtm4 = new ComplexJTableManager(lhvmColumnDescriptorList4, lhvmRowDataList4);
	    jtm4.setDisplayableColumns(columnsToBeDisplayed4, false, true);
	    jtm4.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane4 = jtm4.getJScrollPane();
	    //jtm4.setTableToolTipText("str");
	    tableScrollPanePanel4 = new JPanel();
		tableScrollPanePanel4.add(tableScrollPane4);
		setDatabaseMessage();
		
	    lhvmRowDataList5 = lhvmDataManager.readDataFromRoutingMethod();
	    lhvmColumnDescriptorList5.add(new JTableColumnDescriptor("Routing Method", true, 150, "center"));
	    jtm5 = new ComplexJTableManager(lhvmColumnDescriptorList5, lhvmRowDataList5);
	    jtm5.setDisplayableColumns(columnsToBeDisplayed5, false, true);
	    jtm5.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane5 = jtm5.getJScrollPane();
	    //jtm5.setTableToolTipText("str");
	    tableScrollPanePanel5 = new JPanel();
		tableScrollPanePanel5.add(tableScrollPane5);
		setDatabaseMessage();
		
	    lhvmRowDataList6 = lhvmDataManager.readDataFromSnowMethod();
	    lhvmColumnDescriptorList6.add(new JTableColumnDescriptor("Snow Method", true, 150, "center"));
	    jtm6 = new ComplexJTableManager(lhvmColumnDescriptorList6, lhvmRowDataList6);
	    jtm6.setDisplayableColumns(columnsToBeDisplayed6, false, true);
	    jtm6.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane6 = jtm6.getJScrollPane();
	    //jtm6.setTableToolTipText("str");
	    tableScrollPanePanel6 = new JPanel();
		tableScrollPanePanel6.add(tableScrollPane6);
		setDatabaseMessage();
		
	    lhvmRowDataList7 = lhvmDataManager.readDataFromWatsupCoordAgency();
	    lhvmColumnDescriptorList7.add(new JTableColumnDescriptor("Wat Sup Coord Agency", true, 150, "center"));
	    jtm7 = new ComplexJTableManager(lhvmColumnDescriptorList7, lhvmRowDataList7);
	    jtm7.setDisplayableColumns(columnsToBeDisplayed7, false, true);
	    jtm7.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane7 = jtm7.getJScrollPane();
	    //jtm7.setTableToolTipText("str");
	    tableScrollPanePanel7 = new JPanel();
		tableScrollPanePanel7.add(tableScrollPane7);
		setDatabaseMessage();
		
	    lhvmRowDataList8 = lhvmDataManager.readDataFromWatsupCriterion();
	    lhvmColumnDescriptorList8.add(new JTableColumnDescriptor("Wat Sup Criterion", true, 150, "center"));
	    jtm8 = new ComplexJTableManager(lhvmColumnDescriptorList8, lhvmRowDataList8);
	    jtm8.setDisplayableColumns(columnsToBeDisplayed8, false, true);
	    jtm8.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane8 = jtm8.getJScrollPane();
	    //jtm8.setTableToolTipText("str");
	    tableScrollPanePanel8 = new JPanel();
		tableScrollPanePanel8.add(tableScrollPane8);
		setDatabaseMessage();
		
	    lhvmRowDataList9 = lhvmDataManager.readDataFromWatsupMethod();
	    lhvmColumnDescriptorList9.add(new JTableColumnDescriptor("Wat Sup Method", true, 150, "center"));
	    jtm9 = new ComplexJTableManager(lhvmColumnDescriptorList9, lhvmRowDataList9);
	    jtm9.setDisplayableColumns(columnsToBeDisplayed9, false, true);
	    jtm9.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane9 = jtm9.getJScrollPane();
	    //jtm9.setTableToolTipText("str");
	    tableScrollPanePanel9 = new JPanel();
		tableScrollPanePanel9.add(tableScrollPane9);
		setDatabaseMessage();
		
	    lhvmRowDataList10 = lhvmDataManager.readDataFromFrequencyUpdate();
	    lhvmColumnDescriptorList10.add(new JTableColumnDescriptor("Frequency Update", true, 150, "center"));
	    jtm10 = new ComplexJTableManager(lhvmColumnDescriptorList10, lhvmRowDataList10);
	    jtm10.setDisplayableColumns(columnsToBeDisplayed10, false, true);
	    jtm10.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane10 = jtm10.getJScrollPane();
	    //jtm10.setTableToolTipText("str");
	    tableScrollPanePanel10 = new JPanel();
		tableScrollPanePanel10.add(tableScrollPane10);
		setDatabaseMessage();
		
	    lhvmRowDataList11 = lhvmDataManager.readDataFromRequiredPeriod();
	    lhvmColumnDescriptorList11.add(new JTableColumnDescriptor("Required Period", true, 150, "center"));
	    jtm11 = new ComplexJTableManager(lhvmColumnDescriptorList11, lhvmRowDataList11);
	    jtm11.setDisplayableColumns(columnsToBeDisplayed11, false, true);
	    jtm11.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane11 = jtm11.getJScrollPane();
	    //jtm11.setTableToolTipText("str");
	    tableScrollPanePanel11 = new JPanel();
		tableScrollPanePanel11.add(tableScrollPane11);
		setDatabaseMessage();
		
	    lhvmRowDataList12 = lhvmDataManager.readDataFromPostProcessing();
	    lhvmColumnDescriptorList12.add(new JTableColumnDescriptor("Post Processing", true, 150, "center"));
	    jtm12 = new ComplexJTableManager(lhvmColumnDescriptorList12, lhvmRowDataList12);
	    jtm12.setDisplayableColumns(columnsToBeDisplayed12, false, true);
	    jtm12.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane12 = jtm12.getJScrollPane();
	    //jtm12.setTableToolTipText("str");
	    tableScrollPanePanel12 = new JPanel();
		tableScrollPanePanel12.add(tableScrollPane12);
		setDatabaseMessage();
		
	    lhvmRowDataList13 = lhvmDataManager.readDataFromHorizon();
	    lhvmColumnDescriptorList13.add(new JTableColumnDescriptor("Horizon", true, 150, "center"));
	    jtm13 = new ComplexJTableManager(lhvmColumnDescriptorList13, lhvmRowDataList13);
	    jtm13.setDisplayableColumns(columnsToBeDisplayed13, false, true);
	    jtm13.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane13 = jtm13.getJScrollPane();
	    //jtm13.setTableToolTipText("str");
	    tableScrollPanePanel13 = new JPanel();
		tableScrollPanePanel13.add(tableScrollPane13);
		setDatabaseMessage();
		
	    lhvmRowDataList14 = lhvmDataManager.readDataFromGenMethod();
	    lhvmColumnDescriptorList14.add(new JTableColumnDescriptor("Typical Fcst Gen Method", true, 150, "center"));
	    jtm14 = new ComplexJTableManager(lhvmColumnDescriptorList14, lhvmRowDataList14);
	    jtm14.setDisplayableColumns(columnsToBeDisplayed14, false, true);
	    jtm14.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane14 = jtm14.getJScrollPane();
	    //jtm14.setTableToolTipText("str");
	    tableScrollPanePanel14 = new JPanel();
		tableScrollPanePanel14.add(tableScrollPane14);
		setDatabaseMessage();
		
	    lhvmRowDataList15 = lhvmDataManager.readDataFromReservoirModel();
	    lhvmColumnDescriptorList15.add(new JTableColumnDescriptor("Reservoir Model", true, 150, "center"));
	    jtm15 = new ComplexJTableManager(lhvmColumnDescriptorList15, lhvmRowDataList15);
	    jtm15.setDisplayableColumns(columnsToBeDisplayed15, false, true);
	    jtm15.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane15 = jtm15.getJScrollPane();
	    //jtm15.setTableToolTipText("str");
	    tableScrollPanePanel15 = new JPanel();
		tableScrollPanePanel15.add(tableScrollPane15);
		setDatabaseMessage();
		
	    lhvmRowDataList16 = lhvmDataManager.readDataFromRespAgency();
	    lhvmColumnDescriptorList16.add(new JTableColumnDescriptor("Wat Sup Resp Agency", true, 150, "center"));
	    jtm16 = new ComplexJTableManager(lhvmColumnDescriptorList16, lhvmRowDataList16);
	    jtm16.setDisplayableColumns(columnsToBeDisplayed16, false, true);
	    jtm16.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane16 = jtm16.getJScrollPane();
	    //jtm16.setTableToolTipText("str");
	    tableScrollPanePanel16 = new JPanel();
		tableScrollPanePanel16.add(tableScrollPane16);
		setDatabaseMessage();
		
	    lhvmRowDataList17 = lhvmDataManager.readDataFromServiceType();
	    lhvmColumnDescriptorList17.add(new JTableColumnDescriptor("Service Type", true, 150, "center"));
	    jtm17 = new ComplexJTableManager(lhvmColumnDescriptorList17, lhvmRowDataList17);
	    jtm17.setDisplayableColumns(columnsToBeDisplayed17, false, true);
	    jtm17.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane17 = jtm17.getJScrollPane();
	    //jtm17.setTableToolTipText("str");
	    tableScrollPanePanel17 = new JPanel();
		tableScrollPanePanel17.add(tableScrollPane17);
		setDatabaseMessage();
		
	    lhvmRowDataList18 = lhvmDataManager.readDataFromVerificationResponseType();
	    lhvmColumnDescriptorList18.add(new JTableColumnDescriptor("Verification Response Type", true, 150, "center"));
	    jtm18 = new ComplexJTableManager(lhvmColumnDescriptorList18, lhvmRowDataList18);
	    jtm18.setDisplayableColumns(columnsToBeDisplayed18, false, true);
	    jtm18.setPreferredSizeForJScrollPane(new Dimension(150, 125));
	    tableScrollPane18 = jtm18.getJScrollPane();
	    //jtm17.setTableToolTipText("str");
	    tableScrollPanePanel18 = new JPanel();
		tableScrollPanePanel18.add(tableScrollPane18);
		setDatabaseMessage();
		   
	    consoleTextArea.insert(new String(LhvmDataManager.displayedMessageReferencesWindow), 0);
	    JScrollPane scrollableTextAreaForConsole = new JScrollPane(consoleTextArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	
	    int numRows1 = lhvmRowDataList1.size();
	    JLabel numRowsLabel1 = new JLabel("            " + numRows1 + " Rows");
	    int numRows2 = lhvmRowDataList2.size();
	    JLabel numRowsLabel2 = new JLabel("            " + numRows2 + " Rows");
	    int numRows3 = lhvmRowDataList3.size();
	    JLabel numRowsLabel3 = new JLabel("            " + numRows3 + " Rows");
	    int numRows4 = lhvmRowDataList4.size();
	    JLabel numRowsLabel4 = new JLabel("            " + numRows4 + " Rows");
	    int numRows5 = lhvmRowDataList5.size();
	    JLabel numRowsLabel5 = new JLabel("            " + numRows5 + " Rows");
	    int numRows6 = lhvmRowDataList6.size();
	    JLabel numRowsLabel6 = new JLabel("            " + numRows6 + " Rows");
	    int numRows7 = lhvmRowDataList7.size();
	    JLabel numRowsLabel7 = new JLabel("              " + numRows7 + " Rows");
	    int numRows8 = lhvmRowDataList8.size();
	    JLabel numRowsLabel8 = new JLabel("            " + numRows8 + " Rows");
	    int numRows9 = lhvmRowDataList9.size();
	    JLabel numRowsLabel9 = new JLabel("            " + numRows9 + " Rows");
	    int numRows10 = lhvmRowDataList10.size();
	    JLabel numRowsLabel10 = new JLabel("            " + numRows10 + " Rows");
	    int numRows11 = lhvmRowDataList11.size();
	    JLabel numRowsLabel11 = new JLabel("            " + numRows11 + " Rows");
	    int numRows12 = lhvmRowDataList12.size();
	    JLabel numRowsLabel12 = new JLabel("            " + numRows12 + " Rows");
	    int numRows13 = lhvmRowDataList13.size();
	    JLabel numRowsLabel13 = new JLabel("            " + numRows13 + " Rows");
	    int numRows14 = lhvmRowDataList14.size();
	    JLabel numRowsLabel14 = new JLabel("            " + numRows14 + " Rows");
	    int numRows15 = lhvmRowDataList15.size();
	    JLabel numRowsLabel15 = new JLabel("            " + numRows15 + " Rows");
	    int numRows16 = lhvmRowDataList16.size();
	    JLabel numRowsLabel16 = new JLabel("                       " + numRows16 + " Rows");
	    int numRows17 = lhvmRowDataList17.size(); 
	    JLabel numRowsLabel17 = new JLabel("                      " + numRows17 + " Rows");
	    int numRows18 = lhvmRowDataList18.size();
	    JLabel numRowsLabel18 = new JLabel("                         " + numRows18 + " Rows");
	    
	    JFrame tablesPanel = new JFrame();
	    tablesPanel.setLayout(new SpringLayout());
	    tablesPanel.add(numRowsLabel1);
	    tablesPanel.add(numRowsLabel2);
	    tablesPanel.add(numRowsLabel3);
	    tablesPanel.add(numRowsLabel4);	
	    tablesPanel.add(numRowsLabel5);
	    tablesPanel.add(fcstTypeLabel);
	    tablesPanel.add(flowTypeLabel);
	    tablesPanel.add(hydrologicMethodLabel);
	    tablesPanel.add(issueCriteriaLabel);
        tablesPanel.add(routingMethodLabel);
	    tablesPanel.add(tableScrollPanePanel1);
	    tablesPanel.add(tableScrollPanePanel2);
	    tablesPanel.add(tableScrollPanePanel3);
	    tablesPanel.add(tableScrollPanePanel4);
	    tablesPanel.add(tableScrollPanePanel5);
	    SpringUtilities.makeCompactGrid(tablesPanel.getContentPane(), 3, 5, 0, 0, 0, 0);
	    
	    JFrame tablesPanel1 = new JFrame();
	    tablesPanel1.setLayout(new SpringLayout());	    
	    tablesPanel1.add(numRowsLabel6);
	    tablesPanel1.add(numRowsLabel7);
	    tablesPanel1.add(numRowsLabel8);
	    tablesPanel1.add(numRowsLabel9);
	    tablesPanel1.add(numRowsLabel10);
	    tablesPanel1.add(snowMethodLabel);
	    tablesPanel1.add(watsupCoordAgencyLabel);
	    tablesPanel1.add(watsupCriterionLabel);
	    tablesPanel1.add(watsupMethodLabel);
        tablesPanel1.add(frequencyUpdateLabel);
	    tablesPanel1.add(tableScrollPanePanel6);
	    tablesPanel1.add(tableScrollPanePanel7);
	    tablesPanel1.add(tableScrollPanePanel8);
	    tablesPanel1.add(tableScrollPanePanel9);
	    tablesPanel1.add(tableScrollPanePanel10);
	    SpringUtilities.makeCompactGrid(tablesPanel1.getContentPane(), 3, 5, 0, 0, 0, 0);
	    
	    JFrame tablesPanel2 = new JFrame();
	    tablesPanel2.setLayout(new SpringLayout());	    
	    tablesPanel2.add(numRowsLabel11);
	    tablesPanel2.add(numRowsLabel12);
	    tablesPanel2.add(numRowsLabel13);
	    tablesPanel2.add(numRowsLabel14);
	    tablesPanel2.add(numRowsLabel15);
        tablesPanel2.add(requiredPeriodLabel);
	    tablesPanel2.add(postProcessingLabel);
	    tablesPanel2.add(horizonLabel);
	    tablesPanel2.add(genMethodLabel);
	    tablesPanel2.add(reservoirModelLabel);
	    tablesPanel2.add(tableScrollPanePanel11);
	    tablesPanel2.add(tableScrollPanePanel12);
	    tablesPanel2.add(tableScrollPanePanel13);
	    tablesPanel2.add(tableScrollPanePanel14);
	    tablesPanel2.add(tableScrollPanePanel15);
	    SpringUtilities.makeCompactGrid(tablesPanel2.getContentPane(), 3, 5, 0, 0, 0, 0);
	    
	    JFrame tablesPanel3 = new JFrame();
	    tablesPanel3.setLayout(new SpringLayout());	    	    
	    tablesPanel3.add(numRowsLabel16);
	    tablesPanel3.add(numRowsLabel17);
	    tablesPanel3.add(numRowsLabel18);
        tablesPanel3.add(respAgencyLabel);
        tablesPanel3.add(serviceTypeLabel);
	    tablesPanel3.add(verifRespTypeLabel);
	    tablesPanel3.add(tableScrollPanePanel16);	    
	    tablesPanel3.add(tableScrollPanePanel17);
	    tablesPanel3.add(tableScrollPanePanel18);	    
	    SpringUtilities.makeCompactGrid(tablesPanel3.getContentPane(), 3, 3, 0, 0, 0, 0);
	    
	    lhvm.getContentPane().add(tablesPanel.getContentPane());
	    lhvm.getContentPane ().add(separatorLabel1);
	    lhvm.getContentPane().add(tablesPanel1.getContentPane());
	    lhvm.getContentPane ().add(separatorLabel2);
	    lhvm.getContentPane().add(tablesPanel2.getContentPane());
	    lhvm.getContentPane ().add(separatorLabel3);
	    lhvm.getContentPane().add(tablesPanel3.getContentPane());
	    lhvm.getContentPane ().add(closeButton);
	    lhvm.getContentPane().add(consoleLabel);
	    lhvm.getContentPane().add(scrollableTextAreaForConsole);
	    SpringUtilities.makeCompactGrid(lhvm.getContentPane(), 10, 1, 0, 0, 6, 6);
	    
	    lhvm.addWindowListener(new ServicesDbWindowAdapter());
	    
	    lhvm.pack();
	    //Show the JFrame
	    lhvm.setVisible (true);
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
	
    private class CloseActionListener implements ActionListener
    {	
    	public void actionPerformed(ActionEvent e) 
        {
    		operationCloseWindow();
        }
    }
}
