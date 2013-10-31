package ohd.hseb.rivermonlocgroup;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.InputVerifier;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import ohd.hseb.db.Database;
import ohd.hseb.ihfsdb.generated.RiverMonLocationRecord;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.FrameCloseWindowListener;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableManager;

public class RiverMonLocationDialog extends JDialog
{
	private List _riverMonLocationColumnDescriptorList = null;
	private JTableManager _riverMonLocationTableManager = null;
	private RiverMonLocGroupDataManager _riverMonLocGroupDataMgr = null;
	private List _allRowDataList = null;
	private JScrollPane _tableScrollPane = null;
	private int _selectedRowIndex;
	private boolean _riverMonLocationTableChanged = false;
	
	private JLabel _lidLabel;
	private JComboBox _lidComboBox ;
	private JLabel _lidOrdinalLabel;
	private JTextField _lidOrdinalField ;
	private JLabel _groupIdLabel;
	private JComboBox _groupIdComboBox;
	private JButton _updateButton;
	private JButton _deleteButton ;
	private JButton _closeButton ;
	private String _groupIdArray[];
	private String _lidArray[];
	
	private boolean _infoDialogShown = false;
	private SessionLogger _logger = null;
	
	private RiverMonLocationJTableRowData _selectedRowData;
	private TextFieldInputVerifier _textFieldInputVerifier = new TextFieldInputVerifier();
	
	public RiverMonLocationDialog(JFrame mainFrame, RiverMonLocGroupDataManager riverMonLocGroupDataMgr, SessionLogger logger)
	{
		super(mainFrame, true);
		_logger = logger;
		this.setTitle("RiverMonLocation");
		this.getContentPane().setLayout(new GridBagLayout());
		_riverMonLocGroupDataMgr = riverMonLocGroupDataMgr;
		_groupIdArray = _riverMonLocGroupDataMgr.getAllGroup();
		discardDefaultGroupInGroupIdArray();
		_lidArray = _riverMonLocGroupDataMgr.getAllLid();
		initialize(riverMonLocGroupDataMgr);
		Dimension min = new Dimension(300,570);
		Dimension max = new Dimension(350,570);
		new WindowResizingManager(this, min, max);
		this.pack();
        this.setLocation(25, 25);
	}
	
	public boolean showRiverMonLocationDialog()
	{
	    _riverMonLocationTableChanged = false;
        _allRowDataList = _riverMonLocGroupDataMgr.readDataFromRiverMonLocation();
        _riverMonLocationTableManager.setChangedAllRowDataList(_allRowDataList);
        _riverMonLocationTableManager.refreshDisplay();

		_groupIdArray = _riverMonLocGroupDataMgr.getAllGroup();
		_groupIdComboBox.removeAllItems();
		discardDefaultGroupInGroupIdArray();
		for(int i=0; i <_groupIdArray.length; i++)
		{
			_groupIdComboBox.addItem(_groupIdArray[i]);
		}

		_lidArray = _riverMonLocGroupDataMgr.getAllLid();
		_lidComboBox.removeAllItems();
		for(int i=0; i <_lidArray.length; i++)
		{
			_lidComboBox.addItem(_lidArray[i]);
		}
		this.setVisible(true);
		return(_riverMonLocationTableChanged);
	}
	
	private void discardDefaultGroupInGroupIdArray()
	{
	  List groupIdList = new ArrayList(); 
	  for(int i=0; i < _groupIdArray.length; i++)
	  {
		  if(_groupIdArray[i].equalsIgnoreCase("default"))
			  continue;
		  else
			  groupIdList.add(_groupIdArray[i]);
	  }
	  String[] groupIdArray = new String[groupIdList.size()];
	  for(int i=0; i < groupIdList.size(); i++)
	  {
		  groupIdArray[i] = (String) groupIdList.get(i);
	  }
	  _groupIdArray = groupIdArray;
	}
	
	private void initialize(RiverMonLocGroupDataManager riverMonLocGroupDataMgr)
	{
	    _riverMonLocationColumnDescriptorList = new RiverMonLocationColumns().getRiverMonLocationColumnsList(_riverMonLocationColumnDescriptorList);
	    
		_allRowDataList = riverMonLocGroupDataMgr.readDataFromRiverMonLocation();
		_riverMonLocationTableManager = new ComplexJTableManager(_riverMonLocationColumnDescriptorList, _allRowDataList);
		String columnsSelected[] = _riverMonLocationTableManager.getColumnNamesThatAreCurrentlyDisplayed();
		_riverMonLocationTableManager.setDisplayableColumns(columnsSelected, true, true);
		_riverMonLocationTableManager.setPreferredSizeForJScrollPane(new Dimension(315, 300));
		_riverMonLocationTableManager.setVerifyInputWhenFocusTarget(false);
		RiverMonitorLocationTableMouseListener tableMouseListener = new RiverMonitorLocationTableMouseListener();
		_riverMonLocationTableManager.addTableListener(tableMouseListener);
		_tableScrollPane = _riverMonLocationTableManager.getJScrollPane();
	
		JPanel tableScrollPanePanel = new JPanel();
		tableScrollPanePanel.add(_tableScrollPane);
		tableScrollPanePanel.setMinimumSize(new Dimension(315,310));
		JPanel tablePanel = new JPanel();
		tablePanel.add(tableScrollPanePanel);
		tablePanel.setMinimumSize(new Dimension(315,310));
		
		_lidLabel = new JLabel("Location");
		_lidComboBox = new JComboBox(_lidArray);
		
		_lidOrdinalLabel = new JLabel("Location Ordinal");
		_lidOrdinalField = new JTextField();
		_lidOrdinalField.setInputVerifier(_textFieldInputVerifier);
		
		_groupIdLabel = new JLabel("Group Id");
		_groupIdComboBox = new JComboBox(_groupIdArray);
		
		JPanel infoPanel = new JPanel();
		infoPanel.setLayout(new GridLayout(3,2));
		infoPanel.add(_lidLabel);
		infoPanel.add(_lidComboBox);
		
		infoPanel.add(_groupIdLabel);
		infoPanel.add(_groupIdComboBox);
		
		infoPanel.add(_lidOrdinalLabel);
		infoPanel.add(_lidOrdinalField);
		
		JPanel actionPanel = new JPanel();
		actionPanel = new JPanel();
		_updateButton = new JButton("Save");
		_deleteButton = new JButton("Delete");
		_closeButton = new JButton("Close");
		_closeButton.setVerifyInputWhenFocusTarget(false);
		actionPanel.add(_updateButton);
		actionPanel.add(_deleteButton);
		actionPanel.add(_closeButton);
		
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new GridLayout(1,2));
		editPanel.setBorder(BorderFactory.createTitledBorder("Details")); 
		
		editPanel.add(infoPanel);
		
		Container contentPane = this.getContentPane();
		ComponentHelper.addFrameComponent(contentPane, tablePanel, 0, 0,1,1,1, 1, 1);
		ComponentHelper.addFrameComponent(contentPane, editPanel, 0, 1,1,1, 1, 1, 1);
		ComponentHelper.addFrameComponent(contentPane, actionPanel, 0, 2,1,1, 1, 1, 0);
		
		UpdateListener updateListener = new UpdateListener();
		_updateButton.addActionListener(updateListener);
		
		DeleteListener deleteListener = new DeleteListener();
		_deleteButton.addActionListener(deleteListener);
		
		CloseListener closeListener = new CloseListener();
		_closeButton.addActionListener(closeListener);
		
	}
	
	private void closeRiverMonLocationDialog()
	{
		this.setVisible(false);
	}
	
    private void showCurrentRowData()
	{
		if(_selectedRowData != null)
		{
			_groupIdComboBox.setSelectedItem(_selectedRowData.getGroupId());
			_lidComboBox.setSelectedItem(_selectedRowData.getLid());
			_lidOrdinalField.setText(new Integer(_selectedRowData.getLocationOrdinal()).toString());
		}
	}
	
	private RiverMonLocationRecord getRiverMonLocationRecord()
	{
		RiverMonLocationRecord record = new RiverMonLocationRecord();
		
		record.setLid(_lidComboBox.getSelectedItem().toString());
		
		if(!(_lidOrdinalField.getInputVerifier().verify(_lidOrdinalField)))
		{
			JOptionPane.showMessageDialog(null, "Ordinal Number must be a numeric value","RiverMonGroup Application", JOptionPane.PLAIN_MESSAGE);
			_infoDialogShown = true;
			return null;
		}
		else
		{
			record.setOrdinal(Integer.parseInt(_lidOrdinalField.getText()));
		}
		record.setGroup_id(_groupIdComboBox.getSelectedItem().toString());
		
		return record;
	}
	
	private void getCurrentDataFromDataMgr()
	{
		List dbInfoList = null;
		String header = "RiverMonLocationDialog.getCurrentDataFromDataMgr() ";
		try
		{
			dbInfoList = _riverMonLocGroupDataMgr.readDataFromRiverMonLocation();
		}
		catch(Exception e)
		{
			_logger.log(header+ e);
		}
		_allRowDataList = dbInfoList;
		_riverMonLocationTableManager.setChangedAllRowDataList(_allRowDataList);
	}
    
    private void delete()
    {
        String header = "RiverMonLocationDialog.delete(): ";
        
        RiverMonLocationRecord record = getRiverMonLocationRecord();

        _logger.log(header+" Delete record"+ record.toString());
        _riverMonLocGroupDataMgr.delete(record);
        getCurrentDataFromDataMgr();
        _riverMonLocationTableManager.refreshDisplay();
        _selectedRowData = null;
        showCurrentRowData();
        _riverMonLocationTableChanged = true;

    }

    private void insertOrUpdate()
    {
        String header = "RiverMonLocationDialog.insertOrUpdate(): ";
        
        RiverMonLocationRecord record = getRiverMonLocationRecord();

        _logger.log(header+" Update record"+ record.toString());
        _riverMonLocGroupDataMgr.updateOrInsert(record);
        getCurrentDataFromDataMgr();
        _riverMonLocationTableManager.refreshDisplay();
        int rowToHighLight = _riverMonLocationTableManager.getTheRowIndexToHighLightBasedOnValue(_lidComboBox.getSelectedItem().toString());
        _riverMonLocationTableManager.selectRows(rowToHighLight, rowToHighLight);
        _riverMonLocationTableChanged = true;

    }
    
	private class TextFieldInputVerifier extends InputVerifier
	{
		public boolean verify(JComponent component) 
		{
			boolean returnValue = true;
			JTextField textField = (JTextField)component;
			try 
			{
				Integer.parseInt(textField.getText());
				returnValue = true;
			} 
			catch (NumberFormatException e) 
			{
				returnValue = false;
			}
			if(!returnValue)
			{
				JOptionPane.showMessageDialog(null, "Ordinal Number must be a numeric value","RiverMonGroup Application", JOptionPane.PLAIN_MESSAGE);
			}
			return returnValue;
		}
	}
	
	private class CloseListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			closeRiverMonLocationDialog();            
		}
	}
	
	private class DeleteListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			String header = "RiverMonitorLocationDialog.DeleteListener.actionPerformed(): ";
			if(!_infoDialogShown)
			{           
				delete();
			}
		}
	}
	
	private class UpdateListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			String header = "RiverMonLocationDialog.UpdateListener.actionPerformed(): ";
			if(!_infoDialogShown)
			{            
                insertOrUpdate();	
			}
		}
	}
	
	private class RiverMonitorLocationTableMouseListener implements MouseListener
	{
		public void mouseClicked(MouseEvent e) 
		{
			Point p = new Point(e.getX(), e.getY());
			_selectedRowIndex = _riverMonLocationTableManager.getMousePointedRowIndex(p);
			_selectedRowData = (RiverMonLocationJTableRowData) _riverMonLocationTableManager.getSelectedRowData(_selectedRowIndex);
			showCurrentRowData();
		}
		public void mousePressed(MouseEvent e){}
		public void mouseReleased(MouseEvent e){}
		public void mouseEntered(MouseEvent e){}
		public void mouseExited(MouseEvent e){}
	}
    
    public static void main(String[] args)
    {
        /*
         * This main is for testing purposes
         */
        
        String jdbcUrl = args[0];
        String baseLogFilePath = args[1];
        String defaultHsa = args[2];
        
        System.out.println("jdbcUrl = " + jdbcUrl );
        
        Database database = new Database(jdbcUrl);
        
        String messageId = "M1";
        SessionLogger logger = new SessionLogger("RiverMonLocation", baseLogFilePath, true, true,  messageId);
        
        RiverMonLocGroupDataManager dataMgr = new RiverMonLocGroupDataManager(database, logger, "", defaultHsa );
        
        JFrame frame = new JFrame();
        
        FrameCloseWindowListener.addFrameCloseWindowListener(frame);
        
        RiverMonLocationDialog dialog = new RiverMonLocationDialog(frame, dataMgr, logger);
        dialog.setVisible(true);
        frame.setVisible(true);
        
    }
	
}
