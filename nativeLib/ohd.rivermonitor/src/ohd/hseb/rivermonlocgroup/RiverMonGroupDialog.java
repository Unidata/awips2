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
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.InputVerifier;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import ohd.hseb.db.Database;
import ohd.hseb.ihfsdb.generated.RiverMonGroupRecord;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.ComponentHelper;
import ohd.hseb.util.gui.FrameCloseWindowListener;
import ohd.hseb.util.gui.WindowResizingManager;
import ohd.hseb.util.gui.jtable.ComplexJTableManager;
import ohd.hseb.util.gui.jtable.JTableManager;

public class RiverMonGroupDialog extends JDialog
{
	private List _riverMonGroupColumnDescriptorList = null;
	private JTableManager _riverMonGroupTableManager = null;
	private RiverMonLocGroupDataManager _riverMonLocGroupDataMgr = null;
	private List _allRowDataList = null;
	private JScrollPane _tableScrollPane = null;
	private int _selectedRowIndex;
	private boolean _riverMonGroupTableChanged = false;
	private String _defaultHsa = null;

	private JLabel _groupIdLabel;
	private JTextField _groupIdField ;
	private JLabel _groupNameLabel;
	private JTextField _groupNameField ;
	private JLabel _groupOrdinalLabel;
	private JTextField _groupOrdinalField ;
	private JLabel _hsaLabel;
	private JTextField _hsaField;
	private JButton _updateButton;
	private JButton _deleteButton ;
	private JButton _closeButton ;
	
	private boolean _infoDialogShown = false;
	private SessionLogger _logger = null;
	
	private RiverMonGroupJTableRowData _selectedRowData;
	private TextFieldInputVerifier _textFieldInputVerifier = new TextFieldInputVerifier();
	
	public RiverMonGroupDialog(JFrame mainFrame, RiverMonLocGroupDataManager riverMonLocGroupDataMgr, SessionLogger logger, String defaultHsa)
	{
		super(mainFrame, true);
		_logger = logger;
		_defaultHsa = defaultHsa;
		this.setTitle("RiverMonGroup");
		this.getContentPane().setLayout(new GridBagLayout());
		_riverMonLocGroupDataMgr = riverMonLocGroupDataMgr;
		initialize(riverMonLocGroupDataMgr);
		Dimension min = new Dimension(600,570);
		Dimension max = new Dimension(700,570);
		new WindowResizingManager(this, min, max);
		this.pack();
        this.setLocation(25, 25);
	}
	
	public boolean showRiverMonGroupDialog()
	{
	    _riverMonGroupTableChanged = false;
        _allRowDataList = _riverMonLocGroupDataMgr.readDataFromRiverMonGroup();
        _riverMonGroupTableManager.setChangedAllRowDataList(_allRowDataList);
        _riverMonGroupTableManager.refreshDisplay();
        _selectedRowData = null;
        showCurrentRowData();
		this.setVisible(true);
		return _riverMonGroupTableChanged;
	}
	
	private void initialize(RiverMonLocGroupDataManager riverMonLocGroupDataMgr)
	{
	    _riverMonGroupColumnDescriptorList = new RiverMonGroupColumns().getRiverMonGroupColumnsList(_riverMonGroupColumnDescriptorList);
	     
		_allRowDataList = riverMonLocGroupDataMgr.readDataFromRiverMonGroup();
		System.out.println("All row data list:"+ _allRowDataList.size());
		System.out.println("Column list:"+ _riverMonGroupColumnDescriptorList.size());
		
		_riverMonGroupTableManager = new ComplexJTableManager(_riverMonGroupColumnDescriptorList, _allRowDataList);
		String columnsSelected[] = _riverMonGroupTableManager.getColumnNamesThatAreCurrentlyDisplayed();
		_riverMonGroupTableManager.setDisplayableColumns(columnsSelected, true, true);
		_riverMonGroupTableManager.setPreferredSizeForJScrollPane(new Dimension(590, 300));
		_riverMonGroupTableManager.setVerifyInputWhenFocusTarget(false);
		RiverMonitorGroupTableMouseListener tableMouseListener = new RiverMonitorGroupTableMouseListener();
		_riverMonGroupTableManager.addTableListener(tableMouseListener);
		_tableScrollPane = _riverMonGroupTableManager.getJScrollPane();
		
		JPanel tableScrollPanePanel = new JPanel();
		tableScrollPanePanel.add(_tableScrollPane);
		tableScrollPanePanel.setMinimumSize(new Dimension(600,310));
		JPanel tablePanel = new JPanel();
		tablePanel.add(tableScrollPanePanel);
		tablePanel.setMinimumSize(new Dimension(600,310));
		
		_groupIdLabel = new JLabel("Group Id");
		_groupIdField = new JTextField();
		_groupIdField.setInputVerifier(_textFieldInputVerifier);
		
		_groupNameLabel = new JLabel("Group Name");
		_groupNameField = new JTextField();
		_groupNameField.setInputVerifier(_textFieldInputVerifier);
		
		_groupOrdinalLabel = new JLabel("Group Ordinal");
		_groupOrdinalField = new JTextField();
		_groupOrdinalField.setInputVerifier(_textFieldInputVerifier);
		
		_hsaLabel = new JLabel("HSA");
		_hsaField = new JTextField();
		_hsaField.setEditable(false);
		
		JPanel infoPanel = new JPanel();
		infoPanel.setLayout(new GridLayout(4,4));
		JPanel dummyPanel1 = createDummyPanel();
		infoPanel.add(dummyPanel1);
		infoPanel.add(_groupIdLabel);
		infoPanel.add(_groupIdField);
		JPanel dummyPanel2 = createDummyPanel();
		infoPanel.add(dummyPanel2);
		
		JPanel dummyPanel3 = createDummyPanel();
		infoPanel.add(dummyPanel3);
		infoPanel.add(_groupNameLabel);
		infoPanel.add(_groupNameField);
		JPanel dummyPanel4 = createDummyPanel();
		infoPanel.add(dummyPanel4);
		
		JPanel dummyPanel5 = createDummyPanel();
		infoPanel.add(dummyPanel5);
		infoPanel.add(_groupOrdinalLabel);
		infoPanel.add(_groupOrdinalField);
		JPanel dummyPanel6 = createDummyPanel();
		infoPanel.add(dummyPanel6);
		
		JPanel dummyPanel7 = createDummyPanel();
		infoPanel.add(dummyPanel7);
		infoPanel.add(_hsaLabel);
		infoPanel.add(_hsaField);
		JPanel dummyPanel8 = createDummyPanel();
		infoPanel.add(dummyPanel8);
		
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
	
	private JPanel createDummyPanel()
	{
		JPanel dummyPanel = new JPanel();
		return dummyPanel;
	}
	
	private void closeRiverMonGroupDialog()
	{
		this.setVisible(false);
	}
	
    private void showCurrentRowData()
	{
		if(_selectedRowData != null)
		{
			_groupIdField.setText(_selectedRowData.getGroupId());
			_groupNameField.setText(_selectedRowData.getGroupName());
			_groupOrdinalField.setText(new Integer(_selectedRowData.getGroupOrdinal()).toString());
			_hsaField.setText(_selectedRowData.getHsa());
			_selectedRowData = null;
		}
		else
		{
			_groupIdField.setText("");
			_groupNameField.setText("");
			_groupOrdinalField.setText("");
			_hsaField.setText("");
		}
	}
	
	private RiverMonGroupRecord getRiverMonGroupRecord()
	{
		RiverMonGroupRecord record = new RiverMonGroupRecord();
		if(!(_groupIdField.getInputVerifier().verify(_groupIdField)))
		{
			JOptionPane.showMessageDialog(null, "Group Id length must be >=1 and <= 8","RiverMonGroup Application", JOptionPane.PLAIN_MESSAGE);
			_infoDialogShown = true;
			return null;
		}
		else
		{
			record.setGroup_id(_groupIdField.getText().toUpperCase());
		}
		if(!(_groupNameField.getInputVerifier().verify(_groupNameField)))
		{
			JOptionPane.showMessageDialog(null, "Group Name length must be >=1 and <= 32","RiverMonGroup Application", JOptionPane.PLAIN_MESSAGE);
			_infoDialogShown = true;
			return null;
		}
		else
		{
			record.setGroup_name(_groupNameField.getText());
		}
		if(!(_groupOrdinalField.getInputVerifier().verify(_groupOrdinalField)))
		{
		    JOptionPane.showMessageDialog(null, "Ordinal Number must be a numeric value","RiverMonGroup Application", JOptionPane.PLAIN_MESSAGE);
			_infoDialogShown = true;
			return null;
		}
		else
		{
    		record.setOrdinal(Integer.parseInt(_groupOrdinalField.getText()));
		}
		if(_hsaField.getText().length() > 0)
		{
		  record.setHsa(_hsaField.getText().toUpperCase());
		}
		else
		{
			_hsaField.setText(_defaultHsa);
			record.setHsa(_defaultHsa);
		}

		return record;
	}
	
	private void getCurrentDataFromDataMgr()
	{
		List dbInfoList = null;
		String header = "RiverMonGroupDialog.getCurrentDataFromDataMgr() ";
		try
		{
			dbInfoList = _riverMonLocGroupDataMgr.readDataFromRiverMonGroup();
		}
		catch(Exception e)
		{
			_logger.log(header+ e);
		}
		_allRowDataList = dbInfoList;
		_riverMonGroupTableManager.setChangedAllRowDataList(_allRowDataList);
	}
	
	private class TextFieldInputVerifier extends InputVerifier
	{
		public boolean verify(JComponent component) 
		{
			boolean returnValue = true;
			JTextField textField = (JTextField)component;
			if(component == _groupIdField)
			{
				String id= textField.getText();
				if(id != null)
				{
					if(id.length() == 0)
						returnValue = false;
					else if(id.length() > 8)
						returnValue = false;
				}
				else
					returnValue = false;
			}
			else if(component == _groupNameField)
			{
				String name = textField.getText();
				if(name != null)
				{
					if(name.length() == 0)
						returnValue = false;
					else if(name.length() > 32)
						returnValue = false;
				}
				else
					returnValue = false;
			}
			else if(component == _groupOrdinalField)
			{
				try 
				{
					Integer.parseInt(textField.getText());
					returnValue = true;
				} 
				catch (NumberFormatException e) 
				{
					returnValue = false;
				}
			}
			if(!returnValue)
			{
				if(component ==_groupIdField)
			      JOptionPane.showMessageDialog(null, "Group Id length must be >=1 and <= 8","RiverMonGroup Application", JOptionPane.PLAIN_MESSAGE);
				else if(component == _groupNameField)
					JOptionPane.showMessageDialog(null, "Group Name length must be >=1 and <= 32","RiverMonGroup Application", JOptionPane.PLAIN_MESSAGE);
				else if(component == _groupOrdinalField)
					JOptionPane.showMessageDialog(null, "Ordinal Number must be a numeric value","RiverMonGroup Application", JOptionPane.PLAIN_MESSAGE);
			}
			return returnValue;
		}
	}
	
	private class CloseListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			closeRiverMonGroupDialog();            
		}
	}
	
	private class DeleteListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			String header = "RiverMonitorGroupDialog.DeleteListener.actionPerformed(): ";
			RiverMonGroupRecord record = getRiverMonGroupRecord();
			if(!_infoDialogShown)
			{
				_logger.log(header+" Delete record"+ record.toString());
				_riverMonLocGroupDataMgr.delete(record);
				getCurrentDataFromDataMgr();
				_riverMonGroupTableManager.refreshDisplay();
				_selectedRowData = null;
				showCurrentRowData();
				_riverMonGroupTableChanged = true;
			}
		}
	}
	
	private class UpdateListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			String header = "RiverMonGroupDialog.UpdateListener.actionPerformed(): ";
			RiverMonGroupRecord record = getRiverMonGroupRecord();
			boolean recordExists = _riverMonLocGroupDataMgr.checkIfRecordExists(record);
			if(!recordExists)
			{
				record.setHsa(_defaultHsa);
				_hsaField.setText(_defaultHsa);
			}
			if(!_infoDialogShown)
			{
				_logger.log(header+" Update record"+ record.toString());
				_riverMonLocGroupDataMgr.updateOrInsert(record);
				getCurrentDataFromDataMgr();
				_riverMonGroupTableManager.refreshDisplay();
				int rowToHighLight = _riverMonGroupTableManager.getTheRowIndexToHighLightBasedOnValue(_groupIdField.getText());
				_riverMonGroupTableManager.selectRows(rowToHighLight, rowToHighLight);
				_riverMonGroupTableChanged = true;
			}
		}
	}
	
	private class RiverMonitorGroupTableMouseListener implements MouseListener
	{
		public void mouseClicked(MouseEvent e) 
		{
			Point p = new Point(e.getX(), e.getY());
			_selectedRowIndex = _riverMonGroupTableManager.getMousePointedRowIndex(p);
			_selectedRowData = (RiverMonGroupJTableRowData) _riverMonGroupTableManager.getSelectedRowData(_selectedRowIndex);
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
	    SessionLogger logger = new SessionLogger("RiverMonGroup", baseLogFilePath, true, true,  messageId);

	    RiverMonLocGroupDataManager dataMgr = new RiverMonLocGroupDataManager(database, logger, "", defaultHsa );

	    JFrame frame = new JFrame();

	    FrameCloseWindowListener.addFrameCloseWindowListener(frame);

	    RiverMonGroupDialog dialog = new RiverMonGroupDialog(frame, dataMgr, logger, defaultHsa);
	    dialog.setVisible(true);
	    frame.setVisible(true);

	}

}
